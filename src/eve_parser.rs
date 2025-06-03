use std::collections::HashMap;

use crate::{
    expression_parser::{Expression, ParseError, Parser},
    lexer::{Token, TokenSpan},
};

#[derive(Debug, Clone, PartialEq)]
pub enum CellType {
    Constant,
    Logic,
    Interface,
}

#[derive(Debug, Clone)]
pub struct Relation {
    pub name_set: Vec<String>,
    pub position: LinePosition,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct LinePosition {
    pub line: usize,
    pub column: usize,
    pub file: String,
}

impl LinePosition {
    pub fn new(file: &str, line: usize, column: usize) -> Self {
        Self {
            file: file.to_string(),
            line,
            column,
        }
    }
}

pub type Position = Box<dyn std::any::Any>;

pub trait EveCallbackSuite {
    fn add_view_proc(&mut self, name: &str, position: &Position, arguments: &Expression);
    fn add_cell_proc(
        &mut self,
        cell_type: CellType,
        name: &str,
        position: &LinePosition,
        expression: &Expression,
        brief: &str,
        detailed: &str,
    );
    fn add_relation_proc(
        &mut self,
        position: &LinePosition,
        conditional: &Expression,
        relations: &[Relation],
        brief: &str,
        detailed: &str,
    );
    fn add_interface_proc(
        &mut self,
        name: &str,
        linked: bool,
        init_position: &LinePosition,
        initializer: &Expression,
        expr_position: &LinePosition,
        expression: &Expression,
        brief: &str,
        detailed: &str,
    );
    fn finalize_sheet_proc(&mut self);
}

pub struct EveParser<T: EveCallbackSuite> {
    parser: Parser,
    assembler: T,
    keywords: HashMap<String, Token>,
}

impl<T: EveCallbackSuite> EveParser<T> {
    pub fn new(input: Vec<TokenSpan>, assembler: T) -> Self {
        let mut keywords = HashMap::new();
        keywords.insert(
            "constant".to_string(),
            Token::Keyword("constant".to_string()),
        );
        keywords.insert(
            "interface".to_string(),
            Token::Keyword("interface".to_string()),
        );
        keywords.insert("layout".to_string(), Token::Keyword("layout".to_string()));
        keywords.insert("logic".to_string(), Token::Keyword("logic".to_string()));
        keywords.insert("relate".to_string(), Token::Keyword("relate".to_string()));
        keywords.insert("unlink".to_string(), Token::Keyword("unlink".to_string()));
        keywords.insert("view".to_string(), Token::Keyword("view".to_string()));
        keywords.insert("when".to_string(), Token::Keyword("when".to_string()));

        Self {
            parser: Parser::new(input),
            assembler,
            keywords,
        }
    }

    pub fn parse(&mut self, position: &Position) -> Result<(), ParseError> {
        if !self.is_layout_specifier(position)? {
            return Err(ParseError::Other(
                "layout specifier required".to_string(),
                self.parser.line,
                self.parser.column,
            ));
        }
        Ok(())
    }

    fn is_layout_specifier(&mut self, position: &Position) -> Result<bool, ParseError> {
        // Skip lead comment
        self.is_lead_comment()?;

        if !self.is_keyword("layout")? {
            return Ok(false);
        }

        self.require_identifier()?;
        self.expect_token(Token::OpenBrace)?;

        while self.is_qualified_cell_decl()? {}

        self.assembler.finalize_sheet_proc();

        self.require_keyword("view")?;
        if !self.is_view_definition(position)? {
            return Err(ParseError::Other(
                "view definition required".to_string(),
                self.parser.line,
                self.parser.column,
            ));
        }
        self.expect_token(Token::CloseBrace)?;
        self.is_trail_comment()?;
        Ok(true)
    }

    fn is_qualified_cell_decl(&mut self) -> Result<bool, ParseError> {
        Ok(self.is_interface_set_decl()?
            || self.is_constant_set_decl()?
            || self.is_logic_set_decl()?)
    }

    fn is_interface_set_decl(&mut self) -> Result<bool, ParseError> {
        self.is_set_decl("interface", |parser| parser.is_interface_cell_decl())
    }

    fn is_constant_set_decl(&mut self) -> Result<bool, ParseError> {
        self.is_set_decl("constant", |parser| parser.is_constant_cell_decl())
    }

    fn is_logic_set_decl(&mut self) -> Result<bool, ParseError> {
        self.is_set_decl("logic", |parser| parser.is_logic_cell_decl())
    }

    fn is_set_decl<F>(&mut self, keyword: &str, mut cell_decl_fn: F) -> Result<bool, ParseError>
    where
        F: FnMut(&mut Self) -> Result<bool, ParseError>,
    {
        if !self.is_keyword(keyword)? {
            return Ok(false);
        }

        self.expect_token(Token::Colon)?;

        loop {
            let _detailed = self.is_lead_comment()?;
            if !cell_decl_fn(self)? {
                break;
            }
        }

        Ok(true)
    }

    fn is_interface_cell_decl(&mut self) -> Result<bool, ParseError> {
        let linked = !self.is_keyword("unlink")?;

        let cell_name = match self.try_identifier()? {
            Some(name) => name,
            None => return Ok(false),
        };

        let (init_position, initializer) = self.try_initializer()?;
        let (expr_position, expression) = self.try_define_expression()?;
        let brief = self.require_end_statement()?;

        self.assembler.add_interface_proc(
            &cell_name,
            linked,
            &init_position.unwrap_or_else(|| LinePosition::new("", 0, 0)),
            &initializer.unwrap_or(Expression::Literal(Token::Empty)),
            &expr_position.unwrap_or_else(|| LinePosition::new("", 0, 0)),
            &expression.unwrap_or(Expression::Literal(Token::Empty)),
            &brief,
            "",
        );

        Ok(true)
    }

    fn is_constant_cell_decl(&mut self) -> Result<bool, ParseError> {
        let cell_name = match self.try_identifier()? {
            Some(name) => name,
            None => return Ok(false),
        };

        let (position, initializer) = match self.try_initializer()? {
            (Some(pos), Some(init)) => (pos, init),
            _ => {
                return Err(ParseError::Other(
                    "initializer required".to_string(),
                    self.parser.line,
                    self.parser.column,
                ));
            }
        };

        let brief = self.require_end_statement()?;

        self.assembler.add_cell_proc(
            CellType::Constant,
            &cell_name,
            &position,
            &initializer,
            &brief,
            "",
        );

        Ok(true)
    }

    fn is_logic_cell_decl(&mut self) -> Result<bool, ParseError> {
        if let Some((cell_name, position, expression, brief)) = self.try_named_decl()? {
            self.assembler.add_cell_proc(
                CellType::Logic,
                &cell_name,
                &position,
                &expression,
                &brief,
                "",
            );
            return Ok(true);
        }

        if let Some((position, expression, relations, brief)) = self.try_relate_decl()? {
            self.assembler
                .add_relation_proc(&position, &expression, &relations, &brief, "");
            return Ok(true);
        }

        Ok(false)
    }

    fn is_view_definition(&mut self, _position: &Position) -> Result<bool, ParseError> {
        // Simplified view definition parsing
        // In a full implementation, this would parse the view hierarchy
        Ok(true)
    }

    // Helper methods
    fn is_keyword(&mut self, keyword: &str) -> Result<bool, ParseError> {
        let token = self.parser.peek()?;
        if let Token::Keyword(k) = &token.value {
            if k == keyword {
                self.parser.next()?;
                return Ok(true);
            }
        }
        if let Token::Identifier(k) = &token.value {
            if k == keyword {
                self.parser.next()?;
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn require_keyword(&mut self, keyword: &str) -> Result<(), ParseError> {
        if !self.is_keyword(keyword)? {
            return Err(ParseError::Other(
                format!("Expected keyword '{}'", keyword),
                self.parser.line,
                self.parser.column,
            ));
        }
        Ok(())
    }

    fn try_identifier(&mut self) -> Result<Option<String>, ParseError> {
        let token = self.parser.peek()?;
        if let Token::Identifier(name) = &token.value {
            let name = name.clone();
            self.parser.next()?;
            Ok(Some(name))
        } else {
            Ok(None)
        }
    }

    fn require_identifier(&mut self) -> Result<String, ParseError> {
        match self.try_identifier()? {
            Some(name) => Ok(name),
            None => Err(ParseError::Other(
                "Expected identifier".to_string(),
                self.parser.line,
                self.parser.column,
            )),
        }
    }

    fn expect_token(&mut self, expected: Token) -> Result<(), ParseError> {
        self.parser.expect_token(expected)?;
        Ok(())
    }

    fn is_lead_comment(&mut self) -> Result<String, ParseError> {
        // Simplified comment handling
        Ok(String::new())
    }

    fn is_trail_comment(&mut self) -> Result<String, ParseError> {
        // Simplified comment handling
        Ok(String::new())
    }

    fn try_initializer(
        &mut self,
    ) -> Result<(Option<LinePosition>, Option<Expression>), ParseError> {
        // Simplified initializer parsing
        Ok((None, None))
    }

    fn try_define_expression(
        &mut self,
    ) -> Result<(Option<LinePosition>, Option<Expression>), ParseError> {
        // Simplified define expression parsing
        Ok((None, None))
    }

    fn require_end_statement(&mut self) -> Result<String, ParseError> {
        // Simplified end statement parsing
        Ok(String::new())
    }

    fn try_named_decl(
        &mut self,
    ) -> Result<Option<(String, LinePosition, Expression, String)>, ParseError> {
        // Simplified named declaration parsing
        Ok(None)
    }

    fn try_relate_decl(
        &mut self,
    ) -> Result<Option<(LinePosition, Expression, Vec<Relation>, String)>, ParseError> {
        // Simplified relate declaration parsing
        Ok(None)
    }
}

pub fn parse<T: EveCallbackSuite>(
    input: Vec<TokenSpan>,
    position: &Position,
    assembler: T,
) -> Result<T, ParseError> {
    let mut parser = EveParser::new(input, assembler);
    parser.parse(position)?;
    Ok(parser.assembler)
}

