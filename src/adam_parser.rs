use std::collections::HashMap;

use crate::{
    expression_parser::{Expression, ParseError, Parser},
    lexer::{Token, TokenSpan},
    adam_evaluate::CellType,
};

#[derive(Debug, Clone)]
pub struct Relation {
    pub name_set: Vec<String>,
    pub position: LinePosition,
    pub expression: Expression,
    pub brief: String,
    pub detailed: String,
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

pub trait AdamCallbackSuite {
    fn add_cell_proc(
        &mut self,
        cell_type: CellType,
        name: &str,
        position: &LinePosition,
        init_or_expr: &Expression,
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
    
    fn add_external_proc(
        &mut self,
        name: &str,
        position: &LinePosition,
        brief: &str,
        detailed: &str,
    );
}

pub struct AdamParser<T: AdamCallbackSuite> {
    parser: Parser,
    callbacks: T,
    keywords: HashMap<String, Token>,
}

impl<T: AdamCallbackSuite> AdamParser<T> {
    pub fn new(input: Vec<TokenSpan>, callbacks: T) -> Self {
        let mut keywords = HashMap::new();
        keywords.insert("constant".to_string(), Token::Keyword("constant".to_string()));
        keywords.insert("external".to_string(), Token::Keyword("external".to_string()));
        keywords.insert("input".to_string(), Token::Keyword("input".to_string()));
        keywords.insert("interface".to_string(), Token::Keyword("interface".to_string()));
        keywords.insert("invariant".to_string(), Token::Keyword("invariant".to_string()));
        keywords.insert("logic".to_string(), Token::Keyword("logic".to_string()));
        keywords.insert("output".to_string(), Token::Keyword("output".to_string()));
        keywords.insert("relate".to_string(), Token::Keyword("relate".to_string()));
        keywords.insert("sheet".to_string(), Token::Keyword("sheet".to_string()));
        keywords.insert("unlink".to_string(), Token::Keyword("unlink".to_string()));
        keywords.insert("when".to_string(), Token::Keyword("when".to_string()));

        Self {
            parser: Parser::new(input),
            callbacks,
            keywords,
        }
    }

    pub fn parse(&mut self) -> Result<(), ParseError> {
        while self.is_sheet_specifier()? {}
        self.require_token(Token::Empty)?; // EOF equivalent
        Ok(())
    }

    // sheet_specifier = [lead_comment] "sheet" identifier "{" { qualified_cell_decl } "}" [trail_comment].
    fn is_sheet_specifier(&mut self) -> Result<bool, ParseError> {
        self.is_lead_comment()?;
        
        if !self.is_keyword("sheet")? {
            return Ok(false);
        }

        self.require_identifier()?;
        self.expect_token(Token::OpenBrace)?;

        while self.is_qualified_cell_decl()? {}

        self.expect_token(Token::CloseBrace)?;
        self.is_trail_comment()?;
        Ok(true)
    }

    // qualified_cell_decl = interface_set_decl | input_set_decl | output_set_decl
    //                     | constant_set_decl | logic_set_decl | invariant_set_decl
    //                     | external_set_decl.
    fn is_qualified_cell_decl(&mut self) -> Result<bool, ParseError> {
        Ok(self.is_interface_set_decl()?
            || self.is_input_set_decl()?
            || self.is_output_set_decl()?
            || self.is_constant_set_decl()?
            || self.is_logic_set_decl()?
            || self.is_invariant_set_decl()?
            || self.is_external_set_decl()?)
    }

    // interface_set_decl = "interface" ":" { [lead_comment] interface_cell_decl }.
    fn is_interface_set_decl(&mut self) -> Result<bool, ParseError> {
        self.is_set_decl("interface", |parser| parser.is_interface_cell_decl())
    }

    // input_set_decl = "input" ":" { [lead_comment] input_cell_decl }.
    fn is_input_set_decl(&mut self) -> Result<bool, ParseError> {
        self.is_set_decl("input", |parser| parser.is_input_cell_decl())
    }

    // output_set_decl = "output" ":" { [lead_comment] output_cell_decl }.
    fn is_output_set_decl(&mut self) -> Result<bool, ParseError> {
        self.is_set_decl("output", |parser| parser.is_output_cell_decl())
    }

    // constant_set_decl = "constant" ":" { [lead_comment] constant_cell_decl }.
    fn is_constant_set_decl(&mut self) -> Result<bool, ParseError> {
        self.is_set_decl("constant", |parser| parser.is_constant_cell_decl())
    }

    // logic_set_decl = "logic" ":" { [lead_comment] logic_cell_decl }.
    fn is_logic_set_decl(&mut self) -> Result<bool, ParseError> {
        self.is_set_decl("logic", |parser| parser.is_logic_cell_decl())
    }

    // invariant_set_decl = "invariant" ":" { [lead_comment] invariant_cell_decl }.
    fn is_invariant_set_decl(&mut self) -> Result<bool, ParseError> {
        self.is_set_decl("invariant", |parser| parser.is_invariant_cell_decl())
    }

    // external_set_decl = "external" ":" { [lead_comment] identifier end_statement }.
    fn is_external_set_decl(&mut self) -> Result<bool, ParseError> {
        if !self.is_keyword("external")? {
            return Ok(false);
        }

        self.expect_token(Token::Colon)?;

        loop {
            let detailed = self.is_lead_comment()?;
            let position = self.next_position();
            
            if let Some(cell_name) = self.try_identifier()? {
                let brief = self.require_end_statement()?;
                self.callbacks.add_external_proc(&cell_name, &position, &brief, &detailed);
            } else {
                break;
            }
        }

        Ok(true)
    }

    // interface_cell_decl = ["unlink"] identifier [initializer] [define_expression] end_statement.
    fn is_interface_cell_decl(&mut self) -> Result<bool, ParseError> {
        let detailed = self.is_lead_comment()?;
        let linked = !self.is_keyword("unlink")?;

        if let Some(cell_name) = self.try_identifier()? {
            let (init_position, initializer) = self.try_initializer()?;
            let (expr_position, expression) = self.try_define_expression()?;
            let brief = self.require_end_statement()?;

            self.callbacks.add_interface_proc(
                &cell_name,
                linked,
                &init_position.unwrap_or_else(|| LinePosition::new("", 0, 0)),
                &initializer.unwrap_or(Expression::Literal(Token::Empty)),
                &expr_position.unwrap_or_else(|| LinePosition::new("", 0, 0)),
                &expression.unwrap_or(Expression::Literal(Token::Empty)),
                &brief,
                &detailed,
            );
            Ok(true)
        } else {
            Ok(false)
        }
    }

    // input_cell_decl = identifier [initializer] end_statement.
    fn is_input_cell_decl(&mut self) -> Result<bool, ParseError> {
        let detailed = self.is_lead_comment()?;
        
        if let Some(cell_name) = self.try_identifier()? {
            let (position, initializer) = self.try_initializer()?;
            let brief = self.require_end_statement()?;

            self.callbacks.add_cell_proc(
                CellType::Input,
                &cell_name,
                &position.unwrap_or_else(|| LinePosition::new("", 0, 0)),
                &initializer.unwrap_or(Expression::Literal(Token::Empty)),
                &brief,
                &detailed,
            );
            Ok(true)
        } else {
            Ok(false)
        }
    }

    // output_cell_decl = named_decl.
    fn is_output_cell_decl(&mut self) -> Result<bool, ParseError> {
        let detailed = self.is_lead_comment()?;
        
        if let Some((cell_name, position, expression, brief)) = self.try_named_decl()? {
            self.callbacks.add_cell_proc(
                CellType::Output,
                &cell_name,
                &position,
                &expression,
                &brief,
                &detailed,
            );
            Ok(true)
        } else {
            Ok(false)
        }
    }

    // constant_cell_decl = identifier initializer end_statement.
    fn is_constant_cell_decl(&mut self) -> Result<bool, ParseError> {
        let detailed = self.is_lead_comment()?;
        
        if let Some(cell_name) = self.try_identifier()? {
            let (position, initializer) = match self.try_initializer()? {
                (Some(pos), Some(init)) => (pos, init),
                _ => {
                    return Err(ParseError::Other(
                        "initializer required".to_string(),
                        self.parser.line,
                        self.parser.column,
                    ))
                }
            };
            let brief = self.require_end_statement()?;

            self.callbacks.add_cell_proc(
                CellType::Constant,
                &cell_name,
                &position,
                &initializer,
                &brief,
                &detailed,
            );
            Ok(true)
        } else {
            Ok(false)
        }
    }

    // logic_cell_decl = named_decl | relate_decl.
    fn is_logic_cell_decl(&mut self) -> Result<bool, ParseError> {
        let detailed = self.is_lead_comment()?;
        
        if let Some((cell_name, position, expression, brief)) = self.try_named_decl()? {
            self.callbacks.add_cell_proc(
                CellType::Logic,
                &cell_name,
                &position,
                &expression,
                &brief,
                &detailed,
            );
            Ok(true)
        } else if let Some((position, expression, relations, brief)) = self.try_relate_decl()? {
            self.callbacks.add_relation_proc(
                &position,
                &expression,
                &relations,
                &brief,
                &detailed,
            );
            Ok(true)
        } else {
            Ok(false)
        }
    }

    // invariant_cell_decl = named_decl.
    fn is_invariant_cell_decl(&mut self) -> Result<bool, ParseError> {
        let detailed = self.is_lead_comment()?;
        
        if let Some((cell_name, position, expression, brief)) = self.try_named_decl()? {
            self.callbacks.add_cell_proc(
                CellType::Invariant,
                &cell_name,
                &position,
                &expression,
                &brief,
                &detailed,
            );
            Ok(true)
        } else {
            Ok(false)
        }
    }

    // relate_decl = [conditional] "relate" "{" relate_expression relate_expression { relate_expression } "}" [trail_comment]
    fn try_relate_decl(&mut self) -> Result<Option<(LinePosition, Expression, Vec<Relation>, String)>, ParseError> {
        let (conditional, position, expression) = if let Some((pos, expr)) = self.try_conditional()? {
            (true, pos, expr)
        } else {
            (false, LinePosition::new("", 0, 0), Expression::Literal(Token::Empty))
        };

        if !self.is_keyword("relate")? {
            if conditional {
                return Err(ParseError::Other(
                    "relate required".to_string(),
                    self.parser.line,
                    self.parser.column,
                ));
            }
            return Ok(None);
        }

        let position = if !conditional {
            self.next_position()
        } else {
            position
        };

        self.expect_token(Token::OpenBrace)?;

        let mut relations = Vec::new();
        
        // Require minimum two relate expressions
        if let Some(relation1) = self.try_relate_expression_decl()? {
            relations.push(relation1);
        } else {
            return Err(ParseError::Other(
                "minimum two relate_expression required".to_string(),
                self.parser.line,
                self.parser.column,
            ));
        }

        if let Some(relation2) = self.try_relate_expression_decl()? {
            relations.push(relation2);
        } else {
            return Err(ParseError::Other(
                "minimum two relate_expression required".to_string(),
                self.parser.line,
                self.parser.column,
            ));
        }

        // Parse additional relations
        while let Some(relation) = self.try_relate_expression_decl()? {
            relations.push(relation);
        }

        self.expect_token(Token::CloseBrace)?;
        let brief = self.is_trail_comment()?;

        Ok(Some((position, expression, relations, brief)))
    }

    // relate_expression = [lead_comment] identifier { "," identifier } define_expression end_statement.
    fn try_relate_expression_decl(&mut self) -> Result<Option<Relation>, ParseError> {
        let detailed = self.is_lead_comment()?;
        
        if let Some(cell_name) = self.try_identifier()? {
            let mut name_set = vec![cell_name];

            while self.match_token(Token::Comma)? {
                if let Some(name) = self.try_identifier()? {
                    name_set.push(name);
                } else {
                    return Err(ParseError::Other(
                        "identifier required".to_string(),
                        self.parser.line,
                        self.parser.column,
                    ));
                }
            }

            let (position, expression) = match self.try_define_expression()? {
                (Some(pos), Some(expr)) => (pos, expr),
                _ => {
                    return Err(ParseError::Other(
                        "define_expression required".to_string(),
                        self.parser.line,
                        self.parser.column,
                    ))
                }
            };

            let brief = self.require_end_statement()?;

            Ok(Some(Relation {
                name_set,
                position,
                expression,
                brief,
                detailed,
            }))
        } else {
            Ok(None)
        }
    }

    // Helper methods
    fn is_set_decl<F>(&mut self, keyword: &str, mut cell_decl_fn: F) -> Result<bool, ParseError>
    where
        F: FnMut(&mut Self) -> Result<bool, ParseError>,
    {
        if !self.is_keyword(keyword)? {
            return Ok(false);
        }

        self.expect_token(Token::Colon)?;

        loop {
            if !cell_decl_fn(self)? {
                break;
            }
        }

        Ok(true)
    }

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

    fn require_token(&mut self, expected: Token) -> Result<(), ParseError> {
        self.parser.expect_token(expected)?;
        Ok(())
    }

    fn match_token(&mut self, expected: Token) -> Result<bool, ParseError> {
        let token = self.parser.peek()?;
        if token.value == expected {
            self.parser.next()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn is_lead_comment(&mut self) -> Result<String, ParseError> {
        // Simplified comment handling
        Ok(String::new())
    }

    fn is_trail_comment(&mut self) -> Result<String, ParseError> {
        // Simplified comment handling
        Ok(String::new())
    }

    fn try_initializer(&mut self) -> Result<(Option<LinePosition>, Option<Expression>), ParseError> {
        if self.match_token(Token::Colon)? {
            let position = self.next_position();
            let expression = self.parser.parse_expression()?;
            Ok((Some(position), Some(expression)))
        } else {
            Ok((None, None))
        }
    }

    fn try_define_expression(&mut self) -> Result<(Option<LinePosition>, Option<Expression>), ParseError> {
        // Check for "<==" token (represented as a specific token or sequence)
        // For simplicity, using a placeholder check
        if self.match_token(Token::Equality)? { // Placeholder - should be "<==" token
            let position = self.next_position();
            let expression = self.parser.parse_expression()?;
            Ok((Some(position), Some(expression)))
        } else {
            Ok((None, None))
        }
    }

    fn try_conditional(&mut self) -> Result<Option<(LinePosition, Expression)>, ParseError> {
        if self.is_keyword("when")? {
            self.expect_token(Token::OpenParen)?;
            let position = self.next_position();
            let expression = self.parser.parse_expression()?;
            self.expect_token(Token::CloseParen)?;
            Ok(Some((position, expression)))
        } else {
            Ok(None)
        }
    }

    fn try_named_decl(&mut self) -> Result<Option<(String, LinePosition, Expression, String)>, ParseError> {
        if let Some(cell_name) = self.try_identifier()? {
            let (position, expression) = match self.try_define_expression()? {
                (Some(pos), Some(expr)) => (pos, expr),
                _ => {
                    return Err(ParseError::Other(
                        "define_expression required".to_string(),
                        self.parser.line,
                        self.parser.column,
                    ))
                }
            };
            let brief = self.require_end_statement()?;
            Ok(Some((cell_name, position, expression, brief)))
        } else {
            Ok(None)
        }
    }

    fn require_end_statement(&mut self) -> Result<String, ParseError> {
        self.expect_token(Token::Semicolon)?;
        let brief = self.is_trail_comment()?;
        Ok(brief)
    }

    fn next_position(&self) -> LinePosition {
        LinePosition::new("", self.parser.line, self.parser.column)
    }
}

pub fn parse<T: AdamCallbackSuite>(
    input: Vec<TokenSpan>,
    mut callbacks: T,
) -> Result<T, ParseError> {
    let mut parser = AdamParser::new(input, callbacks);
    parser.parse()?;
    Ok(parser.callbacks)
}

pub fn adam_keyword_lookup(name: &str) -> bool {
    matches!(
        name,
        "constant" | "external" | "input" | "interface" | "invariant" 
        | "logic" | "output" | "relate" | "sheet" | "unlink" | "when"
    )
} 