use std::vec::IntoIter;

use itertools::{Itertools, MultiPeek};

use crate::lexer::{LexError, Token, TokenSpan};

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Token),
    Identifier(String),
    Unary {
        operator: Token,
        operand: Box<Expression>,
    },
    Binary {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
    },
    Ternary {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Box<Expression>,
    },
    FunctionCall {
        name: String,
        arguments: Vec<Expression>,
        named_arguments: Vec<(String, Expression)>,
    },
    ArrayAccess {
        array: Box<Expression>,
        index: Box<Expression>,
    },
    MemberAccess {
        object: Box<Expression>,
        member: String,
    },
    ArrayLiteral(Vec<Expression>),
    DictionaryLiteral(Vec<(String, Expression)>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    UnexpectedToken {
        expected: Option<String>,
        found: Token,
        line: usize,
        column: usize,
    },
    UnexpectedEof {
        line: usize,
        column: usize,
    },
    Other(String, usize, usize),
}

impl From<LexError> for ParseError {
    fn from(value: LexError) -> Self {
        match value {
            LexError::UnexpectedEof { line, column } => ParseError::UnexpectedEof { line, column },
            LexError::UnexpectedChar {
                expected,
                found,
                line,
                column,
            } => ParseError::UnexpectedToken {
                expected: Some(expected.to_string()),
                found: Token::Empty,
                line,
                column,
            },
            LexError::LexerError { err, line, column } => ParseError::Other(err, line, column),
        }
    }
}

pub struct Parser {
    tokens: MultiPeek<IntoIter<TokenSpan>>,
    pub line: usize,
    pub column: usize,
    // TODO: Stack
}

impl Parser {
    pub fn new(input: Vec<TokenSpan>) -> Self {
        let stream = input.into_iter().multipeek();
        Self {
            tokens: stream,
            line: 1,
            column: 1,
        }
    }

    pub fn peek(&mut self) -> Result<TokenSpan, ParseError> {
        let t = self
            .tokens
            .peek()
            .ok_or(ParseError::UnexpectedEof {
                line: self.line,
                column: self.column,
            })?
            .clone();
        self.tokens.reset_peek();
        // dbg!(&t);
        Ok(t)
    }
    pub fn peek_multi(&mut self) -> Result<TokenSpan, ParseError> {
        let t = self.tokens.peek().ok_or(ParseError::UnexpectedEof {
            line: self.line,
            column: self.column,
        })?;
        Ok(t.clone())
    }

    pub fn next(&mut self) -> Result<TokenSpan, ParseError> {
        let t = self.tokens.next().ok_or(ParseError::UnexpectedEof {
            line: self.line,
            column: self.column,
        })?;
        self.line = t.span.line;
        self.column = t.span.column;
        Ok(t)
    }

    pub fn expect_token(&mut self, expected: Token) -> Result<TokenSpan, ParseError> {
        let t = self.peek()?;

        // if std::mem::discriminant(&expected) == std::mem::discriminant(&t.value) {
        if t.value == expected {
            self.next()
        } else {
            Err(ParseError::UnexpectedToken {
                expected: Some(format!("Expected token: {expected:?}")),
                found: t.value.clone(),
                line: t.span.line,
                column: t.span.column,
            })
        }
    }

    fn match_token(&mut self, expected: Token) -> Result<bool, ParseError> {
        let t = self.peek()?;

        // if std::mem::discriminant(&expected) == std::mem::discriminant(&t.value) {
        if t.value == expected {
            self.next()?;
            return Ok(true);
        }
        Ok(false)
    }

    // expression = or_expression ["?" expression ":" expression].
    pub fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        let mut condition = self.parse_or_expression()?;

        if self.match_token(Token::QuestionMark)? {
            let then_branch = self.parse_expression()?;
            self.expect_token(Token::Colon)?;
            let else_branch = self.parse_expression()?;
            condition = Expression::Ternary {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            };
        }
        Ok(condition)
    }

    // or_expression = and_expression { "||" and_expression }.
    fn parse_or_expression(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_and_expression()?;

        while self.match_token(Token::Or)? {
            let right = self.parse_and_expression()?;
            left = Expression::Binary {
                left: Box::new(left),
                operator: Token::Or, // The consumed token was Or
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    // and_expression = bitwise_or_expression { "&&" bitwise_or_expression }.
    fn parse_and_expression(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_bitwise_or_expression()?;
        while self.match_token(Token::And)? {
            let right = self.parse_bitwise_or_expression()?;
            left = Expression::Binary {
                left: Box::new(left),
                operator: Token::And,
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    // bitwise_or_expression = bitwise_xor_expression { "|" bitwise_xor_expression }.
    fn parse_bitwise_or_expression(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_bitwise_xor_expression()?;
        while self.match_token(Token::BitwiseOr)? {
            let right = self.parse_bitwise_xor_expression()?;
            left = Expression::Binary {
                left: Box::new(left),
                operator: Token::BitwiseOr,
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    // bitwise_xor_expression = bitwise_and_expression { "^" bitwise_and_expression }.
    fn parse_bitwise_xor_expression(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_bitwise_and_expression()?;
        while self.match_token(Token::BitwiseXor)? {
            let right = self.parse_bitwise_and_expression()?;
            left = Expression::Binary {
                left: Box::new(left),
                operator: Token::BitwiseXor,
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    // bitwise_and_expression = equality_expression { "&" equality_expression }.
    fn parse_bitwise_and_expression(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_equality_expression()?;
        while self.match_token(Token::BitwiseAnd)? {
            let right = self.parse_equality_expression()?;
            left = Expression::Binary {
                left: Box::new(left),
                operator: Token::BitwiseAnd,
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    // equality_expression = relational_expression { ("==" | "!=") relational_expression }.
    fn parse_equality_expression(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_relational_expression()?;
        loop {
            let current_op = self.peek()?.value;
            if current_op == Token::Equality || current_op == Token::NotEqual {
                self.next()?;
                let right = self.parse_relational_expression()?;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: current_op,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(left)
    }

    // relational_expression = bitshift_expression { ("<" | ">" | "<=" | ">=") bitshift_expression }.
    fn parse_relational_expression(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_bitshift_expression()?;
        loop {
            let current_op = self.peek()?.value;
            if current_op == Token::LessThan
                || current_op == Token::GreaterThan
                || current_op == Token::LessThanOrEqual
                || current_op == Token::GreaterThanOrEqual
            {
                self.next()?;
                let right = self.parse_bitshift_expression()?;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: current_op,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(left)
    }

    // bitshift_expression = additive_expression { ("<<" | ">>") additive_expression }.
    fn parse_bitshift_expression(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_additive_expression()?;
        loop {
            let current_op = self.peek()?.value;
            if current_op == Token::LeftShift || current_op == Token::RightShift {
                self.next()?;
                let right = self.parse_additive_expression()?;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: current_op,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(left)
    }

    // additive_expression = multiplicative_expression { ("+" | "-") multiplicative_expression }.
    fn parse_additive_expression(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_multiplicative_expression()?;

        loop {
            let current_op = self.peek()?.value;
            if current_op == Token::Plus || current_op == Token::Minus {
                self.next()?;
                let right = self.parse_multiplicative_expression()?;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: current_op,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(left)
    }

    // multiplicative_expression = unary_expression { ("*" | "/" | "%") unary_expression }.
    // TODO: handle options here
    fn parse_multiplicative_expression(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_unary_expression()?;
        loop {
            let current_op = self.peek()?.value;
            if current_op == Token::Multiply
                || current_op == Token::Divide
                || current_op == Token::Modulus
            {
                self.next()?;
                let right = self.parse_unary_expression()?;
                left = Expression::Binary {
                    left: Box::new(left),
                    operator: current_op,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(left)
    }

    // unary_expression = postfix_expression | (unary_operator unary_expression).
    fn parse_unary_expression(&mut self) -> Result<Expression, ParseError> {
        let token = self.peek()?.value;
        match token {
            Token::Plus | Token::Minus | Token::Not | Token::BitwiseNot => {
                self.next()?; // Consume the unary operator
                let operand = self.parse_unary_expression()?;

                if token == Token::Plus {
                    Ok(operand)
                } else {
                    Ok(Expression::Unary {
                        operator: token,
                        operand: Box::new(operand),
                    })
                }
            }
            _ => self.parse_postfix_expression(),
        }
    }

    // postfix_expression = primary_expression { ("[" expression "]") | ("." identifier) }.
    fn parse_postfix_expression(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_primary_expression()?;

        loop {
            if self.match_token(Token::OpenBracket)? {
                // Array access: expr[index_expr]
                let index_expr = self.parse_expression()?;
                self.expect_token(Token::CloseBracket)?;
                expr = Expression::ArrayAccess {
                    array: Box::new(expr),
                    index: Box::new(index_expr),
                };
            } else if self.match_token(Token::Dot)? {
                // Member access: expr.identifier
                let member_token = self.next()?;
                if let Token::Identifier(name) = member_token.value {
                    expr = Expression::MemberAccess {
                        object: Box::new(expr),
                        member: name,
                    };
                } else {
                    return Err(ParseError::UnexpectedToken {
                        expected: Some("Identifier after '.'".to_string()),
                        found: member_token.value,
                        line: member_token.span.line,
                        column: member_token.span.column,
                    });
                }
            } else {
                break; // No postfix operator found
            }
        }
        Ok(expr)
    }

    // primary_expression = name | number | boolean | string | "empty" | array | dictionary
    //      | variable_or_function | ( "(" expression ")" ).
    fn parse_primary_expression(&mut self) -> Result<Expression, ParseError> {
        let peekts = self.peek()?;
        let peek = peekts.value.clone();
        match peek {
            Token::At => self.parse_name_expression(),
            Token::Number(_) | Token::String(_) | Token::True | Token::False | Token::Empty => {
                self.next()?;
                Ok(Expression::Literal(peek))
            }
            Token::OpenBracket => self.parse_array_literal(),
            Token::OpenBrace => self.parse_dictionary_literal(),
            Token::Identifier(_) => self.parse_variable_or_function(), // Expects to consume the Identifier
            Token::OpenParen => {
                self.next()?; // Consume '('
                let expr = self.parse_expression()?;
                self.expect_token(Token::CloseParen)?;
                Ok(expr)
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: Some(
                    "primary expression type (@, literal, [, {, identifier, (".to_string(),
                ),
                found: peek,
                line: peekts.span.line,
                column: peekts.span.column,
            }),
        }
    }

    // name = "@" (identifier | keyword).
    fn parse_name_expression(&mut self) -> Result<Expression, ParseError> {
        self.expect_token(Token::At)?; // Consume '@'
        let name_token = self.next()?;
        match name_token.value {
            Token::Identifier(id) => Ok(Expression::Literal(Token::Name(id))), // Or a specific Expression::Name variant
            Token::Keyword(kw) => {
                // The C++ parser allows @keyword. We represent this as Token::Name containing the keyword string.
                Ok(Expression::Literal(Token::Name(kw)))
            }
            // Specific keywords like true/false/empty if they were lexed as such and allowed after @
            Token::True => Ok(Expression::Literal(Token::Name("true".into()))),
            Token::False => Ok(Expression::Literal(Token::Name("false".into()))),
            Token::Empty => Ok(Expression::Literal(Token::Name("empty".to_string()))),
            _ => Err(ParseError::UnexpectedToken {
                expected: Some("Identifier or Keyword after '@'".to_string()),
                found: name_token.value,
                line: name_token.span.line,
                column: name_token.span.column,
            }),
        }
    }

    // variable_or_function = identifier ["(" [argument_expression_list] ")"].
    fn parse_variable_or_function(&mut self) -> Result<Expression, ParseError> {
        let tokenspan = self.peek()?; // Peek, do not consume yet.
        let name = match tokenspan.value {
            Token::Identifier(n) => n,
            _ => {
                // This function should only be called if an identifier is expected/peeked.
                // If called from primary_expression, primary_expression should ensure current token is Identifier.
                return Err(ParseError::UnexpectedToken {
                    expected: Some("Identifier for variable or function".to_string()),
                    found: tokenspan.value,
                    line: tokenspan.span.line,     // TODO from token
                    column: tokenspan.span.column, // TODO from token
                });
            }
        };
        self.next()?; // Now consume the identifier

        if self.match_token(Token::OpenParen)? {
            // Function call
            let (arguments, named_arguments) = self.parse_argument_expression_list()?;
            self.expect_token(Token::CloseParen)?;
            Ok(Expression::FunctionCall {
                name,
                arguments,
                named_arguments,
            })
        } else {
            // Variable
            Ok(Expression::Identifier(name)) // Or Expression::Variable(name)
        }
    }

    // argument_expression_list = named_argument_list | argument_list.
    fn parse_argument_expression_list(
        &mut self,
    ) -> Result<(Vec<Expression>, Vec<(String, Expression)>), ParseError> {
        let peek = self.peek()?;

        // If next token is CloseParen, then it's an empty list.
        if peek.value == Token::CloseParen {
            return Ok((Vec::new(), Vec::new()));
        }

        // Tentative parsing: Peek for `identifier :` sequence for named arguments.
        let lookahead2 = (self.peek_multi()?.value, self.peek_multi()?.value);
        if let (Token::Identifier(_), Token::Colon) = lookahead2 {
            self.parse_named_argument_list()
                .map(|args| (Vec::new(), args))
        } else {
            self.parse_argument_list().map(|args| (args, Vec::new()))
        }
    }

    // argument_list = expression { "," expression }.
    fn parse_argument_list(&mut self) -> Result<Vec<Expression>, ParseError> {
        println!("arg_list");
        let mut args = Vec::new();
        // If the list can be empty and this is called, it implies an issue.
        // `parse_argument_expression_list` should handle the empty case before calling this.
        let current = self.peek()?.value;
        if current == Token::CloseBracket || current == Token::CloseParen {
            return Ok(args);
        }

        args.push(self.parse_expression()?);
        while self.match_token(Token::Comma)? {
            args.push(self.parse_expression()?);
        }
        Ok(args)
    }

    // named_argument_list = named_argument { "," named_argument }.
    fn parse_named_argument_list(&mut self) -> Result<Vec<(String, Expression)>, ParseError> {
        println!("named_arg_list");
        let mut named_args = Vec::new();

        let peek = self.peek()?.value;
        if peek == Token::CloseBracket || peek == Token::CloseParen || peek == Token::CloseBrace {
            return Ok(named_args);
        }

        named_args.push(self.parse_named_argument()?);
        while self.match_token(Token::Comma)? {
            named_args.push(self.parse_named_argument()?);
        }
        Ok(named_args)
    }

    // named_argument = identifier ":" expression.
    fn parse_named_argument(&mut self) -> Result<(String, Expression), ParseError> {
        println!("named_arg");
        let ident_token = self.next()?;
        let name = match ident_token.value {
            Token::Identifier(n) => n,
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: Some("Identifier for named argument".to_string()),
                    found: ident_token.value,
                    line: ident_token.span.line,
                    column: ident_token.span.column,
                });
            }
        };
        self.expect_token(Token::Colon)?;
        let expr = self.parse_expression()?;
        Ok((name, expr))
    }

    // array = "[" [argument_list] "]".
    fn parse_array_literal(&mut self) -> Result<Expression, ParseError> {
        println!("array_literal");
        self.expect_token(Token::OpenBracket)?;
        let elements = self.parse_argument_list()?;
        self.expect_token(Token::CloseBracket)?;
        Ok(Expression::ArrayLiteral(elements))
    }

    // dictionary = "{" [named_argument_list] "}".
    fn parse_dictionary_literal(&mut self) -> Result<Expression, ParseError> {
        println!("dict_literal");
        self.expect_token(Token::OpenBrace)?;
        let members = self.parse_named_argument_list()?;
        self.expect_token(Token::CloseBrace)?;
        Ok(Expression::DictionaryLiteral(members))
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;

    use super::*;

    // Helper to run lexer and parser for tests
    fn parse_str(input: &str) -> Result<Expression, ParseError> {
        let mut lexer = Lexer::new(input);
        lexer.lex()?;
        let mut parser = Parser::new(lexer.tokens);
        parser.parse_expression()
    }

    #[test]
    fn test_simple_number() {
        assert_eq!(
            parse_str("123"),
            Ok(Expression::Literal(Token::Number(123.0)))
        );
    }

    #[test]
    fn test_simple_string() {
        assert_eq!(
            parse_str("\"hello\""),
            Ok(Expression::Literal(Token::String("hello".to_string())))
        );
    }

    #[test]
    fn test_boolean_true() {
        assert_eq!(parse_str("true"), Ok(Expression::Literal(Token::True)));
    }

    #[test]
    fn test_multipeek() {
        let input = "hey there";
        let mut lexer = Lexer::new(input);
        lexer.lex().unwrap();
        println!("{:?}", lexer.tokens);
        let mut parser = Parser::new(lexer.tokens);
        assert_eq!(
            Token::Identifier("hey".into()),
            parser.peek_multi().unwrap().value
        );
        assert_eq!(
            Token::Identifier("there".into()),
            parser.peek_multi().unwrap().value
        );
    }

    #[test]
    fn test_boolean_false() {
        assert_eq!(parse_str("false"), Ok(Expression::Literal(Token::False)));
    }

    // #[test]
    // fn test_stuff() {
    //     assert_eq!(
    //         parse_str("5 > 3 ? x = 2 : x = 3 "),
    //         Ok(Expression::Literal(Token::False))
    //     );
    // }

    #[test]
    fn test_empty_keyword() {
        assert_eq!(parse_str("empty"), Ok(Expression::Literal(Token::Empty)));
    }

    #[test]
    fn test_parenthesized_expression() {
        assert_eq!(
            parse_str("(123)"),
            Ok(Expression::Literal(Token::Number(123.0)))
        );
    }

    #[test]
    fn test_name_expression() {
        assert_eq!(
            parse_str("@my_var"),
            Ok(Expression::Literal(Token::Name("my_var".to_string())))
        );
    }

    #[test]
    fn test_name_with_keyword() {
        assert_eq!(
            parse_str("@true"),
            Ok(Expression::Literal(Token::Name("true".to_string())))
        );
    }

    #[test]
    fn test_simple_variable() {
        assert_eq!(
            parse_str("myVar"),
            Ok(Expression::Identifier("myVar".to_string()))
        );
    }

    #[test]
    fn test_simple_function_call_no_args() {
        assert_eq!(
            parse_str("myFunc()"),
            Ok(Expression::FunctionCall {
                name: "myFunc".to_string(),
                arguments: vec![],
                named_arguments: vec![]
            })
        );
    }

    #[test]
    fn test_function_call_one_arg() {
        assert_eq!(
            parse_str("doSomething(1)"),
            Ok(Expression::FunctionCall {
                name: "doSomething".to_string(),
                arguments: vec![Expression::Literal(Token::Number(1.0))],
                named_arguments: vec![]
            })
        );
    }

    #[test]
    fn test_function_call_multiple_args() {
        assert_eq!(
            parse_str("calc(1, \"test\")"),
            Ok(Expression::FunctionCall {
                name: "calc".to_string(),
                arguments: vec![
                    Expression::Literal(Token::Number(1.0)),
                    Expression::Literal(Token::String("test".to_string())),
                ],
                named_arguments: vec![]
            })
        );
    }

    #[test]
    fn test_function_call_one_named_arg() {
        assert_eq!(
            parse_str("config(mode: \"fast\")"),
            Ok(Expression::FunctionCall {
                name: "config".to_string(),
                arguments: vec![], // No positional if only named were parsed this way
                named_arguments: vec![(
                    "mode".to_string(),
                    Expression::Literal(Token::String("fast".to_string()))
                )]
            })
        );
    }

    #[test]
    fn test_function_call_mixed_args_fail_current_logic() {
        // Current parse_argument_expression_list commits to either full positional or full named.
        // This test should fail as `num:1` would be seen, then `2` would be an error.
        let result = parse_str("setup(num:1, 2)");
        assert!(result.is_err());
    }

    #[test]
    fn test_empty_array_literal() {
        assert_eq!(parse_str("[]"), Ok(Expression::ArrayLiteral(vec![])));
    }

    #[test]
    fn test_array_literal_one_element() {
        assert_eq!(
            parse_str("[123]"),
            Ok(Expression::ArrayLiteral(vec![Expression::Literal(
                Token::Number(123.0)
            )]))
        );
    }

    #[test]
    fn test_array_literal_multiple_elements() {
        assert_eq!(
            parse_str("[1, \"two\"]"),
            Ok(Expression::ArrayLiteral(vec![
                Expression::Literal(Token::Number(1.0)),
                Expression::Literal(Token::String("two".to_string())),
            ]))
        );
    }

    #[test]
    fn test_empty_dictionary_literal() {
        assert_eq!(parse_str("{}"), Ok(Expression::DictionaryLiteral(vec![])));
    }

    #[test]
    fn test_dictionary_literal_one_member() {
        assert_eq!(
            parse_str("{key: \"value\"}"),
            Ok(Expression::DictionaryLiteral(vec![(
                "key".to_string(),
                Expression::Literal(Token::String("value".to_string()))
            )]))
        );
    }

    #[test]
    fn test_dictionary_literal_multiple_members() {
        assert_eq!(
            parse_str("{a: 1, b: true}"),
            Ok(Expression::DictionaryLiteral(vec![
                ("a".to_string(), Expression::Literal(Token::Number(1.0))),
                ("b".to_string(), Expression::Literal(Token::True)),
            ]))
        );
    }

    #[test]
    fn test_addition() {
        let expected = Expression::Binary {
            left: Box::new(Expression::Literal(Token::Number(1.0))),
            operator: Token::Plus,
            right: Box::new(Expression::Literal(Token::Number(2.0))),
        };
        assert_eq!(parse_str("1 + 2"), Ok(expected));
    }

    #[test]
    fn test_ternary_operator() {
        let expected = Expression::Ternary {
            condition: Box::new(Expression::Literal(Token::True)),
            then_branch: Box::new(Expression::Literal(Token::Number(1.0))),
            else_branch: Box::new(Expression::Literal(Token::Number(2.0))),
        };
        assert_eq!(parse_str("true ? 1 : 2"), Ok(expected));
    }

    #[test]
    fn test_unary_minus() {
        let expected = Expression::Unary {
            operator: Token::Minus,
            operand: Box::new(Expression::Literal(Token::Number(5.0))),
        };
        assert_eq!(parse_str("-5"), Ok(expected));
    }

    #[test]
    fn test_unary_not() {
        let expected = Expression::Unary {
            operator: Token::Not,
            operand: Box::new(Expression::Literal(Token::True)),
        };
        assert_eq!(parse_str("!true"), Ok(expected));
    }

    #[test]
    fn test_postfix_array_access() {
        let expected = Expression::ArrayAccess {
            array: Box::new(Expression::Identifier("myArray".to_string())),
            index: Box::new(Expression::Literal(Token::Number(0.0))),
        };
        assert_eq!(parse_str("myArray[0]"), Ok(expected));
    }

    #[test]
    fn test_postfix_member_access() {
        let expected = Expression::MemberAccess {
            object: Box::new(Expression::Identifier("myObj".to_string())),
            member: "field".to_string(),
        };
        assert_eq!(parse_str("myObj.field"), Ok(expected));
    }
}
