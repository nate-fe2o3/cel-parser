use std::{collections::HashMap, vec::IntoIter};

use crate::stack::{self, Seg, Value};
use cel_rs::{DynSegment, Segment};
use itertools::{Itertools, MultiPeek};

use crate::lexer::{LexError, Token, TokenSpan};

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
    pub seg: Seg,
}

impl Parser {
    pub fn new(input: Vec<TokenSpan>) -> Self {
        let stream = input.into_iter().multipeek();
        Self {
            tokens: stream,
            line: 1,
            column: 1,
            seg: Seg::new(Vec::new()),
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
    pub fn parse_expression(&mut self) -> Result<(), ParseError> {
        println!("parse_expression: {:?}", self.seg.stack);
        self.parse_or_expression()?;

        if self.match_token(Token::QuestionMark)? {
            let Value::Boolean(b) = self.seg.pop() else {
                return Err(ParseError::Other(
                    "not boolean".into(),
                    self.line,
                    self.column,
                ));
            };
            if b {
                self.parse_expression()?;
                self.expect_token(Token::Colon)?;
                self.parse_expression()?;
                self.seg.pop();
            } else {
                while self.peek()?.value != Token::Colon {
                    self.next();
                }
                self.expect_token(Token::Colon)?;
                self.parse_expression()?;
            }
        }
        Ok(())
    }

    // or_expression = and_expression { "||" and_expression }.
    fn parse_or_expression(&mut self) -> Result<(), ParseError> {
        println!("parse_or_expression: {:?}", self.seg.stack);
        self.parse_and_expression()?;

        while self.match_token(Token::Or)? {
            self.parse_and_expression()?;
            self.seg.callfn("or".into(), 2);
        }
        Ok(())
    }

    // and_expression = bitwise_or_expression { "&&" bitwise_or_expression }.
    fn parse_and_expression(&mut self) -> Result<(), ParseError> {
        println!("parse_and_expression: {:?}", self.seg.stack);
        self.parse_bitwise_or_expression()?;
        while self.match_token(Token::And)? {
            self.parse_bitwise_or_expression()?;
            self.seg.callfn("bitand".into(), 2);
        }
        Ok(())
    }

    // bitwise_or_expression = bitwise_xor_expression { "|" bitwise_xor_expression }.
    fn parse_bitwise_or_expression(&mut self) -> Result<(), ParseError> {
        println!("parse_bitwise_or_expression: {:?}", self.seg.stack);
        self.parse_bitwise_xor_expression()?;
        while self.match_token(Token::BitwiseOr)? {
            self.parse_bitwise_xor_expression()?;
            self.seg.callfn("bitor".into(), 2);
        }
        Ok(())
    }

    // bitwise_xor_expression = bitwise_and_expression { "^" bitwise_and_expression }.
    fn parse_bitwise_xor_expression(&mut self) -> Result<(), ParseError> {
        println!("parse_bitwise_xor_expression: {:?}", self.seg.stack);
        self.parse_bitwise_and_expression()?;
        while self.match_token(Token::BitwiseXor)? {
            self.parse_bitwise_and_expression()?;
            self.seg.callfn("bitxor".into(), 2);
        }
        Ok(())
    }

    // bitwise_and_expression = equality_expression { "&" equality_expression }.
    fn parse_bitwise_and_expression(&mut self) -> Result<(), ParseError> {
        println!("parse_bitwise_and_expression: {:?}", self.seg.stack);
        self.parse_equality_expression()?;
        while self.match_token(Token::BitwiseAnd)? {
            self.parse_equality_expression()?;
            self.seg.callfn("bitand".into(), 2);
        }
        Ok(())
    }

    // equality_expression = relational_expression { ("==" | "!=") relational_expression }.
    fn parse_equality_expression(&mut self) -> Result<(), ParseError> {
        println!("parse_equality_expression: {:?}", self.seg.stack);
        self.parse_relational_expression()?;
        loop {
            let current_op = self.peek()?.value;
            match current_op {
                Token::Equality => {
                    self.next()?;
                    self.parse_relational_expression()?;
                    self.seg.callfn("eq".into(), 2);
                }
                Token::NotEqual => {
                    self.next()?;
                    self.parse_relational_expression()?;
                    self.seg.callfn("neq".into(), 2);
                }
                _ => {
                    break;
                }
            }
        }
        Ok(())
    }

    // relational_expression = bitshift_expression { ("<" | ">" | "<=" | ">=") bitshift_expression }.
    fn parse_relational_expression(&mut self) -> Result<(), ParseError> {
        println!("parse_relational_expression: {:?}", self.seg.stack);
        self.parse_bitshift_expression()?;
        loop {
            let current_op = self.peek()?.value;
            match current_op {
                Token::LessThan => {
                    self.next()?;
                    self.parse_bitshift_expression()?;
                    self.seg.callfn("lt".into(), 2);
                }
                Token::GreaterThan => {
                    self.next()?;
                    self.parse_bitshift_expression()?;
                    self.seg.callfn("gt".into(), 2);
                }
                Token::LessThanOrEqual => {
                    self.next()?;
                    self.parse_bitshift_expression()?;
                    self.seg.callfn("lte".into(), 2);
                }
                Token::GreaterThanOrEqual => {
                    self.next()?;
                    self.parse_bitshift_expression()?;
                    self.seg.callfn("gte".into(), 2);
                }
                _ => {
                    break;
                }
            }
        }
        Ok(())
    }

    // bitshift_expression = additive_expression { ("<<" | ">>") additive_expression }.
    fn parse_bitshift_expression(&mut self) -> Result<(), ParseError> {
        println!("parse_bitshift_expression: {:?}", self.seg.stack);
        self.parse_additive_expression()?;
        loop {
            let current_op = self.peek()?.value;
            match current_op {
                Token::LeftShift => {
                    self.next()?;
                    self.parse_additive_expression()?;
                    //do stuff
                }
                Token::RightShift => {
                    self.next()?;
                    self.parse_additive_expression()?;
                    // do stuff
                }
                _ => {
                    break;
                }
            }
        }
        Ok(())
    }

    // additive_expression = multiplicative_expression { ("+" | "-") multiplicative_expression }.
    fn parse_additive_expression(&mut self) -> Result<(), ParseError> {
        println!("parse_additive_expression: {:?}", self.seg.stack);
        self.parse_multiplicative_expression()?;

        loop {
            let current_op = self.peek()?.value;
            // if current_op == Token::Plus || current_op == Token::Minus {
            match current_op {
                Token::Plus => {
                    println!("add found");
                    self.next()?;
                    self.parse_multiplicative_expression()?;
                    self.seg.callfn("add".into(), 2);
                }
                Token::Minus => {
                    self.next()?;
                    self.parse_multiplicative_expression()?;
                    self.seg.callfn("sub".into(), 2);
                }
                _ => {
                    break;
                }
            }
        }
        Ok(())
    }

    // multiplicative_expression = unary_expression { ("*" | "/" | "%") unary_expression }.
    // TODO: handle options here
    fn parse_multiplicative_expression(&mut self) -> Result<(), ParseError> {
        println!("parse_multiplicative_expression: {:?}", self.seg.stack);
        self.parse_unary_expression()?;
        loop {
            let current_op = self.peek()?.value;
            match current_op {
                Token::Multiply => {
                    println!("multiply found");
                    self.next()?;
                    self.parse_unary_expression()?;
                    self.seg.callfn("mult".into(), 2);
                }
                Token::Divide => {
                    self.next()?;
                    self.parse_unary_expression()?;
                    self.seg.callfn("div".into(), 2);
                }
                Token::Modulus => {
                    self.next()?;
                    self.parse_unary_expression()?;
                    self.seg.callfn("mod".into(), 2);
                }
                _ => {
                    break;
                }
            }
        }
        Ok(())
    }

    // unary_expression = postfix_expression | (unary_operator unary_expression).
    fn parse_unary_expression(&mut self) -> Result<(), ParseError> {
        println!("parse_unary_expression: {:?}", self.seg.stack);
        let token = self.peek()?.value;
        match token {
            Token::Plus => {
                self.next()?; // Consume the unary operator
                // self.parse_unary_expression()?;
                self.parse_postfix_expression()?;
            }
            Token::Minus => {
                self.next()?;
                // self.parse_unary_expression()?;
                self.parse_postfix_expression()?;
                self.seg.callfn("neg".into(), 1);
            }
            Token::Not => {
                self.next()?;
                // self.parse_unary_expression()?;
                self.parse_postfix_expression()?;
                self.seg.callfn("not".into(), 1);
            }
            Token::BitwiseNot => {
                self.next()?;
                // self.parse_unary_expression()?;
                self.parse_postfix_expression()?;
                self.seg.callfn("bitnot".into(), 1);
            }
            _ => {
                self.parse_postfix_expression()?;
            }
        }
        Ok(())
    }

    // postfix_expression = primary_expression { ("[" expression "]") | ("." identifier) }.
    fn parse_postfix_expression(&mut self) -> Result<(), ParseError> {
        println!("parse_postfix_expression: {:?}", self.seg.stack);
        self.parse_primary_expression()?;

        loop {
            match self.peek()?.value {
                Token::OpenBracket => {
                    // self.parse_expression()?;
                    // self.expect_token(Token::CloseBracket)?;
                    // let index = self.seg.pop();
                    // let Value::Num(n) = index else {
                    //     return Err(ParseError::Other(
                    //         format!("array access expr with non-number: {index:?}"),
                    //         self.line,
                    //         self.column,
                    //     ));
                    // };
                    // let array = self.seg.pop();
                    // let Value::Vec(v) = &array else {
                    //     return Err(ParseError::Other(
                    //         format!("array access expr on non-array: {array:?}"),
                    //         self.line,
                    //         self.column,
                    //     ));
                    // };
                    // // TODO: decide what we are doing here. clone value, put array back and then
                    // // value on top for now
                    // let Some(val) = v.get(n as usize) else {
                    //     return Err(ParseError::Other(
                    //         format!("Array index out of bounds: {n}"),
                    //         self.line,
                    //         self.column,
                    //     ));
                    // };
                    // let cv = val.clone();
                    // self.seg.push(array);
                    // self.seg.push(cv);
                }
                Token::Dot => {}
                _ => break,
            }
        }
        Ok(())
    }

    // primary_expression = name | number | boolean | string | "empty" | array | dictionary
    //      | variable_or_function | ( "(" expression ")" ).
    fn parse_primary_expression(&mut self) -> Result<(), ParseError> {
        println!("parse_primary_expression: {:?}", self.seg.stack);
        let peekts = self.peek()?;
        let peek = peekts.value.clone();
        match peek {
            Token::At => {
                self.parse_name_expression()?;
            }
            Token::Number(n) => {
                self.next()?;
                self.seg.push(n.into());
            }
            Token::String(s) => {
                self.next()?;
                self.seg.push(s.into());
            }
            Token::True => {
                self.next()?;
                self.seg.push(true.into());
            }
            Token::False => {
                self.next()?;
                self.seg.push(false.into());
            }
            // Token::Empty => {
            //     self.next()?;
            //     //TODO: fix later
            //     let pc = peek.clone();
            //     self.seg.stack.push(peek.clone());
            //     Ok(Expression::Literal(pc))
            // }
            Token::OpenBracket => {
                self.parse_array_literal()?;
            }
            Token::OpenBrace => {
                self.parse_dictionary_literal()?;
            }
            Token::Identifier(_) => {
                self.parse_variable_or_function()?;
            }
            Token::OpenParen => {
                self.next()?; // Consume '('
                self.parse_expression()?;
                self.expect_token(Token::CloseParen)?;
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: Some(
                        "primary expression type (@, literal, [, {, identifier, (".to_string(),
                    ),
                    found: peek,
                    line: peekts.span.line,
                    column: peekts.span.column,
                });
            }
        }
        Ok(())
    }

    //TODO: enable
    // name = "@" (identifier | keyword).
    fn parse_name_expression(&mut self) -> Result<(), ParseError> {
        println!("parse_name_expression: {:?}", self.seg.stack);
        self.expect_token(Token::At)?; // Consume '@'
        let name_token = self.next()?;
        // match name_token.value {
        //     Token::Identifier(id) => Ok(Expression::Literal(Token::Name(id))), // Or a specific Expression::Name variant
        //     Token::Keyword(kw) => {
        //         // The C++ parser allows @keyword. We represent this as Token::Name containing the keyword string.
        //         Ok(Expression::Literal(Token::Name(kw)))
        //     }
        //     // Specific keywords like true/false/empty if they were lexed as such and allowed after @
        //     Token::True => Ok(Expression::Literal(Token::Name("true".into()))),
        //     Token::False => Ok(Expression::Literal(Token::Name("false".into()))),
        //     Token::Empty => Ok(Expression::Literal(Token::Name("empty".to_string()))),
        //     _ => Err(ParseError::UnexpectedToken {
        //         expected: Some("Identifier or Keyword after '@'".to_string()),
        //         found: name_token.value,
        //         line: name_token.span.line,
        //         column: name_token.span.column,
        //     }),
        // }
        Ok(())
    }

    // variable_or_function = identifier ["(" [argument_expression_list] ")"].
    fn parse_variable_or_function(&mut self) -> Result<(), ParseError> {
        println!("parse_variable_or_function: {:?}", self.seg.stack);
        let tokenspan = self.peek()?; // Peek, do not consume yet.
        let name = match tokenspan.value {
            Token::Identifier(n) => n,
            _ => {
                // This function should only be called if an identifier is expected/peeked.
                // If called from primary_expression, primary_expression should ensure current token is Identifier.
                return Err(ParseError::UnexpectedToken {
                    expected: Some("Identifier for variable or function".to_string()),
                    found: tokenspan.value,
                    line: tokenspan.span.line,
                    column: tokenspan.span.column,
                });
            }
        };
        self.next()?; // Now consume the identifier

        if self.match_token(Token::OpenParen)? {
            // Function call
            self.parse_argument_expression_list(name)?;
            self.expect_token(Token::CloseParen)?;
        } else {
            // Variable
            // TODO: get var value
            // Ok(Expression::Identifier(name)) // Or Expression::Variable(name)
        }
        Ok(())
    }

    // argument_expression_list = named_argument_list | argument_list.
    fn parse_argument_expression_list(&mut self, fname: String) -> Result<(), ParseError> {
        println!("parse_argument_expression_list: {:?}", self.seg.stack);
        let peek = self.peek()?;
        // TODO: named args parsing
        // Tentative parsing: Peek for `identifier :` sequence for named arguments.
        // let lookahead2 = (self.peek_multi()?.value, self.peek_multi()?.value);
        // if let (Token::Identifier(_), Token::Colon) = lookahead2 {
        //     self.parse_named_argument_list()
        //         .map(|args| (Vec::new(), args))
        // } else {
        let args = self.parse_argument_list()?;
        self.seg.callfn(fname, args);
        // }
        Ok(())
    }

    // argument_list = expression { "," expression }.
    fn parse_argument_list(&mut self) -> Result<u8, ParseError> {
        println!("parse_argument_list: {:?}", self.seg.stack);
        println!("arg_list");
        let mut args = 0;
        // If the list can be empty and this is called, it implies an issue.
        // `parse_argument_expression_list` should handle the empty case before calling this.
        let current = self.peek()?.value;
        if current == Token::CloseBracket || current == Token::CloseParen {
            return Ok(args);
        }

        self.parse_expression()?;
        args += 1;
        while self.match_token(Token::Comma)? {
            self.parse_expression()?;
            args += 1;
        }
        Ok(args)
    }

    // named_argument_list = named_argument { "," named_argument }.
    fn parse_named_argument_list(&mut self) -> Result<HashMap<String, Value>, ParseError> {
        let mut hm = HashMap::new();
        let peek = self.peek()?.value;
        if peek == Token::CloseBracket || peek == Token::CloseParen || peek == Token::CloseBrace {
            return Ok(hm);
        }
        loop {
            let kv = self.parse_named_argument()?;
            hm.insert(kv.0, kv.1);
            if !self.match_token(Token::Comma)? {
                break;
            }
        }
        Ok(hm)
    }

    // named_argument = identifier ":" expression.
    fn parse_named_argument(&mut self) -> Result<(String, Value), ParseError> {
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
        self.parse_expression()?;
        Ok((name, self.seg.pop()))
    }

    // array = "[" [argument_list] "]".
    // TODO: Improve
    fn parse_array_literal(&mut self) -> Result<(), ParseError> {
        println!("parse_array_literal: {:?}", self.seg.stack);
        println!("array_literal");
        self.expect_token(Token::OpenBracket)?;
        let len = self.parse_argument_list()?;
        self.expect_token(Token::CloseBracket)?;
        // TODO: find a better way to do this. dumb.
        let mut array = (0..len).map(|_| self.seg.pop()).collect_vec();
        array.reverse();
        self.seg.push(array.into());
        Ok(())
    }

    // dictionary = "{" [named_argument_list] "}".
    // TODO: Enable
    fn parse_dictionary_literal(&mut self) -> Result<(), ParseError> {
        println!("parse_dictionary_literal: {:?}", self.seg.stack);
        println!("dict_literal");
        self.expect_token(Token::OpenBrace)?;
        let members = self.parse_named_argument_list()?;
        self.expect_token(Token::CloseBrace)?;
        self.seg.push(members.into());
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::lexer::Lexer;

    use super::*;

    // Helper to run lexer and parser for tests
    fn parse_str(input: &str) -> (Parser, Result<(), ParseError>) {
        let mut lexer = Lexer::new(input);
        lexer.lex().unwrap();
        let mut parser = Parser::new(lexer.tokens);
        let r = parser.parse_expression();
        (parser, r)
    }

    #[test]
    fn test_simple_number() {
        let (mut p, e) = parse_str("123");
        assert!(e.is_ok());
        assert_eq!(p.seg.pop(), Value::Num(123.))
    }

    #[test]
    fn test_simple_string() {
        let (mut p, e) = parse_str("\"hello\"");
        assert!(e.is_ok());
        assert_eq!(p.seg.pop(), Value::String("hello".into()));
    }

    #[test]
    fn test_boolean_true() {
        let (mut p, e) = parse_str("true");
        assert!(e.is_ok());
        assert_eq!(p.seg.pop(), Value::Boolean(true));
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
        let (mut p, e) = parse_str("false");
        assert!(e.is_ok());
        assert_eq!(p.seg.pop(), Value::Boolean(false));
    }

    // #[test]
    // fn test_stuff() {
    //     assert_eq!(
    //         parse_str("5 > 3 ? x = 2 : x = 3 "),
    //         Ok(Expression::Literal(Token::False))
    //     );
    // }

    // #[test]
    // fn test_empty_keyword() {
    //     assert_eq!(parse_str("empty"), Ok(Expression::Literal(Token::Empty)));
    // }

    #[test]
    fn test_parenthesized_expression() {
        let (mut p, e) = parse_str("(123)");
        assert!(e.is_ok());
        assert_eq!(p.seg.pop(), Value::Num(123.));
    }

    // #[test]
    // fn test_name_expression() {
    //     assert_eq!(
    //         parse_str("@my_var"),
    //         Ok(Expression::Literal(Token::Name("my_var".to_string())))
    //     );
    // }

    // #[test]
    // fn test_name_with_keyword() {
    //     assert_eq!(
    //         parse_str("@true"),
    //         Ok(Expression::Literal(Token::Name("true".to_string())))
    //     );
    // }

    // #[test]
    // fn test_simple_variable() {
    //     let mut p = parse_str("myVar");
    //     println!("{:?}", p.seg.stack);
    //     assert_eq!(p.seg.pop(), Value::String("myVar".into()));
    // }

    #[test]
    fn test_simple_function_call_no_args() {
        let mut l = Lexer::new("myFunc()");
        l.lex().unwrap();
        let mut p = Parser::new(l.tokens);
        p.seg.register0("myFunc", || Value::Num(500.));
        p.parse_expression().unwrap();
        println!("WTF: {:?}", p.seg.stack);
        assert_eq!(p.seg.pop(), Value::Num(500.))
    }

    #[test]
    fn test_function_call_one_arg() {
        let (mut p, e) = parse_str("neg(5)");
        assert!(e.is_ok());
        assert_eq!(p.seg.pop(), Value::Num(-5.))
    }

    #[test]
    fn test_function_call_multiple_args() {
        let (mut p, e) = parse_str("add(\"This is a \", \"test\")");
        assert!(e.is_ok());
        assert_eq!(p.seg.pop(), Value::String("This is a test".into()));
    }

    // #[test]
    // fn test_function_call_one_named_arg() {
    //     assert_eq!(
    //         parse_str("config(mode: \"fast\")"),
    //         Ok(Expression::FunctionCall {
    //             name: "config".to_string(),
    //             arguments: vec![], // No positional if only named were parsed this way
    //             named_arguments: vec![(
    //                 "mode".to_string(),
    //                 Expression::Literal(Token::String("fast".to_string()))
    //             )]
    //         })
    //     );
    // }

    // #[test]
    // fn test_function_call_mixed_args_fail_current_logic() {
    //     // Current parse_argument_expression_list commits to either full positional or full named.
    //     // This test should fail as `num:1` would be seen, then `2` would be an error.
    //     let result = parse_str("setup(num:1, 2)");
    //     assert!(result.is_err());
    // }

    #[test]
    fn test_empty_array_literal() {
        let (mut p, e) = parse_str("[]");
        assert!(e.is_ok());
        assert_eq!(p.seg.pop(), Value::Vec(vec![]));
    }

    #[test]
    fn test_array_literal_one_element() {
        let (mut p, e) = parse_str("[123]");
        assert!(e.is_ok());
        assert_eq!(p.seg.pop(), Value::Vec(vec![Value::Num(123.)]));
    }

    #[test]
    fn test_array_literal_multiple_elements() {
        let (mut p, e) = parse_str("[1, \"two\"]");
        assert!(e.is_ok());
        assert_eq!(
            p.seg.pop(),
            Value::Vec(vec![Value::Num(1.), Value::String("two".into())])
        );
    }

    // #[test]
    // fn test_empty_dictionary_literal() {
    //     assert_eq!(parse_str("{}"), Ok(Expression::DictionaryLiteral(vec![])));
    // }

    // #[test]
    // fn test_dictionary_literal_one_member() {
    //     assert_eq!(
    //         parse_str("{key: \"value\"}"),
    //         Ok(Expression::DictionaryLiteral(vec![(
    //             "key".to_string(),
    //             Expression::Literal(Token::String("value".to_string()))
    //         )]))
    //     );
    // }

    #[test]
    fn test_dictionary_literal_multiple_members() {
        let (mut p, e) = parse_str("{a: 1, b: true}");
        assert!(e.is_ok());
        assert_eq!(
            p.seg.pop(),
            Value::Dict(HashMap::from([
                ("a".into(), Value::Num(1.)),
                ("b".into(), Value::Boolean(true))
            ]))
        );
    }

    #[test]
    fn test_addition() {
        let (mut p, e) = parse_str("1 + 2");
        assert!(e.is_ok());
        println!("{:?}", p.seg.stack);
        assert_eq!(p.seg.pop(), Value::Num(3.));
    }

    #[test]
    fn test_ternary_operator() {
        let (mut p, e) = parse_str("true ? 1 : 2");
        assert!(e.is_ok());
        assert_eq!(p.seg.pop(), Value::Num(1.));
    }

    #[test]
    fn test_unary_minus() {
        let (mut p, e) = parse_str("-5");
        assert!(e.is_ok());
        assert_eq!(p.seg.pop(), Value::Num(-5.));
    }

    #[test]
    fn test_unary_not() {
        let (mut p, e) = parse_str("!true");
        assert!(e.is_ok());
        assert_eq!(p.seg.pop(), Value::Boolean(false));
    }

    //TODO: why should this be an error in a stack based language
    // #[test]
    // fn test_incomplete_expression() {
    //     let (mut p, e) = parse_str("10 + 25 25");
    //     assert!(e.is_err());
    //     println!("{e:?}");
    //     // assert_eq!(
    //     //     e,
    //     //     vec![Value::String(
    //     //         "compile_error ! (\"Unexpected token\")".into()
    //     //     )]
    //     // );
    // }

    #[test]
    fn test_arithmetic_expression() {
        let (mut p, e) = parse_str("10 + 20 * 30");
        assert!(e.is_ok());
        assert_eq!(p.seg.pop(), Value::Num(610.));
    }

    #[test]
    fn test_parenthesized_expression_2() {
        let (mut p, e) = parse_str("(10 + 20) * 30");
        assert!(e.is_ok());
        assert_eq!(p.seg.pop(), Value::Num(900.))
    }

    #[test]
    fn test_complex_expression() {
        let (mut p, e) = parse_str("10 + 20 * (30 - 5) / 2");
        assert!(e.is_ok());
        assert_eq!(p.seg.pop(), Value::Num(260.))
    }

    // TODO: how are we resolving all of these
    // #[test]
    // fn test_logical_expression() {
    //     let mut p = parse_str("a && b || c");
    //     assert!(true);
    // }
    //
    // #[test]
    // fn test_comparison_expression() {
    //     let input = TokenStream::from_str("a == b && c > d").unwrap();
    //     let mut parser = CELParser::new(input.into_iter());
    //     assert!(parser.is_expression());
    // }
    //
    // #[test]
    // fn test_bitwise_expression() {
    //     let input = TokenStream::from_str("a | b & c ^ d").unwrap();
    //     let mut parser = CELParser::new(input.into_iter());
    //     assert!(parser.is_expression());
    // }
    //
    // #[test]
    // fn test_shift_expression() {
    //     let input = TokenStream::from_str("a << 2 + b >> 1").unwrap();
    //     let mut parser = CELParser::new(input.into_iter());
    //     assert!(parser.is_expression());
    // }
    //
    // #[test]
    // fn test_unary_expression() {
    //     let input = TokenStream::from_str("-a + !b").unwrap();
    //     let mut parser = CELParser::new(input.into_iter());
    //     assert!(parser.is_expression());
    // }
    //
    // #[test]
    // fn test_chained_unary_expression() {
    //     let input = TokenStream::from_str("!!a + --b").unwrap();
    //     let mut parser = CELParser::new(input.into_iter());
    //     assert!(parser.is_expression());
    // }

    #[test]
    fn test_invalid_expression() {
        let (mut p, e) = parse_str("+");
        assert!(e.is_err());
        assert_eq!(
            e.unwrap_err(),
            ParseError::UnexpectedToken {
                expected: Some("primary expression type (@, literal, [, {, identifier, (".into()),
                found: Token::Eof,
                line: 1,
                column: 2
            }
        );
    }

    // #[test]
    // fn test_error_formatting() {
    //     let source = "10 + 20 30"; // Missing operator between 20 and 30
    //     let input = TokenStream::from_str(source).unwrap();
    //     let mut parser = CELParser::new(input.into_iter());
    //
    //     // This should fail parsing
    //     assert!(!parser.is_expression());
    //
    //     // Test error message extraction
    //     let error_msg = parser.extract_error_message();
    //     assert!(error_msg.is_some());
    //     assert_eq!(error_msg.unwrap(), "Unexpected token");
    //
    //     // Test error formatting
    //     let formatted_error = parser.format_error(source, "test.cel", 1u32);
    //     assert!(formatted_error.is_some());
    //
    //     let formatted = formatted_error.unwrap();
    //     assert!(formatted.contains("error: Unexpected token"));
    //     assert!(formatted.contains("test.cel:1:")); // Should include line number
    //     assert!(formatted.contains("1 | 10 + 20 30")); // Should show the line with line number
    //     assert!(formatted.contains("^")); // Should have carets pointing to the error
    // }

    // #[test]
    // fn test_error_formatting_with_line_offset() {
    //     let source = "a + b c"; // Missing operator between b and c
    //     let input = TokenStream::from_str(source).unwrap();
    //     let mut parser = CELParser::new(input.into_iter());
    //
    //     // This should fail parsing
    //     assert!(!parser.is_expression());
    //
    //     // Test error formatting with line offset (as if expression starts at line 42)
    //     let formatted_error = parser.format_error(source, "large_file.rs", 42u32);
    //     assert!(formatted_error.is_some());
    //
    //     let formatted = formatted_error.unwrap();
    //     assert!(formatted.contains("error: Unexpected token"));
    //     assert!(formatted.contains("large_file.rs:42:")); // Should show offset line number
    //     assert!(formatted.contains("42 | a + b c")); // Should show the line with offset line number
    //     assert!(formatted.contains("^")); // Should have carets pointing to the error
    // }

    // #[test]
    // fn print_error_formatting() {
    //     let line = line!() + 1;
    //     let source = r#"
    //         10 + 20 30 // Unexpected token
    //     "#;
    //     let input = TokenStream::from_str(source).unwrap();
    //     let mut parser = CELParser::new(input.into_iter());
    //
    //     if !parser.is_expression() {
    //         // Format error starting at line 1
    //         if let Some(formatted_error) = parser.format_error(source, file!(), line) {
    //             println!("{}", formatted_error);
    //             // error: Unexpected token
    //             // --> cel-parser/src/lib.rs:593:21
    //             //     |
    //             // 593 |             10 + 20 30 // Unexpected token
    //             //     |                     ^^
    //         }
    //     }
    // }

    // #[test]
    // fn test_postfix_array_access() {
    //     assert_eq!(parse_str("[1, 2, 3] myArray[0]"), Ok(expected));
    // }

    // #[test]
    // fn test_postfix_member_access() {
    //     let expected = Expression::MemberAccess {
    //         object: Box::new(Expression::Identifier("myObj".to_string())),
    //         member: "field".to_string(),
    //     };
    //     assert_eq!(parse_str("myObj.field"), Ok(expected));
    // }
}
