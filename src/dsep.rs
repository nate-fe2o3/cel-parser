use std::{
    collections::HashMap,
    ops::{BitAnd, BitOr, BitXor},
    vec::IntoIter,
};

use cel_rs::{DynSegment, Segment};
use itertools::{Itertools, MultiPeek};

use crate::{
    lexer::{LexError, Token, TokenSpan},
    stack::FnStack,
};

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
    pub stack: DynSegment,
    pub fns: FnStack,
}

impl Parser {
    pub fn new(input: Vec<TokenSpan>) -> Self {
        let stream = input.into_iter().multipeek();
        Self {
            tokens: stream,
            line: 1,
            column: 1,
            stack: DynSegment::new::<()>(),
            fns: FnStack::new(),
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
    // TODO: then and else having different types
    pub fn parse_expression<T: 'static>(&mut self) -> Result<(), ParseError> {
        let typ = self.parse_or_expression()?;

        if self.match_token(Token::QuestionMark)? {
            self.parse_expression::<T>()?;
            self.expect_token(Token::Colon)?;
            self.parse_expression::<T>()?;
            self.stack
                .op3(|conditional: bool, then: T, els: T| if conditional { then } else { els });
        }
        Ok(())
    }

    // or_expression = and_expression { "||" and_expression }.
    fn parse_or_expression(&mut self) -> Result<(), ParseError> {
        self.parse_and_expression()?;

        while self.match_token(Token::Or)? {
            self.parse_and_expression()?;
            self.stack.op2(|x: bool, y: bool| x || y);
        }
        Ok(())
    }

    // and_expression = bitwise_or_expression { "&&" bitwise_or_expression }.
    fn parse_and_expression(&mut self) -> Result<(), ParseError> {
        self.parse_bitwise_or_expression()?;
        while self.match_token(Token::And)? {
            self.parse_bitwise_or_expression()?;
            self.stack.op2(|x: bool, y: bool| x && y);
        }
        Ok(())
    }

    // bitwise_or_expression = bitwise_xor_expression { "|" bitwise_xor_expression }.
    fn parse_bitwise_or_expression(&mut self) -> Result<(), ParseError> {
        self.parse_bitwise_xor_expression()?;
        while self.match_token(Token::BitwiseOr)? {
            self.parse_bitwise_xor_expression()?;
            self.stack
                .op2(|x: f64, y: f64| ((x as i64) | (y as i64)) as f64);
        }
        Ok(())
    }

    // bitwise_xor_expression = bitwise_and_expression { "^" bitwise_and_expression }.
    fn parse_bitwise_xor_expression(&mut self) -> Result<(), ParseError> {
        self.parse_bitwise_and_expression()?;
        while self.match_token(Token::BitwiseXor)? {
            self.parse_bitwise_and_expression()?;
            self.stack
                .op2(|x: f64, y: f64| ((x as i64) ^ (y as i64)) as f64);
        }
        Ok(())
    }

    // bitwise_and_expression = equality_expression { "&" equality_expression }.
    fn parse_bitwise_and_expression(&mut self) -> Result<(), ParseError> {
        self.parse_equality_expression()?;
        while self.match_token(Token::BitwiseAnd)? {
            self.parse_equality_expression()?;
            self.stack
                .op2(|x: f64, y: f64| ((x as i64) & (y as i64)) as f64);
        }
        Ok(())
    }

    // equality_expression = relational_expression { ("==" | "!=") relational_expression }.
    fn parse_equality_expression<T: 'static + PartialEq>(&mut self) -> Result<(), ParseError> {
        self.parse_relational_expression()?;
        loop {
            let current_op = self.peek()?.value;
            match current_op {
                Token::Equality => {
                    self.next()?;
                    self.parse_relational_expression()?;
                    self.stack.op2(|x: T, y: T| x == y);
                }
                Token::NotEqual => {
                    self.next()?;
                    self.parse_relational_expression()?;
                    self.stack.op2(|x: T, y: T| x != y);
                }
                _ => {
                    break;
                }
            }
        }
        Ok(())
    }

    // relational_expression = bitshift_expression { ("<" | ">" | "<=" | ">=") bitshift_expression }.
    fn parse_relational_expression<T: 'static + PartialOrd>(&mut self) -> Result<(), ParseError> {
        self.parse_bitshift_expression()?;
        loop {
            let current_op = self.peek()?.value;
            match current_op {
                Token::LessThan => {
                    self.next()?;
                    self.parse_bitshift_expression()?;
                    self.stack.op2(|x: T, y: T| x < y);
                }
                Token::GreaterThan => {
                    self.next()?;
                    self.parse_bitshift_expression()?;
                    self.stack.op2(|x: T, y: T| x > y);
                }
                Token::LessThanOrEqual => {
                    self.next()?;
                    self.parse_bitshift_expression()?;
                    self.stack.op2(|x: T, y: T| x <= y);
                }
                Token::GreaterThanOrEqual => {
                    self.next()?;
                    self.parse_bitshift_expression()?;
                    self.stack.op2(|x: T, y: T| x >= y);
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
        self.parse_additive_expression()?;
        loop {
            let current_op = self.peek()?.value;
            match current_op {
                Token::LeftShift => {
                    self.next()?;
                    self.parse_additive_expression()?;
                    self.stack
                        .op2(|x: f64, y: f64| ((x as i64) << (y as i64)) as f64);
                }
                Token::RightShift => {
                    self.next()?;
                    self.parse_additive_expression()?;
                    self.stack
                        .op2(|x: f64, y: f64| ((x as i64) >> (y as i64)) as f64);
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
        self.parse_multiplicative_expression()?;

        loop {
            let current_op = self.peek()?.value;
            // if current_op == Token::Plus || current_op == Token::Minus {
            match current_op {
                Token::Plus => {
                    self.next()?;
                    self.parse_multiplicative_expression()?;
                    self.stack.op2(|x: f64, y: f64| x + y);
                }
                Token::Minus => {
                    self.next()?;
                    self.parse_multiplicative_expression()?;
                    self.stack.op2(|x: f64, y: f64| x - y);
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
        self.parse_unary_expression()?;
        loop {
            let current_op = self.peek()?.value;
            match current_op {
                Token::Multiply => {
                    self.next()?;
                    self.parse_unary_expression()?;
                    self.stack.op2(|x: f64, y: f64| x * y);
                }
                Token::Divide => {
                    self.next()?;
                    self.parse_unary_expression()?;
                    self.stack.op2(|x: f64, y: f64| x / y);
                }
                Token::Modulus => {
                    self.next()?;
                    self.parse_unary_expression()?;
                    self.stack.op2(|x: f64, y: f64| x % y);
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
                self.stack.op1(|x: f64| -x);
            }
            Token::Not => {
                self.next()?;
                // self.parse_unary_expression()?;
                self.parse_postfix_expression()?;
                self.stack.op1(|x: bool| !x);
            }
            Token::BitwiseNot => {
                self.next()?;
                // self.parse_unary_expression()?;
                self.parse_postfix_expression()?;
                self.stack.op1(|x: f64| !(x as i64) as f64);
            }
            _ => {
                self.parse_postfix_expression()?;
            }
        }
        Ok(())
    }

    // postfix_expression = primary_expression { ("[" expression "]") | ("." identifier) }.
    fn parse_postfix_expression(&mut self) -> Result<(), ParseError> {
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
        let peekts = self.peek()?;
        let peek = peekts.value.clone();
        match peek {
            Token::At => {
                self.parse_name_expression()?;
            }
            Token::Number(n) => {
                self.next()?;
                self.stack.op0(|| n);
            }
            Token::String(s) => {
                self.next()?;
                self.stack.op0(|| s);
            }
            Token::True => {
                self.next()?;
                self.stack.op0(|| true);
            }
            Token::False => {
                self.next()?;
                self.stack.op0(|| false);
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
    fn parse_str(input: &str) -> Parser {
        let mut lexer = Lexer::new(input);
        lexer.lex().unwrap();
        let mut parser = Parser::new(lexer.tokens);
        parser.parse_expression().unwrap();
        parser
    }

    #[test]
    fn test_simple_number() {
        assert_eq!(parse_str("123").seg.pop(), Value::Num(123.))
    }

    #[test]
    fn test_simple_string() {
        assert_eq!(
            parse_str("\"hello\"").seg.pop(),
            Value::String("hello".into())
        );
    }

    #[test]
    fn test_boolean_true() {
        assert_eq!(parse_str("true").seg.pop(), Value::Boolean(true));
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
        assert_eq!(parse_str("false").seg.pop(), Value::Boolean(false));
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
        assert_eq!(parse_str("(123)").seg.pop(), Value::Num(123.));
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
        assert_eq!(parse_str("neg(5)").seg.pop(), Value::Num(-5.))
    }

    #[test]
    fn test_function_call_multiple_args() {
        assert_eq!(
            parse_str("add(\"This is a \", \"test\")").seg.pop(),
            Value::String("This is a test".into())
        );
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
        assert_eq!(parse_str("[]").seg.pop(), Value::Vec(vec![]));
    }

    #[test]
    fn test_array_literal_one_element() {
        assert_eq!(
            parse_str("[123]").seg.pop(),
            Value::Vec(vec![Value::Num(123.)])
        );
    }

    #[test]
    fn test_array_literal_multiple_elements() {
        assert_eq!(
            parse_str("[1, \"two\"]").seg.pop(),
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
        assert_eq!(
            parse_str("{a: 1, b: true}").seg.pop(),
            Value::Dict(HashMap::from([
                ("a".into(), Value::Num(1.)),
                ("b".into(), Value::Boolean(true))
            ]))
        );
    }

    #[test]
    fn test_addition() {
        let mut p = parse_str("1 + 2");
        println!("{:?}", p.seg.stack);
        assert_eq!(p.seg.pop(), Value::Num(3.));
    }

    #[test]
    fn test_ternary_operator() {
        assert_eq!(parse_str("true ? 1 : 2").seg.pop(), Value::Num(1.));
    }

    #[test]
    fn test_unary_minus() {
        assert_eq!(parse_str("-5").seg.pop(), Value::Num(-5.));
    }

    #[test]
    fn test_unary_not() {
        assert_eq!(parse_str("!true").seg.pop(), Value::Boolean(false));
    }

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
