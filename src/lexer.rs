use std::{iter::Peekable, str::Chars};

const NEWLINES: [char; 2] = ['\n', '\r'];

pub(crate) struct Lexer<'a> {
    pub tokens: Vec<TokenSpan>,
    input: Peekable<Chars<'a>>,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let stream = input.chars().peekable();
        Self {
            tokens: vec![],
            input: stream,
            line: 1,
            column: 1,
        }
    }

    fn peek(&mut self) -> Result<char, LexError> {
        let c = self.input.peek().ok_or(LexError::UnexpectedEof {
            line: self.line,
            column: self.column,
        })?;
        Ok(*c)
    }

    fn next(&mut self) -> Result<char, LexError> {
        let c = self.input.next().ok_or(LexError::UnexpectedEof {
            line: self.line,
            column: self.column,
        })?;
        if NEWLINES.contains(&c) {
            self.column = 1;
            self.line += 1;
        } else {
            self.column += 1;
        }
        Ok(c)
    }

    fn match_char(&mut self, expected: char) -> Result<bool, LexError> {
        let c = self.input.peek().ok_or(LexError::UnexpectedEof {
            line: self.line,
            column: self.column,
        })?;
        if expected == *c {
            self.next()?;
            return Ok(true);
        }
        Ok(false)
    }

    fn expect_char(&mut self, expected: char) -> Result<char, LexError> {
        let c = self.input.peek().ok_or(LexError::UnexpectedEof {
            line: self.line,
            column: self.column,
        })?;
        if expected == *c {
            self.next()
        } else {
            Err(LexError::UnexpectedChar {
                expected,
                found: *c,
                line: self.line,
                column: self.column,
            })
        }
    }

    fn push_token(&mut self, t: Token) {
        let span: Span = Span {
            line: self.line,
            column: self.column,
        };
        self.tokens.push(TokenSpan { value: t, span });
    }

    pub fn lex(&mut self) -> Result<(), LexError> {
        while let Ok(c) = self.next() {
            match c {
                // explicit whitespace arm, do something with it later?
                sp if c.is_ascii_whitespace() => {}
                '+' => self.push_token(Token::Plus),
                '-' => self.push_token(Token::Minus),
                '*' => self.push_token(Token::Multiply),
                '%' => self.push_token(Token::Modulus),
                '?' => self.push_token(Token::QuestionMark),
                ':' => self.push_token(Token::Colon),
                '{' => self.push_token(Token::OpenBrace),
                '}' => self.push_token(Token::CloseBrace),
                '(' => self.push_token(Token::OpenParen),
                ')' => self.push_token(Token::CloseParen),
                '[' => self.push_token(Token::OpenBracket),
                ']' => self.push_token(Token::CloseBracket),
                ';' => self.push_token(Token::Semicolon),
                ',' => self.push_token(Token::Comma),
                '.' => self.push_token(Token::Dot),
                '@' => self.push_token(Token::At),
                '^' => self.push_token(Token::BitwiseXor),
                '~' => self.push_token(Token::BitwiseNot),
                // ident
                n if (c.is_ascii_alphabetic() || c == '_') => {
                    let mut ident = String::from(n);
                    while let Ok(c) = self.peek() {
                        if c.is_ascii_alphanumeric() || c == '_' {
                            ident.push(self.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    // check keywords first, else generic identifier token
                    // TODO: Maybe have a keyword hashmap instantiated to just get by key and ident
                    // if fail
                    match ident.as_str() {
                        "empty" => self.push_token(Token::Empty),
                        "true" => self.push_token(Token::True),
                        "false" => self.push_token(Token::False),
                        _ => {
                            self.push_token(Token::Identifier(ident.clone()));
                        }
                    }
                }
                // string
                // TODO: check if multiline is acceptable
                n if c == '\'' || c == '\"' => {
                    let mut string = String::new();
                    loop {
                        if self.peek()? == n {
                            _ = self.next()?;
                            self.push_token(Token::String(string.clone()));
                            break;
                        }
                        string.push(self.next()?);
                    }
                }
                // number
                // TODO: hex and e numbers
                n if c.is_ascii_digit() => {
                    let mut number = String::from(n);
                    let mut after_decimal = false;
                    while let Ok(m) = self.peek() {
                        match m {
                            _o if m.is_ascii_digit() => {
                                number.push(self.next()?);
                            }
                            _p if m == '.' && !after_decimal => {
                                number.push(self.next()?);
                                after_decimal = true;
                            }
                            _ => {
                                break;
                            }
                        }
                    }
                    let num = number.parse::<f64>().map_err(|e| LexError::LexerError {
                        err: e.to_string(),
                        line: self.line,
                        column: self.column,
                    })?;
                    self.push_token(Token::Number(num));
                }
                // equality | assignment
                '=' => match self.peek() {
                    Err(e) => break,
                    Ok(o) => {
                        if o == '=' {
                            self.next()?;
                            self.push_token(Token::Equality);
                        } else {
                            self.push_token(Token::Assign);
                        }
                    }
                },
                // Division | single line comment | multiline comment
                '/' => match self.peek() {
                    Err(e) => break,
                    Ok(o) => match o {
                        '/' => {
                            self.next()?;
                            while let Ok(c) = self.peek() {
                                self.next()?;
                                if NEWLINES.contains(&c) {
                                    break;
                                }
                            }
                        }
                        '*' => {
                            self.next()?;
                            while let Ok(c) = self.peek() {
                                if c == '*' {
                                    self.next()?;
                                    if let Ok(e) = self.peek() {
                                        if e == '/' {
                                            self.next()?;
                                            break;
                                        }
                                    }
                                }
                                self.next()?;
                            }
                        }
                        _ => {
                            self.push_token(Token::Divide);
                        }
                    },
                },
                // Not | not equal
                // NOTE: Cannot be EOF
                '!' => match self.peek()? {
                    '=' => {
                        self.next()?;
                        self.push_token(Token::NotEqual);
                    }
                    _ => {
                        self.push_token(Token::Not);
                    }
                },
                // Less than | less than or equal to
                // NOTE: Cannot be EOF
                '<' => match self.peek()? {
                    '=' => {
                        self.next()?;
                        self.push_token(Token::LessThanOrEqual);
                    }
                    '<' => {
                        self.next()?;
                        self.push_token(Token::LeftShift);
                    }
                    _ => {
                        self.push_token(Token::LessThan);
                    }
                },
                // Greater than | greater than or equal to
                // NOTE: Cannot be EOF
                '>' => match self.peek()? {
                    '=' => {
                        self.next()?;
                        self.push_token(Token::GreaterThanOrEqual);
                    }
                    '>' => {
                        self.next()?;
                        self.push_token(Token::RightShift);
                    }
                    _ => {
                        self.push_token(Token::GreaterThan);
                    }
                },
                // Or
                '|' => match self.peek()? {
                    '|' => {
                        self.next()?;
                        self.push_token(Token::Or);
                    }
                    _ => {
                        self.push_token(Token::BitwiseOr);
                    }
                },
                // And
                '&' => match self.peek()? {
                    '&' => {
                        self.next()?;
                        self.push_token(Token::And);
                    }
                    _ => {
                        self.push_token(Token::BitwiseAnd);
                    }
                },

                _ => panic!(),
            }
        }
        self.push_token(Token::Eof);
        Ok(())
    }
}

// pub enum Literal {
//     Number(f64),
//     String(String),
//     Boolean(bool),
// }

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Literals
    Number(f64),
    String(String),
    Identifier(String),
    // Needed? booleans are top level tokens
    Keyword(String), // e.g., 'true', 'false', 'empty'
    // TODO: implement. Do we need to?
    Name(String), // Represents '@' followed by identifier or keyword
    True,
    False,
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulus,
    Assign,
    Equality,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    And, // Logical AND (&&)
    Or,  // Logical OR (||)
    Not, // Logical NOT (!)
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot, // (~)

    LeftShift,
    RightShift,

    OpenParen,
    CloseParen,
    OpenBracket,  // [
    CloseBracket, // ]
    OpenBrace,    // {
    CloseBrace,   // }
    Comma,
    Colon,
    Semicolon,
    QuestionMark, // ?
    Dot,          // .
    At,           // @

    // Special
    // TODO:
    IfElse,     // Represents the ternary operator ? : as a single unit after parsing
    ArrayIndex, // Represents array indexing
    FunctionCall,
    Variable,

    Empty, // Represents the 'empty' keyword/value
    Eof,   // End of File/self.input
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenSpan {
    pub value: Token,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub line: usize,
    pub column: usize,
}

// Lexer Errors
#[derive(Debug, Clone, PartialEq)]
pub enum LexError {
    UnexpectedChar {
        expected: char, // Can be a specific token or a description like "expression"
        found: char,
        line: usize,
        column: usize,
    },
    UnexpectedEof {
        line: usize,
        column: usize,
    },
    LexerError {
        err: String,
        line: usize,
        column: usize,
    }, // For errors from the lexing phase
}

#[cfg(test)]
mod tests {
    use super::*;

    fn just_tokens(l: Lexer) -> Vec<Token> {
        l.tokens.into_iter().map(|x| x.value).collect()
    }

    #[test]
    fn single_ident() {
        let mut l = Lexer::new("var");
        l.lex().unwrap();
        assert_eq!(
            just_tokens(l),
            vec![Token::Identifier("var".into()), Token::Eof]
        );
    }

    #[test]
    fn ident_with_unders() {
        let mut l = Lexer::new("va_r");
        l.lex().unwrap();
        assert_eq!(
            just_tokens(l),
            vec![Token::Identifier("va_r".into()), Token::Eof]
        );
    }

    #[test]
    fn ident_with_numbers() {
        let mut l = Lexer::new("var5");
        l.lex().unwrap();
        assert_eq!(
            just_tokens(l),
            vec![Token::Identifier("var5".into()), Token::Eof]
        );
    }

    #[test]
    fn ident_with_space() {
        let mut l = Lexer::new("  var  ");
        l.lex().unwrap();
        assert_eq!(
            just_tokens(l),
            vec![Token::Identifier("var".into()), Token::Eof]
        );
    }
    #[test]
    fn variable_assignment_number() {
        let mut l = Lexer::new("let thing = 54.5");
        l.lex().unwrap();
        assert_eq!(
            just_tokens(l),
            vec![
                Token::Identifier("let".into()),
                Token::Identifier("thing".into()),
                Token::Assign,
                Token::Number(54.5),
                Token::Eof
            ]
        );
    }

    #[test]
    fn boolean_assignment() {
        let mut l = Lexer::new("thing = true");
        l.lex().unwrap();
        assert_eq!(
            just_tokens(l),
            vec![
                Token::Identifier("thing".into()),
                Token::Assign,
                Token::True,
                Token::Eof
            ]
        );
    }

    #[test]
    fn trail_comment() {
        let input = r#"
        thing = true // this is a comment, should not show up
        other = false
        "#;
        let mut l = Lexer::new(input);
        l.lex().unwrap();
        assert_eq!(
            just_tokens(l),
            vec![
                Token::Identifier("thing".into()),
                Token::Assign,
                Token::True,
                Token::Identifier("other".into()),
                Token::Assign,
                Token::False,
                Token::Eof
            ]
        );
    }

    #[test]
    fn lead_comment() {
        let input = r#"thing = true 
        /* Multiline comment, 12345!@#$%
        anyway still commenting
        */
        other = false
        "#;
        let mut l = Lexer::new(input);
        l.lex().unwrap();
        assert_eq!(
            just_tokens(l),
            vec![
                Token::Identifier("thing".into()),
                Token::Assign,
                Token::True,
                Token::Identifier("other".into()),
                Token::Assign,
                Token::False,
                Token::Eof
            ]
        );
    }

    #[test]
    fn compound_tokens() {
        let input = r#"
        4 <= < 5
        5 >= > 4
        1 ==  = 1
        10 != ! 9
        "#;
        let mut l = Lexer::new(input);
        l.lex().unwrap();
        assert_eq!(
            just_tokens(l),
            vec![
                Token::Number(4.0),
                Token::LessThanOrEqual,
                Token::LessThan,
                Token::Number(5.0),
                Token::Number(5.0),
                Token::GreaterThanOrEqual,
                Token::GreaterThan,
                Token::Number(4.0),
                Token::Number(1.0),
                Token::Equality,
                Token::Assign,
                Token::Number(1.0),
                Token::Number(10.0),
                Token::NotEqual,
                Token::Not,
                Token::Number(9.0),
                Token::Eof
            ]
        );
    }

    #[test]
    fn strings() {
        let input = r#"
        mystring = "hello there, 4 + 5"
        yourstring = 'anyway, did you know 5
        + 4 = 9?'
        "#;
        let mut l = Lexer::new(input);
        l.lex().unwrap();
        assert_eq!(
            just_tokens(l),
            vec![
                Token::Identifier("mystring".into()),
                Token::Assign,
                Token::String("hello there, 4 + 5".into()),
                Token::Identifier("yourstring".into()),
                Token::Assign,
                Token::String("anyway, did you know 5\n        + 4 = 9?".into()),
                Token::Eof,
            ]
        );
    }

    #[test]
    fn paren_list() {
        let input = r#"
        myTuple = (4, "hello", 54.4, "goodbye");
        "#;
        let mut l = Lexer::new(input);
        l.lex().unwrap();
        assert_eq!(
            just_tokens(l),
            vec![
                Token::Identifier("myTuple".into()),
                Token::Assign,
                Token::OpenParen,
                Token::Number(4.0),
                Token::Comma,
                Token::String("hello".into()),
                Token::Comma,
                Token::Number(54.4),
                Token::Comma,
                Token::String("goodbye".into()),
                Token::CloseParen,
                Token::Semicolon,
                Token::Eof
            ]
        );
    }
}
