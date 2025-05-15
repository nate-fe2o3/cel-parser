use crate::tokenizer::{Keyword, Literal, tokenize};
use crate::{builtins::*, tokenizer::Token};
use std::vec::IntoIter;
use std::{any::TypeId, cell::LazyCell, collections::HashMap, iter::Peekable};

use cel_rs::{
    dyn_segment::DynSegment,
    segment::{Callable, Segment},
    type_list::{IntoList, List},
};

pub struct Parser {
    input: Peekable<IntoIter<Token>>,
    stack: DynSegment,
}

enum ParseLevel {
    Base,
    Or,
    And,
    Eq,
    Rel,
    Add,
    Mult,
    Unary,
    Postfix,
    Primary,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        let (leftovers, tokens) = tokenize(input).expect("tokenizer to work");
        Self {
            input: tokens.into_iter().peekable(),
            stack: DynSegment::new::<()>(),
        }
    }

    pub fn run(&mut self) {}

    fn exp(&mut self) {
        self.or();
        // rhs
    }
    fn exp_prime(&mut self) {
        let Some(n) = self.input.peek() else {
            return;
        };
        if n == Token::Literal(Literal::Question) {
            // let boolean = self.stack.
        }
    }
    fn or(&mut self) {
        self.and();
    }
    fn and(&mut self) {
        self.eq();
    }
    fn eq(&mut self) {
        self.rel();
    }
    fn rel(&mut self) {
        self.add();
    }
    fn add(&mut self) {
        self.mult();
    }
    fn mult(&mut self) {
        self.unary();
    }
    fn unary(&mut self) {
        // optional here
    }
    fn postfix(&mut self) {
        self.primary();
    }
    fn primary(&mut self) {
        // match self.input.peek().unwrap() {
        //
        // }
    }
    fn var_or_func(&mut self) {}
    fn array(&mut self) {}
    fn dict(&mut self) {}
    fn arg_exp_list(&mut self) {}
    fn arg_list(&mut self) {}
    fn named_arg_list(&mut self) {}
    fn named_arg(&mut self) {}
    fn name(&mut self) {}
    fn boolean(&mut self) {
        match *self.input.peek().unwrap() {
            Token::Keyword(Keyword::True) => self.stack.op0(|| true),
            Token::Keyword(Keyword::False) => self.stack.op0(|| false),
            _ => {}
        }
    }
}
