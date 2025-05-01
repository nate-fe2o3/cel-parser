use std::{any::TypeId, cell::LazyCell, collections::HashMap, iter::Peekable};

use cel_rs::{
    dyn_segment::DynSegment,
    segment::{Callable, Segment},
    type_list::{IntoList, List},
};

type HM = LazyCell<HashMap<&'static str, fn(usize, usize) -> usize>>;

// const FUNCS: HM = LazyCell::new(|| {
//     let mut hm: HashMap<&'static str, fn(usize, usize) -> const R: usize> = HashMap::new();
//     hm.insert("+", |x, y| x + y);
//     hm.insert("*", |x, y| x * y);
//     hm.insert("-", |x, y| x - y);
//     hm.insert("/", |x, y| x / y);
//     hm.insert("^", |x, y| x.pow(y as u32));
//     hm
// });

pub struct Parser<T: Iterator<Item = &'static str>> {
    input: Peekable<T>,
    stack: DynSegment,
}

impl<T: Iterator<Item = &'static str>> Parser<T> {
    pub fn new(input: T) -> Self {
        let lexed = input.peekable();
        Self {
            input: lexed,
            stack: DynSegment::new::<()>(),
        }
    }

    pub fn run(&mut self) {
        self.expression();
    }

    fn expression(&mut self) {
        self.term();
        self.more_expr();
    }

    fn more_expr(&mut self) {
        let Some(chunk) = self.input.peek().to_owned() else {
            return;
        };
        match *chunk {
            "+" => {
                println!("plus found");
                self.input.next().unwrap();
                // self.literal();
                self.term();
                self.stack
                    .op2(|x: i64, y: i64| x + y)
                    .expect("addition to work");
                self.more_expr();
            }
            "-" => {
                self.input.next().unwrap();
                // self.literal();
                // self.expression();
                self.term();
                self.stack
                    .op2(|x: i64, y: i64| x - y)
                    .expect("subtraction to work");
                self.more_expr();
            }
            ")" => {
                self.input.next().unwrap();
            }
            _ => {}
        };
    }

    fn term(&mut self) {
        self.exponent();
        self.more_term();
    }

    fn more_term(&mut self) {
        let Some(chunk) = self.input.peek().to_owned() else {
            return;
        };
        match *chunk {
            "*" => {
                println!("mult found");
                self.input.next().unwrap();
                // self.literal();
                self.exponent();
                self.stack
                    .op2(|x: i64, y: i64| x * y)
                    .expect("mult to work");
                self.more_term();
            }
            "/" => {
                self.input.next().unwrap();
                // self.literal();
                self.exponent();
                self.stack
                    .op2(|x: i64, y: i64| x / y)
                    .expect("mult to work");
                self.more_term();
            }
            _ => {}
        };
    }

    fn exponent(&mut self) {
        self.literal();
        self.more_exponent();
    }

    fn more_exponent(&mut self) {
        let Some(chunk) = self.input.peek().to_owned() else {
            return;
        };
        if *chunk == "^" {
            self.input.next().unwrap();
            self.literal();
            // recurse first, nested exponents eval from right to left
            self.more_exponent();
            self.stack
                .op2(|x: i64, y: i64| x.pow(y as u32))
                .expect("exponent to work");
        };
    }

    // fn parse_rhs<Args: IntoList + 'static, Stack: List + 'static, R: 'static>(
    //     &mut self,
    //     precedence: u8,
    //     stack: Segment<Args, (R, Stack)>,
    // ) -> Segment<Args, (R, Stack)> {
    //     let Some(thing) = self.input.peek() else {
    //         return stack;
    //     };
    //     let token = thing.to_owned();
    //     println!("token: {token}\nprecedence: {precedence}");
    //     match (precedence, token) {
    //         // add or subtract
    //         (0, "+") | (0, "-") | (1, "*") | (1, "/") | (2, "^") => {
    //             let functions = FUNCS;
    //             let op = functions.get(token).unwrap();
    //             self.input.next();
    //             if precedence == 2 {
    //                 // recursive call runs before power calculation, exponents are evaluated right to left
    //                 // replace with prec + 1 when parse function fixed
    //                 let stack = self.parse(precedence, stack);
    //                 stack.op2(op.clone())
    //             } else {
    //                 let stack = stack.op2(op.clone());
    //                 self.parse(precedence, stack)
    //             }
    //         }
    //         (0, ")") => {
    //             self.input.next();
    //             stack
    //         }
    //         (0, _) => panic!(),
    //
    //         _ => {
    //             println!("doing nothing");
    //             stack
    //         }
    //     }
    // }

    fn literal(&mut self) {
        // let functions = FUNCS;
        // let Some(terminal) = self.input.peek() else {
        //     return;
        // };
        let terminal = self.input.peek().unwrap().to_owned();
        if let Ok(number) = terminal.parse::<i64>() {
            dbg!(number);
            self.input.next();
            self.stack.op0(move || number);
            return;
        }
        // if let Some(fun) = functions.get(terminal) {
        //     self.stack.op2(fun.clone()).unwrap();
        // }
        match terminal {
            "(" => {
                println!("start paren found in number check");
                self.input.next();
                self.expression();
            }
            _ => panic!(),
        }
    }
    // fn arglist(&mut self) {
    //     self.parse(0);
    //     self.arg_prime();
    // }

    // fn arg_prime(&mut self) {
    //     let Some(maybe_comma) = self.input.peek() else {
    //         return;
    //     };
    //     if *maybe_comma == "," {
    //         self.input.next();
    //         self.parse(0);
    //         self.arg_prime();
    //     }
    // }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn add() {
        let input = "1 + 2".split_whitespace();
        let mut parser = Parser::new(input);
        parser.run();
        let res = parser.stack.call0::<i64>().expect("call to work");
        assert_eq!(3i64, res)
    }

    #[test]
    fn mult() {
        let input = "2 * 3".split_whitespace();
        let mut parser = Parser::new(input);
        parser.run();
        let res = parser.stack.call0::<i64>().expect("call to work");
        assert_eq!(6, res)
    }

    #[test]
    fn mult_and_add() {
        let input = "3 * 2 + 1".split_whitespace();
        let mut parser = Parser::new(input);
        parser.run();
        let res = parser.stack.call0::<i64>().expect("call to work");
        assert_eq!(7, res)
    }

    #[test]
    fn order_of_operations() {
        let input = "1 + 2 * 3".split_whitespace();
        let mut parser = Parser::new(input);
        parser.run();
        let res = parser.stack.call0::<i64>().expect("call to work");
        assert_eq!(7, res)
    }

    #[test]
    fn parens() {
        let input = "( 1 + 2 ) * 3".split_whitespace();
        let mut parser = Parser::new(input);
        parser.run();
        let res = parser.stack.call0::<i64>().expect("call to work");
        assert_eq!(9, res)
    }

    #[test]
    fn parens_end() {
        let input = "2 * ( 2 + 3 )".split_whitespace();
        let mut parser = Parser::new(input);
        parser.run();
        let res = parser.stack.call0::<i64>().expect("call to work");
        assert_eq!(10, res)
    }

    #[test]
    fn basic_exponent() {
        let input = "2 ^ 10".split_whitespace();
        let mut parser = Parser::new(input);
        parser.run();
        let res = parser.stack.call0::<i64>().expect("call to work");
        assert_eq!(1024, res)
    }

    #[test]
    fn nested_exponent() {
        let input = "2 ^ 3 ^ 2".split_whitespace();
        let mut parser = Parser::new(input);
        parser.run();
        let res = parser.stack.call0::<i64>().expect("call to work");
        assert_eq!(512, res)
    }

    #[test]
    fn non_commutative_sub() {
        let input = "10 - 3 + 2".split_whitespace();
        let mut parser = Parser::new(input);
        parser.run();
        let res = parser.stack.call0::<i64>().expect("call to work");
        assert_eq!(9, res)
    }

    #[test]
    fn non_commutative_div() {
        let input = "12 / 2 * 3".split_whitespace();
        let mut parser = Parser::new(input);
        parser.run();
        let res = parser.stack.call0::<i64>().expect("call to work");
        assert_eq!(18, res)
    }

    #[test]
    fn nested_divs() {
        let input = "12 / 3 / 2".split_whitespace();
        let mut parser = Parser::new(input);
        parser.run();
        let res = parser.stack.call0::<i64>().expect("call to work");
        assert_eq!(2, res)
    }
}
