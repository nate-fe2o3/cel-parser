use std::iter::Peekable;

use cel_rs::dyn_segment::DynSegment;

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
        self.parse(0);
    }

    fn parse(&mut self, prec: u8) {
        if prec < 2 {
            self.parse(prec + 1);
        } else {
            self.literal();
        }
        self.rhs(prec);
    }
    fn rhs(&mut self, prec: u8) {
        let Some(chunk) = self.input.peek().to_owned() else {
            return;
        };
        match (prec, *chunk) {
            (0, "+") => {
                println!("plus found");
                self.input.next().unwrap();
                self.parse(prec + 1);
                // self.term();
                self.stack
                    .op2(|x: i64, y: i64| x + y)
                    .expect("addition to work");
                self.rhs(prec);
            }
            (0, "-") => {
                self.input.next().unwrap();
                self.parse(prec + 1);
                // self.term();
                self.stack
                    .op2(|x: i64, y: i64| x - y)
                    .expect("subtraction to work");
                self.rhs(prec);
            }
            (0, ")") => {
                self.input.next().unwrap();
            }
            (1, "*") => {
                println!("mult found");
                self.input.next().unwrap();
                // self.exponent();
                self.parse(prec + 1);
                self.stack
                    .op2(|x: i64, y: i64| x * y)
                    .expect("mult to work");
                self.rhs(prec);
            }
            (1, "/") => {
                self.input.next().unwrap();
                // self.exponent();
                self.parse(prec + 1);
                self.stack
                    .op2(|x: i64, y: i64| x / y)
                    .expect("mult to work");
                self.rhs(prec);
            }
            (2, "^") => {
                self.input.next().unwrap();
                self.literal();
                // recurse first, nested exponents eval from right to left
                self.rhs(prec);
                self.stack
                    .op2(|x: i64, y: i64| x.pow(y as u32))
                    .expect("exponent to work");
            }
            _ => {}
        };
    }

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
                self.parse(0);
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
