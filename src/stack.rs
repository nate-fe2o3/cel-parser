use std::{collections::HashMap, default, mem::Discriminant};

#[derive(Debug)]
enum Value {
    Num(f64),
    String(String),
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Num(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::String(value)
    }
}

impl TryFrom<Value> for f64 {
    type Error = Value;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Num(x) => Ok(x),
            val => Err(val),
        }
    }
}

impl TryFrom<Value> for String {
    type Error = Value;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(x) => Ok(x),
            val => Err(val),
        }
    }
}

macro_rules! unary_fn {
    (
        $name:ident,
        $(($aty:ident, |$a:ident| $body:expr)),* $(,)?
    ) => {
        fn $name(a: Value) -> Value {
            match a {
                $(Value::$aty($a) => $body.into(),)*
                a => panic!("{a:?} is invalid for {}", stringify!($name))
            }
        }
    };
}

macro_rules! binary_fn {
    (
        $name:ident,
        $(($aty:ident, $bty:ident, |$a:ident, $b:ident| $body:expr)),* $(,)?
    ) => {
        fn $name(a: Value, b: Value) -> Value {
            match (a, b) {
                $((Value::$aty($a), Value::$bty($b)) => $body.into(),)*
                (a, b) => panic!("{a:?} and {b:?} are invalid for {}", stringify!($name))
            }
        }
    };
}

macro_rules! ternary_fn {
    (
        $name:ident,
        $(($aty:ident, $bty:ident, $cty:ident, |$a:ident, $b:ident, $c:ident| $body:expr)),* $(,)?
    ) => {
        fn $name(a: Value, b: Value, c: Value) -> Value {
            match (a, b, c) {
                $((Value::$aty($a), Value::$bty($b), Value::$cty($c)) => $body.into(),)*
                (a, b, c) => panic!("{a:?} and {b:?} and {c:?} are invalid for {}", stringify!($name))
            }
        }
    };
}

unary_fn!(neg, (Num, |a| -a));

binary_fn!(
    add,
    (Num, Num, |a, b| a + b),
    (String, String, |a, b| format!("{a}{b}")),
    (Num, String, |a, b| a + b.parse::<f64>().unwrap()),
);
ternary_fn!(add3, (Num, Num, Num, |a, b, c| a + b + c));

binary_fn!(mul, (Num, Num, |a, b| a * b));

type Stack = Vec<Value>;
// type Words = HashMap<(String, usize), Box<dyn Fn(&mut Stack)>>;
type UnaryMap = HashMap<String, fn(Value) -> Value>;
type BinaryMap = HashMap<String, fn(Value, Value) -> Value>;
type TernaryMap = HashMap<String, fn(Value, Value, Value) -> Value>;

#[derive(Default)]
struct Seg {
    stack: Stack,
    un: UnaryMap,
    bi: BinaryMap,
    tern: TernaryMap,
}

impl Seg {
    fn new(stack: Vec<Value>) -> Self {
        Self {
            stack,
            ..Default::default()
        }
    }
    fn register1(&mut self, name: impl Into<String>, f: fn(Value) -> Value) {
        self.un.insert(name.into(), f);
    }

    fn register2(&mut self, name: impl Into<String>, f: fn(Value, Value) -> Value) {
        self.bi.insert(name.into(), f);
    }
    fn register3(&mut self, name: impl Into<String>, f: fn(Value, Value, Value) -> Value) {
        self.tern.insert(name.into(), f);
    }
    fn callfn(&mut self, name: String, args: u8) {
        println!("in callfn. name: {name}, args: {args}");
        match args {
            1 => {
                let f = *self.un.get(&name).unwrap();
                self.op1(f);
            }
            2 => {
                let f = *self.bi.get(&name).unwrap();
                self.op2(f);
            }
            3 => {
                let f = *self.tern.get(&name).unwrap();
                self.op3(f);
            }
            _ => panic!(),
        }
    }
    fn op1(&mut self, f: fn(Value) -> Value) {
        let x = self.stack.pop().unwrap();
        self.stack.push(f(x));
    }
    fn op2(&mut self, f: fn(Value, Value) -> Value) {
        let y = self.stack.pop().unwrap();
        let x = self.stack.pop().unwrap();
        self.stack.push(f(x, y));
    }
    fn op3(&mut self, f: fn(Value, Value, Value) -> Value) {
        let z = self.stack.pop().unwrap();
        let y = self.stack.pop().unwrap();
        let x = self.stack.pop().unwrap();
        self.stack.push(f(x, y, z));
    }
}

// fn register1(words: &mut Words, name: impl Into<String>, f: impl Fn(Value) -> Value + 'static) {
//     words.insert(
//         (name.into(), 1),
//         Box::new(move |stack: &mut Stack| {
//             let a = stack.pop().unwrap();
//             stack.push(f(a));
//         }),
//     );
// }
// fn register2(
//     words: &mut Words,
//     name: impl Into<String>,
//     f: impl Fn(Value, Value) -> Value + 'static,
// ) {
//     words.insert(
//         (name.into(), 2),
//         Box::new(move |stack: &mut Stack| {
//             let a = stack.pop().unwrap();
//             let b = stack.pop().unwrap();
//             stack.push(f(a, b));
//         }),
//     );
// }
// fn register3(
//     words: &mut Words,
//     name: impl Into<String>,
//     f: impl Fn(Value, Value, Value) -> Value + 'static,
// ) {
//     words.insert(
//         (name.into(), 3),
//         Box::new(move |stack: &mut Stack| {
//             let a = stack.pop().unwrap();
//             let b = stack.pop().unwrap();
//             let c = stack.pop().unwrap();
//             stack.push(f(a, b, c));
//         }),
//     );
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let mut stack: Stack = vec![1.0.into(), 2.0.into(), 7.0.into()];
        let program = vec![("add".to_string(), 3)];
        let mut fns = Seg::new(stack);
        //container struct with helper functions
        fns.register1("neg", neg);
        fns.register2("add", add);
        fns.register3("add", add3);
        fns.register2("mul", mul);

        for (word, args) in program {
            // let f = &words[&word];
            fns.callfn(word, args);
        }
        fns.op1(|x| {
            let t: f64 = x.try_into().unwrap();
            Value::from(-t)
        });
        println!("stack: {:?}", fns.stack);
        // assert_eq!(fns.stack[0].try_into().unwrap(), 10.0);
        assert_eq!(1, 2)
    }
}
