use std::{collections::HashMap, default, mem::Discriminant};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Num(f64),
    String(String),
    Boolean(bool),
    Vec(Vec<Value>),
    Dict(HashMap<String, Value>),
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Num(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Boolean(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::String(value)
    }
}
impl From<Vec<Value>> for Value {
    fn from(value: Vec<Value>) -> Self {
        Value::Vec(value)
    }
}
impl From<HashMap<String, Value>> for Value {
    fn from(value: HashMap<String, Value>) -> Self {
        Value::Dict(value)
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

impl TryFrom<Value> for bool {
    type Error = Value;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Boolean(x) => Ok(x),
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

impl TryFrom<Value> for Vec<Value> {
    type Error = Value;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Vec(x) => Ok(x),
            val => Err(val),
        }
    }
}
impl TryFrom<Value> for HashMap<String, Value> {
    type Error = Value;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Dict(x) => Ok(x),
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

type Stack = Vec<Value>;
// type Words = HashMap<(String, usize), Box<dyn Fn(&mut Stack)>>;
type NonaryMap = HashMap<String, fn() -> Value>;
type UnaryMap = HashMap<String, fn(Value) -> Value>;
type BinaryMap = HashMap<String, fn(Value, Value) -> Value>;
type TernaryMap = HashMap<String, fn(Value, Value, Value) -> Value>;

#[derive(Default)]
pub struct Seg {
    pub stack: Stack,
    pub non: NonaryMap,
    pub un: UnaryMap,
    pub bi: BinaryMap,
    pub tern: TernaryMap,
}

impl Seg {
    pub fn new(stack: Vec<Value>) -> Self {
        let mut s = Self {
            stack,
            ..Default::default()
        };
        s.register();
        s
    }
    pub fn register(&mut self) {
        self.register1("not", not);
        self.register1("neg", neg);

        self.register2("add", add);
        self.register2("mul", mul);
        self.register2("sub", sub);
        self.register2("div", div);
        self.register2("mod", modd);

        self.register2("lt", lt);
        self.register2("lte", lte);

        self.register2("gt", gt);
        self.register2("gte", gte);

        self.register2("eq", eq);
        self.register2("neq", neq);

        self.register2("and", and);
        self.register2("or", or);
    }
    pub fn push(&mut self, x: Value) {
        self.stack.push(x);
    }

    pub fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }
    pub fn register0(&mut self, name: impl Into<String>, f: fn() -> Value) {
        self.non.insert(name.into(), f);
    }
    pub fn register1(&mut self, name: impl Into<String>, f: fn(Value) -> Value) {
        self.un.insert(name.into(), f);
    }

    pub fn register2(&mut self, name: impl Into<String>, f: fn(Value, Value) -> Value) {
        self.bi.insert(name.into(), f);
    }
    pub fn register3(&mut self, name: impl Into<String>, f: fn(Value, Value, Value) -> Value) {
        self.tern.insert(name.into(), f);
    }
    pub fn callfn(&mut self, name: String, args: u8) {
        println!("in callfn. name: {name}, args: {args}");
        match args {
            0 => {
                let f = *self.non.get(&name).unwrap();
                self.op0(f);
            }
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
    pub fn op0(&mut self, f: fn() -> Value) {
        self.stack.push(f());
    }
    pub fn op1(&mut self, f: fn(Value) -> Value) {
        let x = self.stack.pop().unwrap();
        self.stack.push(f(x));
    }
    pub fn op2(&mut self, f: fn(Value, Value) -> Value) {
        let y = self.stack.pop().unwrap();
        let x = self.stack.pop().unwrap();
        self.stack.push(f(x, y));
    }
    pub fn op3(&mut self, f: fn(Value, Value, Value) -> Value) {
        let z = self.stack.pop().unwrap();
        let y = self.stack.pop().unwrap();
        let x = self.stack.pop().unwrap();
        self.stack.push(f(x, y, z));
    }
}

// BUILTIN FUNCTIONS

unary_fn!(neg, (Num, |a| -a));
unary_fn!(not, (Boolean, |a| !a));

binary_fn!(
    add,
    (Num, Num, |a, b| a + b),
    (String, String, |a, b| format!("{a}{b}")),
    (Num, String, |a, b| a + b.parse::<f64>().unwrap()),
    (String, Num, |a, b| b + a.parse::<f64>().unwrap()),
);

binary_fn!(mul, (Num, Num, |a, b| a * b));
binary_fn!(sub, (Num, Num, |a, b| a - b));
binary_fn!(div, (Num, Num, |a, b| a / b));
binary_fn!(modd, (Num, Num, |a, b| a % b));
binary_fn!(lt, (Num, Num, |a, b| a < b));
binary_fn!(lte, (Num, Num, |a, b| a <= b));
binary_fn!(gt, (Num, Num, |a, b| a > b));
binary_fn!(gte, (Num, Num, |a, b| a >= b));
binary_fn!(or, (Boolean, Boolean, |a, b| a || b));
binary_fn!(and, (Boolean, Boolean, |a, b| a && b));
// f64 issues
// binary_fn!(bitand, (Num, Num, |a, b| a & b));
// unary_fn!(bitnot, (Num |a | !a));
// binary_fn!(bitor, (Num, Num, |a, b| a ` b));
// binary_fn!(bitxor, (Num, Num, |a, b| a ^ b));
// binary_fn!(lshift, (Num, Num, |a, b| a << b));
// binary_fn!(rshift, (Num, Num, |a, b| a >> b));
binary_fn!(
    eq,
    (Num, Num, |a, b| a == b),
    (String, String, |a, b| a == b),
    (Boolean, Boolean, |a, b| a == b),
);
binary_fn!(
    neq,
    (Num, Num, |a, b| a != b),
    (String, String, |a, b| a != b),
    (Boolean, Boolean, |a, b| a != b),
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic() {
        let mut stack: Stack = vec![1.0.into(), 2.0.into(), 7.0.into()];
        let program = vec![("add".to_string(), 2), ("add".to_string(), 2)];
        let mut fns = Seg::new(stack);
        for (word, args) in program {
            fns.callfn(word, args);
        }
        fns.op1(|x| {
            let t: f64 = x.try_into().unwrap();
            Value::from(-t)
        });
        println!("stack: {:?}", fns.stack);
        let thing: f64 = fns.stack[0].clone().try_into().unwrap();
        assert_eq!(thing, -10.);
        // assert_eq!(1, 2)
    }
}
