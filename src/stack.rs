use std::{any::TypeId, collections::HashMap, default, mem::Discriminant};

use cel_rs::{DynSegment, IntoList};

type NonaryMap<T: 'static> = HashMap<String, fn() -> T>;
type UnaryMap<T: 'static, U: 'static> = HashMap<String, fn(T) -> U>;
type BinaryMap<T: 'static, U: 'static, V: 'static> = HashMap<String, fn(T, U) -> V>;
type OldParentMap = HashMap<(TypeId, u8), DynMap>;
type OldDynMap = HashMap<&'static str, Box<dyn Fn()>>;
type ParentMap = HashMap<((Option<TypeId>, u8), TypeId), DynMap>;
type DynMap = HashMap<&'static str, Box<dyn FunctionMappable>>;
// type DynMap = HashMap<&'static str, &'static dyn FunctionMappable>;

#[derive(Default)]
pub struct FnStack(ParentMap);

trait FunctionMappable {}

impl<T> FunctionMappable for fn() -> T {}
impl<T, U> FunctionMappable for fn(T) -> U {}
impl<T, U, V> FunctionMappable for fn(T, U) -> V {}
impl<T, U, V, W> FunctionMappable for fn(T, U, V) -> W {}

impl FnStack {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }
    pub fn register0<T: 'static>(&mut self, name: &'static str, f: fn() -> T) {
        let inner_dyn_map = self
            .0
            .entry(((None, 0), TypeId::of::<T>()))
            .or_insert(HashMap::new());
        inner_dyn_map.insert(name, Box::new(f));
    }
    pub fn register1<T: 'static, U: 'static>(&mut self, name: &'static str, f: fn(T) -> U) {
        let inner_dyn_map = self
            .0
            .entry(((Some(TypeId::of::<T>()), 1), TypeId::of::<U>()))
            .or_insert(HashMap::new());
        inner_dyn_map.insert(name, Box::new(f));
    }
    pub fn register2<T: 'static, U: 'static, V: 'static>(
        &mut self,
        name: &'static str,
        f: fn(T, U) -> V,
    ) {
        let inner_dyn_map = self
            .0
            .entry(((Some(TypeId::of::<(T, U)>()), 2), TypeId::of::<V>()))
            .or_insert(HashMap::new());
        inner_dyn_map.insert(name, Box::new(f));
    }
    pub fn register3<T: 'static, U: 'static, V: 'static, W: 'static>(
        &mut self,
        name: &'static str,
        f: fn(T, U, V) -> W,
    ) {
        let inner_dyn_map = self
            .0
            .entry(((Some(TypeId::of::<(T, U, V)>()), 3), TypeId::of::<W>()))
            .or_insert(HashMap::new());
        inner_dyn_map.insert(name, Box::new(f));
    }

    pub fn callfn(&mut self, name: &'static str, args: u8, stack: DynSegment) {
        let [p0] = stack.get_last_n_padded::<1>();
        stack.pop_types::<(T, ())>()?;
        self.segment.push_op1(op, p0);
        self.push_type::<R>();
        Ok(())
        let ty = stack.po
        self.0.get()
    }

    fn setup(&mut self) {
        let ty = TypeId::of::<(f64, f64, f64)>();
        self.register(
            "testFn",
            ty,
            2,
            Box::new(|x: f64, y: f64| (x / y) * (x + y)),
        );
    }
    // fn op1(&mut self, f: fn(Value) -> Value) {
    //     let x = self.stack.pop().unwrap();
    //     self.stack.push(f(x));
    // }
    // fn op2(&mut self, f: fn(Value, Value) -> Value) {
    //     let y = self.stack.pop().unwrap();
    //     let x = self.stack.pop().unwrap();
    //     //         self.stack.push(f(x, y));
    //     //     }
    //     //     fn op3(&mut self, f: fn(Value, Value, Value) -> Value) {
    //     //         let z = self.stack.pop().unwrap();
    //     //         let y = self.stack.pop().unwrap();
    //     //         let x = self.stack.pop().unwrap();
    //     //         self.stack.push(f(x, y, z));
    // }
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

fn test_fn(x: f64, y: f64) -> String {
    format!("{}", x + y)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let mut thing = FnStack::new();
        thing.register2("testFn", test_fn);
    }
    // #[test]
    // fn test() {
    //     let mut stack: Stack = vec![1.0.into(), 2.0.into(), 7.0.into()];
    //     let program = vec![("add".to_string(), 3)];
    //     let mut fns = Seg::new(stack);
    //     //container struct with helper functions
    //     fns.register1("neg", neg);
    //     fns.register2("add", add);
    //     fns.register3("add", add3);
    //     fns.register2("mul", mul);
    //
    //     for (word, args) in program {
    //         // let f = &words[&word];
    //         fns.callfn(word, args);
    //     }
    //     fns.op1(|x| {
    //         let t: f64 = x.try_into().unwrap();
    //         Value::from(-t)
    //     });
    //     println!("stack: {:?}", fns.stack);
    //     // assert_eq!(fns.stack[0].try_into().unwrap(), 10.0);
    //     assert_eq!(1, 2)
    // }
}
