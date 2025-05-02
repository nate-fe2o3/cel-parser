use std::{any::TypeId, cell::LazyCell, collections::HashMap, ops::Add};

use cel_rs::{
    segment::Segment,
    type_list::{IntoList, List},
};

type HM = LazyCell<HashMap<&'static str, Vec<Box<dyn FunctionMappable>>>>;

const FUNCS: HM = LazyCell::new(|| {
    let mut hm = HashMap::new();
    let thing = Box::new(Segment::<(usize, usize)>::new().op2(|x, y| x + y));
    let other = Box::new(Segment::<(isize, isize)>::new().op2(|x, y| x + y));
    let third = Box::new(Segment::<(&str, &str)>::new().op2(|x, y| format!("{x}{y}")));
    let mut o: Vec<Box<dyn FunctionMappable>> = Vec::new();
    o.push(thing);
    o.push(other);
    o.push(third);
    hm.insert("+", o);
    // hm.insert("*", |x, y| x * y);
    // hm.insert("-", |x, y| x - y);
    // hm.insert("/", |x, y| x / y);
    // hm.insert("^", |x, y| x.pow(y as u32));
    hm
});

trait FunctionMappable {
    fn args_type(&self) -> TypeId;
}

impl<Args: IntoList + 'static, Stack: List + 'static> FunctionMappable for Segment<Args, Stack> {
    fn args_type(&self) -> TypeId {
        TypeId::of::<Args>()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {}
}
