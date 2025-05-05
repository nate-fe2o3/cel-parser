use std::{
    any::TypeId,
    cell::LazyCell,
    collections::HashMap,
};

use cel_rs::{
    segment::Segment,
    type_list::{IntoList, List},
};

type FunctionMap = HashMap<TypeId, Box<dyn FunctionMappable>>;
type HM = LazyCell<HashMap<&'static str, FunctionMap>>;

const FUNCS: HM = LazyCell::new(|| {
    let mut hm = HashMap::new();
    let thing = Box::new(Segment::<(usize,)>::new().op1(|x| x * 2));
    let other = Box::new(Segment::<(isize,)>::new().op1(|x| x * 2));
    let third = Box::new(Segment::<(&str,)>::new().op1(|x| format!("{x}{x}")));
    let mut o: FunctionMap = HashMap::new();
    o.insert(thing.args_type_id(), thing);
    o.insert(other.args_type_id(), other);
    o.insert(third.args_type_id(), third);
    hm.insert("double", o);
    hm
});

trait FunctionMappable {
    fn args_type_id(&self) -> TypeId;
}

impl<Args: IntoList + 'static, Stack: List + 'static> FunctionMappable for Segment<Args, Stack> {
    fn args_type_id(&self) -> TypeId {
        TypeId::of::<Args>()
    }
}

#[cfg(test)]
mod tests {
    use std::any::{Any, type_name_of_val};

    use super::*;

    #[test]
    fn basic() {
        let thing = (1usize, 1usize);
        let f = FUNCS;
        println!("{:?}", &thing.type_id());
        let options = f.get("double").expect("there to be a plus operator");
        let correct = options
            .iter()
            .find(|x| thing.type_id() == x.args_type_id())
            .expect("there to be a (usize,usize) plus function");
    }
}
