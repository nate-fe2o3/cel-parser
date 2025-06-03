use std::{collections::HashMap, sync::Once};

use crate::{
    eve::{Alignment, LayoutAttributes, Placement, SliceSelect},
    eve_parser::{CellType, EveCallbackSuite, LinePosition, Relation},
    expression_parser::Expression,
};

static INIT: Once = Once::new();

// Layout parameter keys
const KEY_SPACING: &str = "spacing";
const KEY_INDENT: &str = "indent";
const KEY_MARGIN: &str = "margin";
const KEY_PLACEMENT: &str = "placement";
const KEY_HORIZONTAL: &str = "horizontal";
const KEY_VERTICAL: &str = "vertical";
const KEY_CHILD_HORIZONTAL: &str = "child_horizontal";
const KEY_CHILD_VERTICAL: &str = "child_vertical";

// Alignment keys
const KEY_ALIGN_LEFT: &str = "align_left";
const KEY_ALIGN_RIGHT: &str = "align_right";
const KEY_ALIGN_TOP: &str = "align_top";
const KEY_ALIGN_BOTTOM: &str = "align_bottom";
const KEY_ALIGN_CENTER: &str = "align_center";
const KEY_ALIGN_PROPORTIONAL: &str = "align_proportional";
const KEY_ALIGN_FILL: &str = "align_fill";

// Placement keys
const KEY_PLACE_ROW: &str = "place_row";
const KEY_PLACE_COLUMN: &str = "place_column";
const KEY_PLACE_OVERLAY: &str = "place_overlay";

// Guide keys
const KEY_GUIDE_MASK: &str = "guide_mask";
const KEY_GUIDE_BALANCE: &str = "guide_balance";
const KEY_GUIDE_BASELINE: &str = "guide_baseline";
const KEY_GUIDE_LABEL: &str = "guide_label";

static mut ALIGNMENT_TABLE: Option<HashMap<String, Alignment>> = None;
static mut PLACEMENT_TABLE: Option<HashMap<String, Placement>> = None;

fn init_tables() {
    INIT.call_once(|| unsafe {
        let mut alignment_table = HashMap::new();
        alignment_table.insert(KEY_ALIGN_LEFT.to_string(), Alignment::Left);
        alignment_table.insert(KEY_ALIGN_RIGHT.to_string(), Alignment::Right);
        alignment_table.insert(KEY_ALIGN_TOP.to_string(), Alignment::Top);
        alignment_table.insert(KEY_ALIGN_BOTTOM.to_string(), Alignment::Bottom);
        alignment_table.insert(KEY_ALIGN_CENTER.to_string(), Alignment::Center);
        alignment_table.insert(KEY_ALIGN_PROPORTIONAL.to_string(), Alignment::Proportional);
        alignment_table.insert(KEY_ALIGN_FILL.to_string(), Alignment::Fill);
        ALIGNMENT_TABLE = Some(alignment_table);

        let mut placement_table = HashMap::new();
        placement_table.insert(KEY_PLACE_ROW.to_string(), Placement::Row);
        placement_table.insert(KEY_PLACE_COLUMN.to_string(), Placement::Column);
        placement_table.insert(KEY_PLACE_OVERLAY.to_string(), Placement::Overlay);
        PLACEMENT_TABLE = Some(placement_table);
    });
}

#[derive(Debug, Clone)]
pub struct Dictionary {
    pub data: HashMap<String, Value>,
}

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Number(f64),
    Boolean(bool),
    Array(Vec<Value>),
    Dictionary(Dictionary),
    Empty,
}

impl Dictionary {
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }

    pub fn get(&self, key: &str) -> Option<&Value> {
        self.data.get(key)
    }

    pub fn insert(&mut self, key: String, value: Value) {
        self.data.insert(key, value);
    }

    pub fn contains_key(&self, key: &str) -> bool {
        self.data.contains_key(key)
    }
}

impl Default for Dictionary {
    fn default() -> Self {
        Self::new()
    }
}

pub trait VirtualMachine {
    fn evaluate(&mut self, expression: &Expression) -> Value;
    fn back(&self) -> &Value;
    fn pop_back(&mut self);
}

pub trait Sheet {
    fn add_constant(&mut self, name: &str, position: &LinePosition, expression: &Expression);
    fn add_logic(&mut self, name: &str, position: &LinePosition, expression: &Expression);
    fn add_interface(
        &mut self,
        name: &str,
        linked: bool,
        init_position: &LinePosition,
        initializer: &Expression,
        expr_position: &LinePosition,
        expression: &Expression,
    );
    fn add_relation(
        &mut self,
        position: &LinePosition,
        conditional: &Expression,
        relations: &[SheetRelation],
    );
    fn update(&mut self);
}

#[derive(Debug, Clone)]
pub struct SheetRelation {
    pub name_set: Vec<String>,
    pub position: LinePosition,
    pub expression: Expression,
}

pub type BindLayoutProc = Box<dyn Fn(&str, &crate::eve_parser::Position, &Dictionary)>;

pub struct EveCallbackSuiteImpl<S: Sheet, V: VirtualMachine> {
    pub sheet: S,
    pub evaluator: V,
    pub bind_proc: Option<BindLayoutProc>,
}

impl<S: Sheet, V: VirtualMachine> EveCallbackSuite for EveCallbackSuiteImpl<S, V> {
    fn add_view_proc(
        &mut self,
        name: &str,
        position: &crate::eve_parser::Position,
        arguments: &Expression,
    ) {
        let dict = self.evaluate_named_arguments(arguments);
        if let Some(ref proc) = self.bind_proc {
            proc(name, position, &dict);
        }
    }

    fn add_cell_proc(
        &mut self,
        cell_type: CellType,
        name: &str,
        position: &LinePosition,
        expression: &Expression,
        _brief: &str,
        _detailed: &str,
    ) {
        match cell_type {
            CellType::Constant => self.sheet.add_constant(name, position, expression),
            CellType::Logic => self.sheet.add_logic(name, position, expression),
            CellType::Interface => {
                // For interface cells, we need to handle them differently
                // This is a simplified implementation
                self.sheet.add_logic(name, position, expression);
            }
        }
    }

    fn add_relation_proc(
        &mut self,
        position: &LinePosition,
        conditional: &Expression,
        relations: &[Relation],
        _brief: &str,
        _detailed: &str,
    ) {
        let sheet_relations: Vec<SheetRelation> = relations
            .iter()
            .map(|r| SheetRelation {
                name_set: r.name_set.clone(),
                position: r.position.clone(),
                expression: r.expression.clone(),
            })
            .collect();

        self.sheet
            .add_relation(position, conditional, &sheet_relations);
    }

    fn add_interface_proc(
        &mut self,
        name: &str,
        linked: bool,
        init_position: &LinePosition,
        initializer: &Expression,
        expr_position: &LinePosition,
        expression: &Expression,
        _brief: &str,
        _detailed: &str,
    ) {
        self.sheet.add_interface(
            name,
            linked,
            init_position,
            initializer,
            expr_position,
            expression,
        );
    }

    fn finalize_sheet_proc(&mut self) {
        self.sheet.update();
    }
}

impl<S: Sheet, V: VirtualMachine> EveCallbackSuiteImpl<S, V> {
    pub fn new(sheet: S, evaluator: V) -> Self {
        Self {
            sheet,
            evaluator,
            bind_proc: None,
        }
    }

    pub fn with_bind_proc(mut self, proc: BindLayoutProc) -> Self {
        self.bind_proc = Some(proc);
        self
    }

    fn evaluate_named_arguments(&mut self, arguments: &Expression) -> Dictionary {
        let value = self.evaluator.evaluate(arguments);
        match value {
            Value::Dictionary(dict) => dict,
            _ => Dictionary::new(),
        }
    }
}

pub fn bind_layout<S: Sheet, V: VirtualMachine>(
    proc: BindLayoutProc,
    sheet: S,
    evaluator: V,
) -> EveCallbackSuiteImpl<S, V> {
    init_tables();
    EveCallbackSuiteImpl::new(sheet, evaluator).with_bind_proc(proc)
}

pub fn apply_layout_parameters(data: &mut LayoutAttributes, parameters: &Dictionary) {
    init_tables();

    // Apply indent
    if let Some(Value::Number(indent)) = parameters.get(KEY_INDENT) {
        data.indent = *indent as i32;
    }

    // Apply horizontal alignment
    if let Some(Value::String(alignment_name)) = parameters.get(KEY_HORIZONTAL) {
        unsafe {
            if let Some(ref table) = ALIGNMENT_TABLE {
                if let Some(&alignment) = table.get(alignment_name) {
                    data.slice[SliceSelect::Horizontal as usize].alignment = alignment;
                }
            }
        }
    }

    // Apply vertical alignment
    if let Some(Value::String(alignment_name)) = parameters.get(KEY_VERTICAL) {
        unsafe {
            if let Some(ref table) = ALIGNMENT_TABLE {
                if let Some(&alignment) = table.get(alignment_name) {
                    data.slice[SliceSelect::Vertical as usize].alignment = alignment;
                }
            }
        }
    }

    // Apply placement
    if let Some(Value::String(placement_name)) = parameters.get(KEY_PLACEMENT) {
        unsafe {
            if let Some(ref table) = PLACEMENT_TABLE {
                if let Some(&placement) = table.get(placement_name) {
                    data.placement = placement;
                }
            }
        }
    }

    // Apply guide mask
    if let Some(Value::Array(guide_mask)) = parameters.get(KEY_GUIDE_MASK) {
        // Turn on all guides initially
        data.slice[SliceSelect::Vertical as usize].suppress = false;
        data.slice[SliceSelect::Horizontal as usize].suppress = false;

        // Selectively suppress guides
        for guide in guide_mask {
            if let Value::String(guide_name) = guide {
                match guide_name.as_str() {
                    KEY_GUIDE_BASELINE => {
                        data.slice[SliceSelect::Vertical as usize].suppress = true;
                    }
                    KEY_GUIDE_LABEL => {
                        data.slice[SliceSelect::Horizontal as usize].suppress = true;
                    }
                    _ => {}
                }
            }
        }
    }

    // Apply margin
    if let Some(Value::Number(margin)) = parameters.get(KEY_MARGIN) {
        let margin_val = *margin as i32;
        data.slice[SliceSelect::Horizontal as usize].margin = (margin_val, margin_val);
        data.slice[SliceSelect::Vertical as usize].margin = (margin_val, margin_val);
    }

    // Apply spacing (simplified - would affect space_before in a full implementation)
    if let Some(Value::Number(_spacing)) = parameters.get(KEY_SPACING) {
        // In a full implementation, this would affect spacing between elements
    }
}

pub fn layout_variables(layout_sheet: &dyn Sheet, name: &str) -> Value {
    // Simplified implementation - in a real system this would query the sheet
    // for the current value of the named variable
    Value::Empty
}

#[cfg(test)]
mod tests {
    use super::*;

    struct MockSheet;
    struct MockVM;

    impl Sheet for MockSheet {
        fn add_constant(
            &mut self,
            _name: &str,
            _position: &LinePosition,
            _expression: &Expression,
        ) {
        }
        fn add_logic(&mut self, _name: &str, _position: &LinePosition, _expression: &Expression) {}
        fn add_interface(
            &mut self,
            _name: &str,
            _linked: bool,
            _init_position: &LinePosition,
            _initializer: &Expression,
            _expr_position: &LinePosition,
            _expression: &Expression,
        ) {
        }
        fn add_relation(
            &mut self,
            _position: &LinePosition,
            _conditional: &Expression,
            _relations: &[SheetRelation],
        ) {
        }
        fn update(&mut self) {}
    }

    impl VirtualMachine for MockVM {
        fn evaluate(&mut self, _expression: &Expression) -> Value {
            Value::Dictionary(Dictionary::new())
        }
        fn back(&self) -> &Value {
            &Value::Empty
        }
        fn pop_back(&mut self) {}
    }

    #[test]
    fn test_apply_layout_parameters() {
        let mut attributes = LayoutAttributes::default();
        let mut params = Dictionary::new();
        params.insert(KEY_INDENT.to_string(), Value::Number(10.0));
        params.insert(
            KEY_ALIGN_LEFT.to_string(),
            Value::String(KEY_ALIGN_CENTER.to_string()),
        );

        apply_layout_parameters(&mut attributes, &params);
        assert_eq!(attributes.indent, 10);
    }

    #[test]
    fn test_bind_layout() {
        let sheet = MockSheet;
        let vm = MockVM;
        let proc =
            Box::new(|_name: &str, _pos: &crate::eve_parser::Position, _dict: &Dictionary| {});

        let _suite = bind_layout(proc, sheet, vm);
        // Test that binding works without panicking
    }

    #[test]
    fn test_dictionary_operations() {
        let mut dict = Dictionary::new();
        dict.insert("test".to_string(), Value::Number(42.0));

        assert!(dict.contains_key("test"));
        assert!(!dict.contains_key("missing"));

        if let Some(Value::Number(val)) = dict.get("test") {
            assert_eq!(*val, 42.0);
        } else {
            panic!("Expected number value");
        }
    }
}

