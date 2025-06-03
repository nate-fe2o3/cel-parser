use std::{
    collections::{HashMap, HashSet, VecDeque},
    rc::{Rc, Weak},
    cell::RefCell,
    any::Any,
};

use crate::{
    expression_parser::Expression,
    adam_parser::LinePosition,
    adam_evaluate::SheetRelation,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Empty,
    Number(f64),
    String(String),
    Boolean(bool),
    Dictionary(HashMap<String, Value>),
    Array(Vec<Value>),
}

impl Default for Value {
    fn default() -> Self {
        Value::Empty
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AccessSpecifier {
    Input,
    InterfaceInput,
    InterfaceOutput,
    Output,
    Logic,
    Constant,
    Invariant,
}

pub trait VirtualMachine {
    fn evaluate(&mut self, expression: &Expression) -> Result<Value, String>;
    fn back(&self) -> &Value;
    fn pop_back(&mut self);
}

pub type MonitorValueCallback = Box<dyn Fn(&Value)>;
pub type MonitorEnabledCallback = Box<dyn Fn(bool)>;
pub type MonitorContributingCallback = Box<dyn Fn(&HashMap<String, Value>)>;
pub type MonitorInvariantCallback = Box<dyn Fn(bool)>;

pub struct Connection {
    id: usize,
}

impl Connection {
    pub fn new(id: usize) -> Self {
        Self { id }
    }
    
    pub fn disconnect(&self) {
        // In a full implementation, this would remove the callback
    }
}

#[derive(Debug, Clone)]
pub struct Cell {
    pub name: String,
    pub specifier: AccessSpecifier,
    pub value: Value,
    pub expression: Option<Expression>,
    pub position: LinePosition,
    pub linked: bool,
    pub invariant: bool,
    pub priority: i32,
    pub resolved: bool,
    pub evaluated: bool,
    pub dirty: bool,
    pub contributing: HashSet<String>,
    pub interface_input: Option<String>, // Name of corresponding input cell for interface outputs
}

impl Cell {
    pub fn new_input(name: String, position: LinePosition, initializer: Option<Expression>) -> Self {
        Self {
            name,
            specifier: AccessSpecifier::Input,
            value: Value::Empty,
            expression: initializer,
            position,
            linked: false,
            invariant: false,
            priority: 0,
            resolved: true,
            evaluated: true,
            dirty: false,
            contributing: HashSet::new(),
            interface_input: None,
        }
    }

    pub fn new_output(name: String, position: LinePosition, expression: Expression) -> Self {
        Self {
            name,
            specifier: AccessSpecifier::Output,
            value: Value::Empty,
            expression: Some(expression),
            position,
            linked: false,
            invariant: false,
            priority: 0,
            resolved: false,
            evaluated: false,
            dirty: true,
            contributing: HashSet::new(),
            interface_input: None,
        }
    }

    pub fn new_constant(name: String, position: LinePosition, initializer: Expression) -> Self {
        Self {
            name,
            specifier: AccessSpecifier::Constant,
            value: Value::Empty,
            expression: Some(initializer),
            position,
            linked: false,
            invariant: false,
            priority: 0,
            resolved: true,
            evaluated: false,
            dirty: false,
            contributing: HashSet::new(),
            interface_input: None,
        }
    }

    pub fn new_logic(name: String, position: LinePosition, expression: Expression) -> Self {
        Self {
            name,
            specifier: AccessSpecifier::Logic,
            value: Value::Empty,
            expression: Some(expression),
            position,
            linked: false,
            invariant: false,
            priority: 0,
            resolved: false,
            evaluated: false,
            dirty: true,
            contributing: HashSet::new(),
            interface_input: None,
        }
    }

    pub fn new_invariant(name: String, position: LinePosition, expression: Expression) -> Self {
        Self {
            name,
            specifier: AccessSpecifier::Invariant,
            value: Value::Empty,
            expression: Some(expression),
            position,
            linked: false,
            invariant: true,
            priority: 0,
            resolved: false,
            evaluated: false,
            dirty: true,
            contributing: HashSet::new(),
            interface_input: None,
        }
    }

    pub fn new_interface(
        name: String,
        linked: bool,
        init_position: LinePosition,
        initializer: Option<Expression>,
        expr_position: LinePosition,
        expression: Option<Expression>,
    ) -> (Self, Option<Self>) {
        let input_cell = Self {
            name: name.clone(),
            specifier: AccessSpecifier::InterfaceInput,
            value: Value::Empty,
            expression: initializer,
            position: init_position,
            linked,
            invariant: false,
            priority: 0,
            resolved: true,
            evaluated: true,
            dirty: false,
            contributing: HashSet::new(),
            interface_input: None,
        };

        let output_cell = if let Some(expr) = expression {
            Some(Self {
                name: format!("{}_output", name),
                specifier: AccessSpecifier::InterfaceOutput,
                value: Value::Empty,
                expression: Some(expr),
                position: expr_position,
                linked: false,
                invariant: false,
                priority: 0,
                resolved: false,
                evaluated: false,
                dirty: true,
                contributing: HashSet::new(),
                interface_input: Some(name.clone()),
            })
        } else {
            None
        };

        (input_cell, output_cell)
    }

    pub fn calculate(&mut self, vm: &mut dyn VirtualMachine) -> Result<(), String> {
        if self.evaluated {
            return Ok(());
        }

        if let Some(ref expression) = self.expression {
            self.value = vm.evaluate(expression)?;
            self.evaluated = true;
            self.dirty = false;
        }

        Ok(())
    }

    pub fn clear_dirty(&mut self) {
        self.dirty = false;
        self.evaluated = matches!(
            self.specifier,
            AccessSpecifier::Input | AccessSpecifier::Constant
        );
        self.resolved = self.evaluated;
    }
}

#[derive(Debug, Clone)]
pub struct RelationCell {
    pub position: LinePosition,
    pub conditional: Expression,
    pub relations: Vec<SheetRelation>,
    pub resolved: bool,
}

impl RelationCell {
    pub fn new(
        position: LinePosition,
        conditional: Expression,
        relations: Vec<SheetRelation>,
    ) -> Self {
        Self {
            position,
            conditional,
            relations,
            resolved: false,
        }
    }
}

pub struct Sheet {
    cells: HashMap<String, Cell>,
    relations: Vec<RelationCell>,
    input_cells: HashSet<String>,
    output_cells: HashSet<String>,
    priority_high: i32,
    priority_low: i32,
    has_output: bool,
    updated: bool,
    vm: Box<dyn VirtualMachine>,
    
    // Monitoring callbacks
    value_monitors: HashMap<String, Vec<MonitorValueCallback>>,
    enabled_monitors: HashMap<String, Vec<MonitorEnabledCallback>>,
    contributing_monitors: HashMap<String, Vec<MonitorContributingCallback>>,
    invariant_monitors: HashMap<String, Vec<MonitorInvariantCallback>>,
    next_connection_id: usize,
}

impl Sheet {
    pub fn new(vm: Box<dyn VirtualMachine>) -> Self {
        Self {
            cells: HashMap::new(),
            relations: Vec::new(),
            input_cells: HashSet::new(),
            output_cells: HashSet::new(),
            priority_high: 0,
            priority_low: 0,
            has_output: false,
            updated: false,
            vm,
            value_monitors: HashMap::new(),
            enabled_monitors: HashMap::new(),
            contributing_monitors: HashMap::new(),
            invariant_monitors: HashMap::new(),
            next_connection_id: 0,
        }
    }

    pub fn inspect(&mut self, expression: &Expression) -> Result<Value, String> {
        let result = self.vm.evaluate(expression)?;
        Ok(result)
    }

    pub fn set(&mut self, name: &str, value: Value) -> Result<(), String> {
        if let Some(cell) = self.cells.get_mut(name) {
            if !matches!(
                cell.specifier,
                AccessSpecifier::Input | AccessSpecifier::InterfaceInput
            ) {
                return Err(format!("Cell {} is not an input cell", name));
            }

            self.priority_high += 1;
            cell.value = value;
            cell.priority = self.priority_high;
            cell.dirty = true;
            self.updated = false;
            Ok(())
        } else {
            Err(format!("Input cell {} does not exist", name))
        }
    }

    pub fn get(&self, name: &str) -> Result<&Value, String> {
        if let Some(cell) = self.cells.get(name) {
            if !cell.evaluated {
                return Err(format!("Cell {} has not been evaluated", name));
            }
            Ok(&cell.value)
        } else {
            Err(format!("Cell {} not found", name))
        }
    }

    pub fn add_input(&mut self, name: &str, position: &LinePosition, initializer: &Expression) {
        let cell = Cell::new_input(
            name.to_string(),
            position.clone(),
            Some(initializer.clone()),
        );
        self.input_cells.insert(name.to_string());
        self.cells.insert(name.to_string(), cell);
    }

    pub fn add_output(&mut self, name: &str, position: &LinePosition, expression: &Expression) {
        let cell = Cell::new_output(
            name.to_string(),
            position.clone(),
            expression.clone(),
        );
        self.output_cells.insert(name.to_string());
        self.has_output = true;
        self.cells.insert(name.to_string(), cell);
    }

    pub fn add_constant(&mut self, name: &str, position: &LinePosition, initializer: &Expression) {
        let cell = Cell::new_constant(
            name.to_string(),
            position.clone(),
            initializer.clone(),
        );
        self.cells.insert(name.to_string(), cell);
    }

    pub fn add_logic(&mut self, name: &str, position: &LinePosition, expression: &Expression) {
        let cell = Cell::new_logic(
            name.to_string(),
            position.clone(),
            expression.clone(),
        );
        self.cells.insert(name.to_string(), cell);
    }

    pub fn add_invariant(&mut self, name: &str, position: &LinePosition, expression: &Expression) {
        let cell = Cell::new_invariant(
            name.to_string(),
            position.clone(),
            expression.clone(),
        );
        self.cells.insert(name.to_string(), cell);
    }

    pub fn add_interface(
        &mut self,
        name: &str,
        linked: bool,
        init_position: &LinePosition,
        initializer: &Expression,
        expr_position: &LinePosition,
        expression: &Expression,
    ) {
        let (input_cell, output_cell) = Cell::new_interface(
            name.to_string(),
            linked,
            init_position.clone(),
            Some(initializer.clone()),
            expr_position.clone(),
            Some(expression.clone()),
        );

        self.input_cells.insert(name.to_string());
        self.cells.insert(name.to_string(), input_cell);

        if let Some(output) = output_cell {
            let output_name = output.name.clone();
            self.output_cells.insert(output_name.clone());
            self.has_output = true;
            self.cells.insert(output_name, output);
        }
    }

    pub fn add_relation(
        &mut self,
        position: &LinePosition,
        conditional: &Expression,
        relations: &[SheetRelation],
    ) {
        let relation_cell = RelationCell::new(
            position.clone(),
            conditional.clone(),
            relations.to_vec(),
        );
        self.relations.push(relation_cell);
    }

    pub fn has_input(&self, name: &str) -> bool {
        self.input_cells.contains(name)
    }

    pub fn has_output(&self, name: &str) -> bool {
        self.output_cells.contains(name)
    }

    pub fn update(&mut self) -> Result<(), String> {
        if self.updated {
            return Ok(());
        }

        // Clear dirty flags
        for cell in self.cells.values_mut() {
            cell.clear_dirty();
        }

        // Calculate all cells that need evaluation
        self.calculate_all()?;

        // Resolve relations
        self.resolve_relations()?;

        self.updated = true;
        Ok(())
    }

    pub fn reinitialize(&mut self) -> Result<(), String> {
        // Reset all cells to initial state
        for cell in self.cells.values_mut() {
            cell.clear_dirty();
            cell.resolved = matches!(
                cell.specifier,
                AccessSpecifier::Input | AccessSpecifier::Constant
            );
            cell.evaluated = cell.resolved;
        }

        // Clear relations
        for relation in &mut self.relations {
            relation.resolved = false;
        }

        self.updated = false;
        self.update()
    }

    pub fn contributing(&self, mark: &HashMap<String, Value>) -> HashMap<String, Value> {
        let mut result = HashMap::new();
        
        for (name, cell) in &self.cells {
            if cell.dirty && !mark.contains_key(name) {
                result.insert(name.clone(), cell.value.clone());
            } else if let Some(mark_value) = mark.get(name) {
                if &cell.value != mark_value {
                    result.insert(name.clone(), cell.value.clone());
                }
            }
        }
        
        result
    }

    pub fn contributing_to_cell(&self, name: &str) -> HashMap<String, Value> {
        let mut result = HashMap::new();
        
        if let Some(cell) = self.cells.get(name) {
            for contributing_name in &cell.contributing {
                if let Some(contributing_cell) = self.cells.get(contributing_name) {
                    result.insert(contributing_name.clone(), contributing_cell.value.clone());
                }
            }
        }
        
        result
    }

    // Monitoring methods
    pub fn monitor_value(&mut self, name: &str, callback: MonitorValueCallback) -> Connection {
        let id = self.next_connection_id;
        self.next_connection_id += 1;

        if let Some(cell) = self.cells.get(name) {
            callback(&cell.value);
        }

        self.value_monitors
            .entry(name.to_string())
            .or_insert_with(Vec::new)
            .push(callback);

        Connection::new(id)
    }

    pub fn monitor_enabled(
        &mut self,
        name: &str,
        _touch_cells: &[String],
        callback: MonitorEnabledCallback,
    ) -> Connection {
        let id = self.next_connection_id;
        self.next_connection_id += 1;

        // Simplified enabled monitoring
        callback(true);

        self.enabled_monitors
            .entry(name.to_string())
            .or_insert_with(Vec::new)
            .push(callback);

        Connection::new(id)
    }

    pub fn monitor_contributing(
        &mut self,
        name: &str,
        mark: &HashMap<String, Value>,
        callback: MonitorContributingCallback,
    ) -> Connection {
        let id = self.next_connection_id;
        self.next_connection_id += 1;

        let contributing = self.contributing_to_cell(name);
        callback(&contributing);

        self.contributing_monitors
            .entry(name.to_string())
            .or_insert_with(Vec::new)
            .push(callback);

        Connection::new(id)
    }

    pub fn monitor_invariant_dependent(
        &mut self,
        name: &str,
        callback: MonitorInvariantCallback,
    ) -> Connection {
        let id = self.next_connection_id;
        self.next_connection_id += 1;

        if let Some(cell) = self.cells.get(name) {
            callback(cell.invariant);
        }

        self.invariant_monitors
            .entry(name.to_string())
            .or_insert_with(Vec::new)
            .push(callback);

        Connection::new(id)
    }

    // Private helper methods
    fn calculate_all(&mut self) -> Result<(), String> {
        // Topological sort and calculation
        let mut to_calculate: VecDeque<String> = self
            .cells
            .iter()
            .filter(|(_, cell)| !cell.evaluated)
            .map(|(name, _)| name.clone())
            .collect();

        while let Some(name) = to_calculate.pop_front() {
            if let Some(cell) = self.cells.get_mut(&name) {
                if !cell.evaluated {
                    // Try to calculate the cell
                    if let Err(e) = cell.calculate(self.vm.as_mut()) {
                        // If calculation fails, put it back in the queue
                        to_calculate.push_back(name);
                        continue;
                    }
                }
            }
        }

        Ok(())
    }

    fn resolve_relations(&mut self) -> Result<(), String> {
        // Simplified relation resolution
        for relation in &mut self.relations {
            if !relation.resolved {
                // Evaluate conditional
                let conditional_result = self.vm.evaluate(&relation.conditional)?;
                
                // For simplicity, assume all relations are resolved
                relation.resolved = true;
            }
        }

        Ok(())
    }

    fn notify_value_monitors(&self, name: &str, value: &Value) {
        if let Some(monitors) = self.value_monitors.get(name) {
            for monitor in monitors {
                monitor(value);
            }
        }
    }

    fn notify_enabled_monitors(&self, name: &str, enabled: bool) {
        if let Some(monitors) = self.enabled_monitors.get(name) {
            for monitor in monitors {
                monitor(enabled);
            }
        }
    }

    fn notify_contributing_monitors(&self, name: &str, contributing: &HashMap<String, Value>) {
        if let Some(monitors) = self.contributing_monitors.get(name) {
            for monitor in monitors {
                monitor(contributing);
            }
        }
    }

    fn notify_invariant_monitors(&self, name: &str, invariant: bool) {
        if let Some(monitors) = self.invariant_monitors.get(name) {
            for monitor in monitors {
                monitor(invariant);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expression_parser::Expression;
    use crate::lexer::Token;

    struct TestVM;

    impl VirtualMachine for TestVM {
        fn evaluate(&mut self, expression: &Expression) -> Result<Value, String> {
            match expression {
                Expression::Literal(Token::Number(n)) => Ok(Value::Number(*n)),
                Expression::Literal(Token::String(s)) => Ok(Value::String(s.clone())),
                Expression::Literal(Token::True) => Ok(Value::Boolean(true)),
                Expression::Literal(Token::False) => Ok(Value::Boolean(false)),
                Expression::Literal(Token::Empty) => Ok(Value::Empty),
                _ => Ok(Value::Number(42.0)), // Simplified evaluation
            }
        }

        fn back(&self) -> &Value {
            &Value::Empty
        }

        fn pop_back(&mut self) {
            // No-op for test
        }
    }

    #[test]
    fn test_sheet_creation() {
        let vm = Box::new(TestVM);
        let sheet = Sheet::new(vm);
        assert!(!sheet.has_output);
        assert!(!sheet.updated);
    }

    #[test]
    fn test_add_input() {
        let vm = Box::new(TestVM);
        let mut sheet = Sheet::new(vm);
        let position = LinePosition::new("test", 1, 1);
        let initializer = Expression::Literal(Token::Number(10.0));
        
        sheet.add_input("test_input", &position, &initializer);
        assert!(sheet.has_input("test_input"));
    }

    #[test]
    fn test_add_output() {
        let vm = Box::new(TestVM);
        let mut sheet = Sheet::new(vm);
        let position = LinePosition::new("test", 1, 1);
        let expression = Expression::Literal(Token::Number(20.0));
        
        sheet.add_output("test_output", &position, &expression);
        assert!(sheet.has_output("test_output"));
        assert!(sheet.has_output);
    }

    #[test]
    fn test_set_and_get() {
        let vm = Box::new(TestVM);
        let mut sheet = Sheet::new(vm);
        let position = LinePosition::new("test", 1, 1);
        let initializer = Expression::Literal(Token::Empty);
        
        sheet.add_input("test_input", &position, &initializer);
        sheet.set("test_input", Value::Number(42.0)).unwrap();
        
        let value = sheet.get("test_input").unwrap();
        assert!(matches!(value, Value::Number(42.0)));
    }

    #[test]
    fn test_update() {
        let vm = Box::new(TestVM);
        let mut sheet = Sheet::new(vm);
        let position = LinePosition::new("test", 1, 1);
        let expression = Expression::Literal(Token::Number(100.0));
        
        sheet.add_output("test_output", &position, &expression);
        sheet.update().unwrap();
        assert!(sheet.updated);
    }
} 