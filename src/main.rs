mod adam;
mod adam_evaluate;
mod adam_parser;
mod eve;
mod eve_evaluate;
mod eve_parser;
mod expression_parser;
mod lexer;
mod single_cel;
mod stack;

fn main() {
    println!("Hello World");
}

// use eve::{EvaluateOptions, Eve, LayoutAttributes, PlaceData, Placeable, SliceSelect};
// use eve_evaluate::{Dictionary, Sheet, SheetRelation, Value, VirtualMachine};
// use eve_parser::LinePosition;
// use expression_parser::Expression;
// use lexer::Lexer;
//
// // Adam imports
// use adam::{Sheet as AdamSheet, Value as AdamValue, VirtualMachine as AdamVM};
// use adam_evaluate::{CellType, Sheet as AdamSheetTrait, bind_to_sheet};
// use adam_parser::{AdamCallbackSuite, LinePosition as AdamLinePosition};
//
// // Example implementation of required traits
// struct ExampleSheet {
//     constants: Vec<(String, Expression)>,
//     logic: Vec<(String, Expression)>,
//     interfaces: Vec<(String, Expression)>,
// }
//
// impl ExampleSheet {
//     fn new() -> Self {
//         Self {
//             constants: Vec::new(),
//             logic: Vec::new(),
//             interfaces: Vec::new(),
//         }
//     }
// }
//
// impl Sheet for ExampleSheet {
//     fn add_constant(&mut self, name: &str, _position: &LinePosition, expression: &Expression) {
//         self.constants.push((name.to_string(), expression.clone()));
//         println!("Added constant: {} = {:?}", name, expression);
//     }
//
//     fn add_logic(&mut self, name: &str, _position: &LinePosition, expression: &Expression) {
//         self.logic.push((name.to_string(), expression.clone()));
//         println!("Added logic: {} = {:?}", name, expression);
//     }
//
//     fn add_interface(
//         &mut self,
//         name: &str,
//         linked: bool,
//         _init_position: &LinePosition,
//         initializer: &Expression,
//         _expr_position: &LinePosition,
//         expression: &Expression,
//     ) {
//         self.interfaces.push((name.to_string(), expression.clone()));
//         println!(
//             "Added interface: {} (linked: {}) = init: {:?}, expr: {:?}",
//             name, linked, initializer, expression
//         );
//     }
//
//     fn add_relation(
//         &mut self,
//         _position: &LinePosition,
//         conditional: &Expression,
//         relations: &[SheetRelation],
//     ) {
//         println!(
//             "Added relation with conditional: {:?}, relations: {:?}",
//             conditional, relations
//         );
//     }
//
//     fn update(&mut self) {
//         println!("Sheet updated");
//     }
// }
//
// struct ExampleVM {
//     stack: Vec<Value>,
// }
//
// impl ExampleVM {
//     fn new() -> Self {
//         Self { stack: Vec::new() }
//     }
// }
//
// impl VirtualMachine for ExampleVM {
//     fn evaluate(&mut self, _expression: &Expression) -> Value {
//         // Simplified evaluation - just return empty dictionary for now
//         let result = Value::Dictionary(Dictionary::new());
//         self.stack.push(result.clone());
//         result
//     }
//
//     fn back(&self) -> &Value {
//         self.stack.last().unwrap_or(&Value::Empty)
//     }
//
//     fn pop_back(&mut self) {
//         self.stack.pop();
//     }
// }
//
// // Adam-specific implementations
// struct ExampleAdamSheet {
//     inputs: Vec<(String, Expression)>,
//     outputs: Vec<(String, Expression)>,
//     constants: Vec<(String, Expression)>,
//     logic: Vec<(String, Expression)>,
//     invariants: Vec<(String, Expression)>,
//     interfaces: Vec<(String, Expression)>,
// }
//
// impl ExampleAdamSheet {
//     fn new() -> Self {
//         Self {
//             inputs: Vec::new(),
//             outputs: Vec::new(),
//             constants: Vec::new(),
//             logic: Vec::new(),
//             invariants: Vec::new(),
//             interfaces: Vec::new(),
//         }
//     }
// }
//
// impl AdamSheetTrait for ExampleAdamSheet {
//     fn add_input(&mut self, name: &str, _position: &AdamLinePosition, initializer: &Expression) {
//         self.inputs.push((name.to_string(), initializer.clone()));
//         println!("Added Adam input: {} = {:?}", name, initializer);
//     }
//
//     fn add_output(&mut self, name: &str, _position: &AdamLinePosition, expression: &Expression) {
//         self.outputs.push((name.to_string(), expression.clone()));
//         println!("Added Adam output: {} = {:?}", name, expression);
//     }
//
//     fn add_constant(&mut self, name: &str, _position: &AdamLinePosition, initializer: &Expression) {
//         self.constants.push((name.to_string(), initializer.clone()));
//         println!("Added Adam constant: {} = {:?}", name, initializer);
//     }
//
//     fn add_logic(&mut self, name: &str, _position: &AdamLinePosition, expression: &Expression) {
//         self.logic.push((name.to_string(), expression.clone()));
//         println!("Added Adam logic: {} = {:?}", name, expression);
//     }
//
//     fn add_invariant(&mut self, name: &str, _position: &AdamLinePosition, expression: &Expression) {
//         self.invariants.push((name.to_string(), expression.clone()));
//         println!("Added Adam invariant: {} = {:?}", name, expression);
//     }
//
//     fn add_interface(
//         &mut self,
//         name: &str,
//         linked: bool,
//         _init_position: &AdamLinePosition,
//         initializer: &Expression,
//         _expr_position: &AdamLinePosition,
//         expression: &Expression,
//     ) {
//         self.interfaces.push((name.to_string(), expression.clone()));
//         println!(
//             "Added Adam interface: {} (linked: {}) = init: {:?}, expr: {:?}",
//             name, linked, initializer, expression
//         );
//     }
//
//     fn add_relation(
//         &mut self,
//         _position: &AdamLinePosition,
//         conditional: &Expression,
//         relations: &[adam_evaluate::SheetRelation],
//     ) {
//         println!(
//             "Added Adam relation with conditional: {:?}, relations: {:?}",
//             conditional, relations
//         );
//     }
//
//     fn update(&mut self) {
//         println!("Adam sheet updated");
//     }
// }
//
// struct ExampleAdamVM {
//     stack: Vec<AdamValue>,
// }
//
// impl ExampleAdamVM {
//     fn new() -> Self {
//         Self { stack: Vec::new() }
//     }
// }
//
// impl AdamVM for ExampleAdamVM {
//     fn evaluate(&mut self, expression: &Expression) -> Result<AdamValue, String> {
//         // Simplified evaluation
//         let result = match expression {
//             Expression::Literal(lexer::Token::Number(n)) => AdamValue::Number(*n),
//             Expression::Literal(lexer::Token::String(s)) => AdamValue::String(s.clone()),
//             Expression::Literal(lexer::Token::True) => AdamValue::Boolean(true),
//             Expression::Literal(lexer::Token::False) => AdamValue::Boolean(false),
//             _ => AdamValue::Empty,
//         };
//         self.stack.push(result.clone());
//         Ok(result)
//     }
//
//     fn back(&self) -> &AdamValue {
//         self.stack.last().unwrap_or(&AdamValue::Empty)
//     }
//
//     fn pop_back(&mut self) {
//         self.stack.pop();
//     }
// }
//
// struct ExamplePlaceable {
//     name: String,
//     size: (i32, i32),
// }
//
// impl ExamplePlaceable {
//     fn new(name: &str, size: (i32, i32)) -> Self {
//         Self {
//             name: name.to_string(),
//             size,
//         }
//     }
// }
//
// impl Placeable for ExamplePlaceable {
//     fn measure(&self, slice: SliceSelect) -> (i32, i32) {
//         match slice {
//             SliceSelect::Horizontal => (self.size.0, self.size.0 * 2),
//             SliceSelect::Vertical => (self.size.1, self.size.1 * 2),
//         }
//     }
//
//     fn place(&mut self, place_data: &PlaceData) {
//         println!(
//             "Placing {}: horizontal={}x{}, vertical={}x{}",
//             self.name,
//             place_data.slice[0].position,
//             place_data.slice[0].length,
//             place_data.slice[1].position,
//             place_data.slice[1].length
//         );
//     }
// }
//
// fn main() {
//     println!("Adobe Source Libraries - Rust Implementation");
//
//     // Example 1: Basic layout engine usage (Eve)
//     println!("\n=== Eve Layout Engine Example ===");
//     let mut eve = Eve::new();
//
//     // Add a root container
//     let root_placeable = Box::new(ExamplePlaceable::new("root", (200, 100)));
//     let root_attrs = LayoutAttributes::default();
//     let root_id = eve.add_placeable(None, root_attrs, true, root_placeable, false);
//
//     // Add some child elements
//     let child1 = Box::new(ExamplePlaceable::new("child1", (50, 30)));
//     let child1_attrs = LayoutAttributes::default();
//     eve.add_placeable(Some(root_id), child1_attrs, false, child1, false);
//
//     let child2 = Box::new(ExamplePlaceable::new("child2", (60, 40)));
//     let child2_attrs = LayoutAttributes::default();
//     eve.add_placeable(Some(root_id), child2_attrs, false, child2, false);
//
//     // Evaluate the layout
//     let (width, height) = eve.evaluate(EvaluateOptions::default(), 300, 200);
//     println!("Final layout size: {}x{}", width, height);
//
//     // Example 2: Eve Parser usage
//     println!("\n=== Eve Parser Example ===");
//
//     // Create a simple Eve layout description
//     let eve_source = r#"
//         layout example {
//             constant:
//                 margin: 10;
//                 spacing: 5;
//
//             interface:
//                 width: 200;
//                 height: 100;
//
//             view button(text: "Click me", margin: @margin) {
//                 // View definition would go here
//             }
//         }
//     "#;
//
//     // Tokenize the input
//     let mut lexer = Lexer::new(eve_source);
//     if let Err(e) = lexer.lex() {
//         println!("Lexer error: {:?}", e);
//         return;
//     }
//
//     println!("Tokenized {} tokens", lexer.tokens.len());
//
//     // Create callback suite
//     let sheet = ExampleSheet::new();
//     let vm = ExampleVM::new();
//     let bind_proc = Box::new(
//         |name: &str, _pos: &eve_parser::Position, dict: &Dictionary| {
//             println!("Binding view: {} with parameters: {:?}", name, dict);
//         },
//     );
//
//     let suite = eve_evaluate::bind_layout(bind_proc, sheet, vm);
//
//     // Parse the Eve layout (simplified - would need proper position handling)
//     let position = Box::new(()) as eve_parser::Position;
//     match eve_parser::parse(lexer.tokens, &position, suite) {
//         Ok(_final_suite) => {
//             println!("Eve parsing completed successfully");
//         }
//         Err(e) => {
//             println!("Eve parse error: {:?}", e);
//         }
//     }
//
//     // Example 3: Adam Sheet Engine
//     println!("\n=== Adam Sheet Engine Example ===");
//
//     let adam_vm = Box::new(ExampleAdamVM::new());
//     let mut adam_sheet = AdamSheet::new(adam_vm);
//
//     // Add some cells
//     let position = AdamLinePosition::new("test", 1, 1);
//     let input_expr = Expression::Literal(lexer::Token::Number(10.0));
//     let output_expr = Expression::Literal(lexer::Token::Number(20.0));
//
//     adam_sheet.add_input("input1", &position, &input_expr);
//     adam_sheet.add_output("output1", &position, &output_expr);
//     adam_sheet.add_constant(
//         "pi",
//         &position,
//         &Expression::Literal(lexer::Token::Number(3.14159)),
//     );
//
//     // Set input value and update
//     adam_sheet.set("input1", AdamValue::Number(42.0)).unwrap();
//     adam_sheet.update().unwrap();
//
//     // Get output value
//     if let Ok(value) = adam_sheet.get("input1") {
//         println!("Adam input1 value: {:?}", value);
//     }
//
//     println!(
//         "Adam sheet has {} inputs, {} outputs",
//         adam_sheet.has_input("input1"),
//         adam_sheet.has_output("output1")
//     );
//
//     // Example 4: Adam Parser usage
//     println!("\n=== Adam Parser Example ===");
//
//     let adam_source = r#"
//         sheet example {
//             input:
//                 width: 200;
//                 height: 100;
//
//             constant:
//                 pi: 3.14159;
//                 margin: 10;
//
//             output:
//                 area <== width * height;
//                 circumference <== 2 * pi * width;
//
//             logic:
//                 when (width > 100) relate {
//                     large_width <== true;
//                     display_mode <== "wide";
//                 }
//         }
//     "#;
//
//     // Tokenize Adam source
//     let mut adam_lexer = Lexer::new(adam_source);
//     if let Err(e) = adam_lexer.lex() {
//         println!("Adam lexer error: {:?}", e);
//         return;
//     }
//
//     println!("Adam tokenized {} tokens", adam_lexer.tokens.len());
//
//     // Create Adam callback suite
//     let adam_sheet_impl = ExampleAdamSheet::new();
//     let adam_suite = bind_to_sheet(adam_sheet_impl);
//
//     // Parse Adam sheet
//     match adam_parser::parse(adam_lexer.tokens, adam_suite) {
//         Ok(_final_suite) => {
//             println!("Adam parsing completed successfully");
//         }
//         Err(e) => {
//             println!("Adam parse error: {:?}", e);
//         }
//     }
//
//     // Example 5: Layout parameter application (Eve)
//     println!("\n=== Layout Parameters Example ===");
//     let mut layout_attrs = LayoutAttributes::default();
//     let mut params = Dictionary::new();
//     params.insert("indent".to_string(), Value::Number(15.0));
//     params.insert(
//         "horizontal".to_string(),
//         Value::String("align_center".to_string()),
//     );
//     params.insert("margin".to_string(), Value::Number(8.0));
//
//     eve_evaluate::apply_layout_parameters(&mut layout_attrs, &params);
//     println!(
//         "Applied layout parameters: indent={}, margin={:?}",
//         layout_attrs.indent, layout_attrs.slice[0].margin
//     );
//
//     // Example 6: Adam expression parsing
//     println!("\n=== Adam Expression Parsing Example ===");
//
//     let expression_str = "width * height + margin";
//     match adam_evaluate::parse_adam_expression(expression_str) {
//         Ok(expr) => {
//             println!("Parsed Adam expression: {:?}", expr);
//         }
//         Err(e) => {
//             println!("Adam expression parse error: {:?}", e);
//         }
//     }
//
//     println!("\n=== Demo Complete ===");
// }
