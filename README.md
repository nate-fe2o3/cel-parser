# Adobe Source Libraries - Rust Implementation

This project is a complete Rust translation of the Adobe Source Libraries, specifically the **Eve** layout engine and **Adam** property model system. The implementation maintains the core functionality of the original C++ codebase while leveraging Rust's safety and performance benefits.

## Overview

The Adobe Source Libraries provide a declarative UI framework with two main components:

1. **Eve** - A layout description language and constraint-based layout engine
2. **Adam** - A property model system for managing application state and data flow

This Rust implementation includes:
- Complete lexer and expression parser
- Eve layout engine with view hierarchies and constraint solving
- Adam property sheet system with cells, relations, and monitoring
- Comprehensive test suite with 49 passing tests
- Working demo showcasing all major features

## Architecture

### Core Components

#### 1. Lexer (`lexer.rs`)
- Tokenizes input text for both Eve and Adam languages
- Handles identifiers, numbers, strings, operators, and keywords
- Supports comments and whitespace handling
- Provides detailed error reporting with line/column information

#### 2. Expression Parser (`expression_parser.rs`)
- Parses expressions used in both Eve and Adam
- Supports literals, variables, function calls, operators
- Handles arrays, dictionaries, and complex expressions
- Implements operator precedence and associativity

#### 3. Eve Layout Engine

##### Eve Parser (`eve_parser.rs`)
- Parses Eve layout description language
- Handles layout specifiers, cell declarations, and view definitions
- Supports constant, interface, and logic cell types
- Uses callback pattern for handling parsed elements

##### Eve Core (`eve.rs`)
- Core layout engine with view hierarchies
- Constraint-based layout calculation
- Support for alignment, placement, and margins
- Guide-based positioning system
- Placeable trait for UI elements

##### Eve Evaluation (`eve_evaluate.rs`)
- Layout parameter application and binding
- Integration with sheets and virtual machines
- Value system for layout properties
- Dictionary-based parameter passing

#### 4. Adam Property Model System

##### Adam Parser (`adam_parser.rs`)
- Parses Adam sheet description language
- Handles input, output, constant, logic, and invariant cells
- Supports interface cells and relation declarations
- Implements conditional logic with "when" clauses

##### Adam Core (`adam.rs`)
- Property sheet engine with cell management
- Dependency tracking and automatic updates
- Monitoring system for value changes
- Support for input/output cells and calculations
- Constraint solving and relation evaluation

##### Adam Evaluation (`adam_evaluate.rs`)
- Callback suite implementation for sheet binding
- External model integration
- Expression evaluation utilities
- Type-safe cell management

## Key Features

### Memory Safety
- No raw pointers - uses `Rc<RefCell<>>` for shared ownership
- Compile-time guarantees against data races
- Automatic memory management

### Type Safety
- Strong typing throughout the system
- Compile-time error checking
- Pattern matching for control flow

### Error Handling
- Result types instead of exceptions
- Detailed error messages with location information
- Graceful error recovery

### Performance
- Zero-cost abstractions
- Efficient constraint solving
- Minimal runtime overhead

## Usage Examples

### Eve Layout Engine

```rust
use eve::{Eve, LayoutAttributes, EvaluateOptions};

// Create layout engine
let mut eve = Eve::new();

// Add root container
let root_attrs = LayoutAttributes::default();
let root_id = eve.add_placeable(None, root_attrs, true, root_placeable, false);

// Add child elements
let child_attrs = LayoutAttributes::default();
eve.add_placeable(Some(root_id), child_attrs, false, child_placeable, false);

// Evaluate layout
let (width, height) = eve.evaluate(EvaluateOptions::default(), 300, 200);
```

### Adam Property Sheets

```rust
use adam::{Sheet, VirtualMachine, Value};

// Create sheet with virtual machine
let vm = Box::new(MyVM::new());
let mut sheet = Sheet::new(vm);

// Add cells
sheet.add_input("width", &position, &Expression::Literal(Token::Number(200.0)));
sheet.add_output("area", &position, &Expression::Binary { /* width * height */ });

// Set values and update
sheet.set("width", Value::Number(300.0))?;
sheet.update()?;

// Get computed values
let area = sheet.get("area")?;
```

### Parsing Eve Layouts

```rust
let eve_source = r#"
    layout example {
        constant: margin: 10;
        interface: width: 200; height: 100;
        view button(text: "Click me") { /* view definition */ }
    }
"#;

// Tokenize and parse
let mut lexer = Lexer::new(eve_source);
lexer.lex()?;

let suite = eve_evaluate::bind_layout(bind_proc, sheet, vm);
let result = eve_parser::parse(lexer.tokens, &position, suite)?;
```

### Parsing Adam Sheets

```rust
let adam_source = r#"
    sheet example {
        input: width: 200; height: 100;
        constant: pi: 3.14159;
        output: area <== width * height;
        logic: when (width > 100) relate {
            large_width <== true;
            display_mode <== "wide";
        }
    }
"#;

// Tokenize and parse
let mut lexer = Lexer::new(adam_source);
lexer.lex()?;

let suite = adam_evaluate::bind_to_sheet(sheet_impl);
let result = adam_parser::parse(lexer.tokens, suite)?;
```

## Testing

The implementation includes comprehensive tests covering:

- **Lexer tests**: Token recognition, error handling, edge cases
- **Parser tests**: Expression parsing, operator precedence, complex structures
- **Eve tests**: Layout calculation, view hierarchies, constraint solving
- **Adam tests**: Cell management, dependency tracking, value updates

Run tests with:
```bash
cargo test
```

All 49 tests pass successfully, demonstrating the robustness of the implementation.

## Demo

Run the demo to see all components in action:
```bash
cargo run
```

The demo showcases:
1. **Eve Layout Engine**: Creating view hierarchies and evaluating layouts
2. **Eve Parser**: Tokenizing and parsing layout descriptions
3. **Adam Sheet Engine**: Managing property cells and dependencies
4. **Adam Parser**: Parsing sheet definitions with various cell types
5. **Layout Parameters**: Applying layout attributes and margins
6. **Expression Parsing**: Parsing complex mathematical expressions

## Improvements Over Original C++

### Safety
- **Memory safety**: No buffer overflows, use-after-free, or memory leaks
- **Thread safety**: Compile-time prevention of data races
- **Type safety**: Strong typing prevents many runtime errors

### Performance
- **Zero-cost abstractions**: High-level code with low-level performance
- **Efficient memory layout**: Better cache locality
- **Optimized compilation**: LLVM backend optimizations

### Maintainability
- **Clear ownership**: Explicit ownership and borrowing rules
- **Pattern matching**: Exhaustive case handling
- **Trait system**: Flexible, composable interfaces
- **Error handling**: Explicit error propagation with `Result` types

### Developer Experience
- **Cargo ecosystem**: Modern package management and build system
- **Documentation**: Built-in documentation generation
- **Testing**: Integrated testing framework
- **Tooling**: Excellent IDE support and debugging tools

## Dependencies

The implementation uses minimal external dependencies:
- `itertools` - For iterator utilities in parsing

## Building

```bash
# Build the project
cargo build

# Build with optimizations
cargo build --release

# Run tests
cargo test

# Run the demo
cargo run

# Generate documentation
cargo doc --open
```

## File Structure

```
src/
├── lib.rs              # Library root module
├── main.rs             # Demo application
├── lexer.rs            # Tokenization
├── expression_parser.rs # Expression parsing
├── eve_parser.rs       # Eve layout parser
├── eve.rs              # Eve layout engine
├── eve_evaluate.rs     # Eve evaluation and binding
├── adam_parser.rs      # Adam sheet parser
├── adam.rs             # Adam property sheet engine
└── adam_evaluate.rs    # Adam evaluation and binding
```

## License

This implementation maintains compatibility with the original Adobe Source Libraries licensing terms.

## Contributing

Contributions are welcome! Please ensure:
- All tests pass (`cargo test`)
- Code follows Rust conventions (`cargo fmt`, `cargo clippy`)
- New features include appropriate tests
- Documentation is updated for public APIs

## Future Enhancements

Potential areas for future development:
- **Advanced constraint solving**: More sophisticated layout algorithms
- **Performance optimizations**: Incremental updates and caching
- **Extended language features**: Additional operators and built-ins
- **Integration examples**: Real-world UI framework integration
- **Serialization support**: Save/load layout and sheet definitions
- **Visual debugging**: Tools for inspecting layout and property flow