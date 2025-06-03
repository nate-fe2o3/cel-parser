use std::collections::HashMap;

use crate::{
    expression_parser::Expression,
    adam_parser::{LinePosition, AdamCallbackSuite, Relation},
};

#[derive(Debug, Clone, PartialEq)]
pub enum CellType {
    Input,
    Output,
    Constant,
    Logic,
    Invariant,
}

#[derive(Debug, Clone)]
pub struct SheetRelation {
    pub name_set: Vec<String>,
    pub position: LinePosition,
    pub expression: Expression,
}

pub trait Sheet {
    fn add_input(&mut self, name: &str, position: &LinePosition, initializer: &Expression);
    fn add_output(&mut self, name: &str, position: &LinePosition, expression: &Expression);
    fn add_constant(&mut self, name: &str, position: &LinePosition, initializer: &Expression);
    fn add_logic(&mut self, name: &str, position: &LinePosition, expression: &Expression);
    fn add_invariant(&mut self, name: &str, position: &LinePosition, expression: &Expression);
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

pub trait ExternalModel {
    fn add_cell(&mut self, name: &str, position: &LinePosition, brief: &str, detailed: &str);
}

pub struct AdamCallbackSuiteImpl<S: Sheet> {
    pub sheet: S,
    pub external_model: Option<Box<dyn ExternalModel>>,
}

impl<S: Sheet> AdamCallbackSuiteImpl<S> {
    pub fn new(sheet: S) -> Self {
        Self {
            sheet,
            external_model: None,
        }
    }

    pub fn with_external_model(mut self, external_model: Box<dyn ExternalModel>) -> Self {
        self.external_model = Some(external_model);
        self
    }

    fn add_cell(
        &mut self,
        cell_type: CellType,
        name: &str,
        position: &LinePosition,
        init_or_expr: &Expression,
        _brief: &str,
        _detailed: &str,
    ) {
        match cell_type {
            CellType::Input => self.sheet.add_input(name, position, init_or_expr),
            CellType::Output => self.sheet.add_output(name, position, init_or_expr),
            CellType::Constant => self.sheet.add_constant(name, position, init_or_expr),
            CellType::Logic => self.sheet.add_logic(name, position, init_or_expr),
            CellType::Invariant => self.sheet.add_invariant(name, position, init_or_expr),
        }
    }

    fn add_relation(
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

        self.sheet.add_relation(position, conditional, &sheet_relations);
    }
}

impl<S: Sheet> AdamCallbackSuite for AdamCallbackSuiteImpl<S> {
    fn add_cell_proc(
        &mut self,
        cell_type: CellType,
        name: &str,
        position: &LinePosition,
        init_or_expr: &Expression,
        brief: &str,
        detailed: &str,
    ) {
        self.add_cell(cell_type, name, position, init_or_expr, brief, detailed);
    }

    fn add_relation_proc(
        &mut self,
        position: &LinePosition,
        conditional: &Expression,
        relations: &[Relation],
        brief: &str,
        detailed: &str,
    ) {
        self.add_relation(position, conditional, relations, brief, detailed);
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

    fn add_external_proc(
        &mut self,
        name: &str,
        position: &LinePosition,
        brief: &str,
        detailed: &str,
    ) {
        if let Some(ref mut external_model) = self.external_model {
            external_model.add_cell(name, position, brief, detailed);
        }
    }
}

pub fn bind_to_sheet<S: Sheet>(sheet: S) -> AdamCallbackSuiteImpl<S> {
    AdamCallbackSuiteImpl::new(sheet)
}

pub fn bind_to_sheet_with_external<S: Sheet>(
    sheet: S,
    external_model: Box<dyn ExternalModel>,
) -> AdamCallbackSuiteImpl<S> {
    AdamCallbackSuiteImpl::new(sheet).with_external_model(external_model)
}

pub fn parse_adam_expression(str_expression: &str) -> Result<Expression, crate::expression_parser::ParseError> {
    use crate::lexer::Lexer;
    use crate::expression_parser::Parser;

    let mut lexer = Lexer::new(str_expression);
    lexer.lex()?;
    
    let mut parser = Parser::new(lexer.tokens);
    parser.parse_expression()
} 