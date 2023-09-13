use std::{cell::RefCell, collections::HashMap, ops::Deref, rc::Rc};

use crate::{
    ast::{Item, Param, Type},
    token::Token,
};

#[derive(Debug)]
#[allow(dead_code)]
struct Function {
    arity: usize,
    ty: Type,
    params: Vec<Param>,
}

#[derive(Debug)]
enum Symbol {
    Function(Function),
    Variable(Type),
}

#[derive(Debug)]
pub struct SymbolTable {
    arena: Vec<HashMap<String, Symbol>>,
    current: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            arena: vec![HashMap::new()],
            current: 0,
        }
    }

    fn begin_scope(&mut self) {
        if self.current + 1 == self.arena.len() {
            self.arena.push(HashMap::new());
        }
        self.current += 1;
    }

    fn end_scope(&mut self) {
        self.current -= 1;
    }

    fn current(&self) -> &HashMap<String, Symbol> {
        &self.arena[self.current]
    }

    fn current_mut(&mut self) -> &mut HashMap<String, Symbol> {
        &mut self.arena[self.current]
    }

    fn get(&self, name: &String) -> Option<&Symbol> {
        self.current().get(name)
    }

    fn insert(&mut self, name: String, symbol: Symbol) {
        self.current_mut().insert(name, symbol);
    }
}

impl From<&Item> for SymbolTable {
    fn from(item: &Item) -> Self {
        let mut table = Self::new();
        self::SymbolTableVisitor::visit(item, &mut table);
        table
    }
}

trait SymbolTableVisitor {
    fn visit(&self, table: &mut SymbolTable);
}

impl SymbolTableVisitor for Item {
    fn visit(&self, table: &mut SymbolTable) {
        match self {
            Item::Program(items) => items
                .iter()
                .for_each(|item| self::SymbolTableVisitor::visit(item, table)),
            Item::Block(items) => items
                .iter()
                .for_each(|item| self::SymbolTableVisitor::visit(item, table)),
            Item::Function(name, params, ty, body) => {
                table.begin_scope();
                for param in params {
                    table.insert(param.name.clone(), Symbol::Variable(param.ty.clone()));
                }
                self::SymbolTableVisitor::visit(body.deref(), table);
                table.end_scope();
                table.insert(
                    name.clone(),
                    Symbol::Function(Function {
                        arity: params.len(),
                        ty: ty.clone(),
                        params: params.clone(),
                    }),
                );
            }
            Item::VarDecl(name, ty, _, _) => {
                table.insert(name.clone(), Symbol::Variable(ty.clone()))
            }
            _ => {}
        }
    }
}

pub struct TypeCheckPass<'a> {
    root: &'a Item,
    table: Rc<RefCell<SymbolTable>>,
    #[allow(dead_code)]
    errored: bool,
    function: Option<Function>,
}

impl<'a> TypeCheckPass<'a> {
    pub fn new(table: SymbolTable, root: &'a Item) -> Self {
        Self {
            root,
            table: Rc::new(RefCell::new(table)),
            errored: false,
            function: None,
        }
    }

    pub fn run(&mut self) {
        self.check(self.root, Rc::clone(&self.table));
    }

    fn error(&mut self, at: &Token, msg: String) {
        eprintln!("{} at {}", msg, at.span);
        self.errored = true;
    }

    fn check(&mut self, item: &Item, table: Rc<RefCell<SymbolTable>>) -> Type {
        match item {
            Item::Program(items) => validators::program(self, items, table),
            Item::Block(items) => validators::block(self, items, table),
            Item::Function(_, params, ty, body) => {
                validators::function(self, params, ty, body, table)
            }
            Item::Binary(left, op, right) => validators::binary(self, left, op, right, table),
            Item::Ident(name, token) => validators::ident(self, name, token, table),
            Item::VarDecl(_, expected, init, token) => {
                validators::var(self, expected, init, token, table)
            }
            Item::Call(name, params, token) => validators::call(self, name, params, token, table),
            Item::Return(expr, token) => validators::ret(self, expr, token, table),
            Item::Float(_) => Type::Float,
            Item::Int(_) => Type::Int,
            Item::Bool(_) => Type::Bool,
            Item::Str(_) => Type::Str,
            e => unimplemented!("{:?}", e),
        }
    }
}

mod validators {
    use std::{cell::RefCell, ops::Deref, rc::Rc};

    use crate::{
        ast::{Item, Param, Type},
        token::Token,
    };

    use super::{Function, Symbol, SymbolTable, TypeCheckPass};

    pub fn program(
        tc: &mut TypeCheckPass,
        items: &Vec<Item>,
        table: Rc<RefCell<SymbolTable>>,
    ) -> Type {
        for item in items {
            tc.check(item, Rc::clone(&table));
        }
        Type::Void
    }

    pub fn block(
        tc: &mut TypeCheckPass,
        items: &Vec<Item>,
        table: Rc<RefCell<SymbolTable>>,
    ) -> Type {
        for item in items {
            tc.check(item, Rc::clone(&table));
        }
        Type::Void
    }

    pub fn function(
        tc: &mut TypeCheckPass,
        params: &Vec<Param>,
        ty: &Type,
        body: &Item,
        table: Rc<RefCell<SymbolTable>>,
    ) -> Type {
        tc.function = Some(Function {
            arity: params.len(),
            ty: ty.clone(),
            params: params.clone(),
        });
        table.borrow_mut().begin_scope();
        tc.check(body.deref(), Rc::clone(&table));
        table.borrow_mut().end_scope();
        tc.function = None;
        ty.clone()
    }

    pub fn binary(
        tc: &mut TypeCheckPass<'_>,
        left: &Item,
        op: &Token,
        right: &Item,
        table: Rc<RefCell<SymbolTable>>,
    ) -> Type {
        let lty = tc.check(left.deref(), Rc::clone(&table));
        let rty = tc.check(right.deref(), Rc::clone(&table));
        if lty != rty {
            tc.error(
                op,
                format!(
                    "Mismatched types in '{}' operation ({:?}, {:?})",
                    op, lty, rty
                ),
            );
        }
        lty
    }

    pub fn call(
        tc: &mut TypeCheckPass<'_>,
        name: &Item,
        params: &[Item],
        token: &Token,
        table: Rc<RefCell<SymbolTable>>,
    ) -> Type {
        let name = match name.deref() {
            Item::Ident(name, _) => name,
            _ => unimplemented!(),
        };
        let borrowed = table.borrow();
        let symbol = borrowed.get(name);
        let function = match symbol {
            Some(Symbol::Function(function)) => function,
            _ => {
                tc.error(token, format!("{} is not a function", name));
                return Type::Void;
            }
        };
        if function.arity != params.len() {
            tc.error(
                token,
                format!(
                    "Expected {} arguments, got {}",
                    function.arity,
                    params.len()
                ),
            );
        }
        for (i, param) in params.iter().enumerate() {
            let ty = tc.check(param, Rc::clone(&table));
            if ty != function.params[i].ty {
                tc.error(
                    token,
                    format!(
                        "Mismatched type in function parameter: (expected {:?}, got {:?}",
                        function.params[i].ty, ty
                    ),
                );
            }
        }
        function.ty.clone()
    }

    pub fn var(
        tc: &mut TypeCheckPass<'_>,
        expected: &Type,
        init: &Item,
        token: &Token,
        table: Rc<RefCell<SymbolTable>>,
    ) -> Type {
        let got = tc.check(init, Rc::clone(&table));
        if *expected != got {
            tc.error(
                token,
                format!(
                    "Mismatched types in variable declaration (expected {:?}, got {:?})",
                    expected, got
                ),
            );
        }
        Type::Void
    }

    pub fn ident(
        tc: &mut TypeCheckPass<'_>,
        name: &String,
        token: &Token,
        table: Rc<RefCell<SymbolTable>>,
    ) -> Type {
        match table.borrow().get(name) {
            Some(Symbol::Variable(ty)) => ty.clone(),
            _ => {
                tc.error(token, format!("{} is not a variable", name));
                Type::Void
            }
        }
    }

    pub fn ret(
        tc: &mut TypeCheckPass<'_>,
        expr: &Item,
        token: &Token,
        table: Rc<RefCell<SymbolTable>>,
    ) -> Type {
        let ty = tc.check(expr.deref(), table);
        match tc.function {
            Some(ref function) => {
                if ty != function.ty {
                    tc.error(
                        token,
                        format!(
                            "Mismatched types in return (expected {:?}, got {:?})",
                            function.ty, ty
                        ),
                    );
                }
            }
            None => tc.error(token, "Return outside of function".into()),
        }
        ty
    }
}
