use crate::{
    ast::{node, Decl},
    builtins,
};
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

#[derive(Debug, Clone)]
pub enum Symbol {
    Class(Rc<node::ClassDecl>),
    Function(Rc<node::FuncDecl>),
    Constant(Rc<node::ConstantDecl>),
    Variable(Rc<node::VarDecl>),
    Opaque(String),
    Str,
    Int,
    Float,
    Bool,
    Void,
}

impl Symbol {
    pub fn class(&self) -> Option<&node::ClassDecl> {
        match self {
            Symbol::Class(cls) => Some(cls),
            _ => None,
        }
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self, Symbol::Class(_) | Symbol::Str)
    }

    pub fn function(&self) -> &node::FuncDecl {
        match self {
            Symbol::Function(fun) => fun,
            _ => panic!("called `Symbol::function()` on a non-function symbol"),
        }
    }

    pub fn has_subclass<'a>(cls: &'a node::ClassDecl, symbols: &'a SymbolTable) -> bool {
        symbols.values().any(|symbol| {
            matches!(symbol, Symbol::Class(subclass) if subclass
            .parent
            .as_ref()
            .is_some_and(|parent| parent.lexeme == cls.name.lexeme))
        })
    }

    pub fn superclasses<'a>(
        mut cls: &'a node::ClassDecl,
        symbols: &'a SymbolTable,
    ) -> Vec<&'a node::ClassDecl> {
        let mut classes = vec![cls];
        while let Some(parent) = cls.parent.as_ref() {
            cls = symbols.get(&parent.to_string()).unwrap().class().unwrap();
            classes.push(cls);
        }
        classes
    }

    pub fn fields(&self, symbols: &SymbolTable) -> Vec<node::Field> {
        let cls = self.class().unwrap();
        let superclasses = Self::superclasses(cls, symbols);
        superclasses
            .iter()
            .rev()
            .flat_map(|cls| cls.fields.iter().cloned())
            .collect()
    }

    pub fn methods(&self, symbols: &SymbolTable) -> Vec<(String, Rc<node::FuncDecl>)> {
        let cls = self.class().unwrap();
        let superclasses = Self::superclasses(cls, symbols);
        let mut used = HashSet::new();
        let mut methods = vec![];
        for c in superclasses.iter().rev() {
            for method in &c.methods {
                let active = superclasses
                    .iter()
                    .find_map(|cls| {
                        cls.methods
                            .iter()
                            .find(|m| m.name == method.name)
                            .map(|m| format!("{}.{}", cls.name, m.name))
                    })
                    .unwrap_or_else(|| panic!("expected to find method {}", method.name));
                let (_, n) = active.rsplit_once('.').unwrap();
                if !used.contains(n) {
                    methods.push((active.clone(), Rc::clone(method)));
                    used.insert(n.to_string());
                }
            }
        }
        methods
    }

    pub fn descriptor(&self, symbols: &SymbolTable) -> (String, Vec<String>) {
        let methods = self
            .methods(symbols)
            .iter()
            .map(|(label, _)| label)
            .cloned()
            .collect();
        let fields = self
            .fields(symbols)
            .iter()
            .map(|f| match f.ty.base.lexeme.unwrap() {
                "int" | "float" | "bool" => 'i',
                "void" => panic!("class cannot contain void field"),
                _ => 'p',
            })
            .collect();
        (fields, methods)
    }
}

crate::newtype!(SymbolTable:HashMap<String, Symbol>);

impl From<&Vec<Decl>> for SymbolTable {
    fn from(nodes: &Vec<Decl>) -> Self {
        let mut table: Self = Self(
            builtins::builtins()
                .nodes
                .iter()
                .map(ToTuple::to_tuple)
                .collect(),
        );
        table.extend(nodes.iter().map(ToTuple::to_tuple));
        table
    }
}

trait ToTuple {
    fn to_tuple(&self) -> (String, Symbol);
}

impl ToTuple for Decl {
    fn to_tuple(&self) -> (String, Symbol) {
        match self {
            Decl::Function(fun) => fun.to_tuple(),
            Decl::Constant(c) => c.to_tuple(),
            Decl::Class(cls) => cls.to_tuple(),
        }
    }
}

impl ToTuple for Rc<node::FuncDecl> {
    fn to_tuple(&self) -> (String, Symbol) {
        (self.name.to_string(), Symbol::Function(Rc::clone(self)))
    }
}

impl ToTuple for Rc<node::ConstantDecl> {
    fn to_tuple(&self) -> (String, Symbol) {
        (self.name.to_string(), Symbol::Constant(Rc::clone(self)))
    }
}

impl ToTuple for Rc<node::ClassDecl> {
    fn to_tuple(&self) -> (String, Symbol) {
        (self.name.to_string(), Symbol::Class(Rc::clone(self)))
    }
}
