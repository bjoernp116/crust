use std::collections::HashMap;

use crate::{functions::FuncID, types::InferedType};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SymbolID(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ScopeID(pub usize);

#[derive(Clone, Debug)]
pub struct Symbol {
    pub identifier: String,
    pub typeid: InferedType,
}

#[derive(Debug)]
pub struct SymbolTable {
    symbols: Vec<Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { symbols: Vec::new() }
    }

    pub fn insert(&mut self, symbol: Symbol) -> SymbolID {
        let id = SymbolID(self.symbols.len());
        self.symbols.push(symbol);
        id
    }

    pub fn get(&mut self, id: SymbolID) -> &Symbol {
        &self.symbols[id.0]
    }

    pub fn get_mut(&mut self, id: SymbolID) -> &mut Symbol {
        &mut self.symbols[id.0]
    }
}

#[derive(Clone, Debug, Copy)]
pub enum Binding {
    Variable(SymbolID),
    Function(FuncID)
}

#[derive(Clone, Debug)]
pub struct Scope {
    pub parent: Option<ScopeID>,
    bindings: HashMap<String, Binding>,
}

impl Scope {
    pub fn new_global() -> Self {
        Self {
            parent: None,
            bindings: HashMap::new(),
        }
    }
    pub fn new(parent: ScopeID) -> Self {
        Self {
            parent: Some(parent),
            bindings: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct ScopeTable {
    scopes: Vec<Scope>,
    current_scope: ScopeID,
}

impl ScopeTable {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new_global()],
            current_scope: ScopeID(0),
        }
    }

    fn create(&mut self, parent: ScopeID) -> ScopeID {
        let id = ScopeID(self.scopes.len());
        self.scopes.push(Scope::new(parent));
        id
    }

    pub fn push_scope(&mut self) -> ScopeID {
        let parent = self.current_scope;
        let child = self.create(parent);
        self.current_scope = child;
        parent
    }

    pub fn pop_scope(&mut self) {
        let parent = self.scopes[self.current_scope.0]
            .parent
            .expect("cant leave global scope");

        self.current_scope = parent;
    }
    
    pub fn lookup(&mut self, identifier: String) -> Option<Binding> {
        let mut scope_id = Some(self.current_scope);

        while let Some(id) = scope_id {
            let scope = &self.scopes[id.0];

            if let Some(binding) = scope.bindings.get(&identifier) {
                return Some(*binding);
            }

            scope_id = scope.parent;
        }

        None
    }

    pub fn insert(&mut self, identifier: String, id: Binding) {
        self.scopes[self.current_scope.0].bindings.insert(identifier, id);
    }
}
