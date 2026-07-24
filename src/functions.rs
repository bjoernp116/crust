use crate::types::TypeID;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FuncID(pub usize);

#[derive(Debug, Clone)]
pub struct FuncSignature {
    pub identifier: String,
    pub params: Vec<TypeID>,
    pub ret: TypeID,
}


#[derive(Debug)]
pub struct FuncTable {
    functions: Vec<FuncSignature>,
    current: Option<FuncID>,
}

impl FuncTable {
    pub fn new() -> Self {
        Self { functions: Vec::new(), current: None }
    }

    pub fn insert(
        &mut self,
        signature: FuncSignature,
    ) -> FuncID {
        let id = FuncID(self.functions.len());
        self.functions.push(signature);
        id
    }

    pub fn get(&self, id: &FuncID) -> &FuncSignature {
        &self.functions[id.0]
    }
    
    pub fn current(&self) -> Option<&FuncSignature> {
        if let Some(id) = self.current {
            Some(self.get(&id))
        } else {
            None
        }
    }

    pub fn enter_func(&mut self, id: FuncID) {
        self.current = Some(id);
    }

    pub fn clear_current(&mut self) {
        self.current = None;
    }
}
