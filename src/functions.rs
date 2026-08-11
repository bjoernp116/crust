use crate::{
    parser::TypeSyntax,
    types::{TypeHandler, TypeID},
};

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
    pub fn new(th: &mut TypeHandler) -> Self {
        let mut functions = Vec::new();
        let ref_u8 = th.lookup_or_define(TypeSyntax::Reference {
            mutable: false,
            pointee: Box::new(TypeSyntax::Raw("u8".to_owned())),
        }).unwrap();
        functions.push(FuncSignature {
            identifier: "crust_write".to_owned(),
            params: vec![ref_u8, TypeID::U64],
            ret: TypeID::I64,
        });
        functions.push(FuncSignature {
            identifier: "crust_exit".to_owned(),
            params: vec![TypeID::U64],
            ret: TypeID::NEVER,
        });
        Self {
            functions,
            current: None,
        }
    }

    pub fn insert(&mut self, signature: FuncSignature) -> FuncID {
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
