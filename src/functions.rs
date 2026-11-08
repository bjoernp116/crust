use crate::{
    error::{TypeResult}, parser::TypeSyntax, types::{TypeHandler, TypeID},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FuncID(pub usize);

#[derive(Debug, Clone)]
pub struct FuncSignature {
    pub identifier: String,
    pub params: Vec<TypeID>,
    pub ret: TypeID,
    pub big_ret: bool,
}

impl FuncSignature {
    pub fn new(
        identifier: String,
        params: Vec<TypeID>,
        ret: TypeID,
        type_handler: &TypeHandler,
    ) -> TypeResult<FuncSignature> {
        let big_ret = !type_handler.is_scalar(&ret)?;
        Ok(FuncSignature {
            identifier,
            params,
            ret,
            big_ret,
        })
    }

    pub fn transform(&mut self, type_handler: &mut TypeHandler) -> TypeResult<()> {
        if !self.big_ret {
            Ok(())
        } else {
            let ret_ty = type_handler.get(&self.ret, None)?;
            let syntax = TypeSyntax::Reference {
                mutable: true,
                pointee: Box::new(TypeSyntax::Raw(ret_ty.identifier.clone())),
            };
            let ret_ref = type_handler.lookup_or_define(syntax).unwrap();
            self.params.insert(0, ret_ref);
            self.ret = TypeID::VOID;
            Ok(())
        }
    }
}

#[derive(Debug)]
pub struct FuncTable {
    functions: Vec<FuncSignature>,
    current: Option<FuncID>,
}

impl FuncTable {
    pub fn new(th: &mut TypeHandler) -> Self {
        let mut functions = Vec::new();
        let ref_u8 = th.u8_ref();
        functions.push(FuncSignature {
            identifier: "crust_write".to_owned(),
            params: vec![ref_u8, TypeID::U64],
            ret: TypeID::I64,
            big_ret: false,
        });
        functions.push(FuncSignature {
            identifier: "crust_exit".to_owned(),
            params: vec![TypeID::U64],
            ret: TypeID::NEVER,
            big_ret: false,
        });
        functions.push(FuncSignature {
            identifier: "crust_mmap".to_owned(),
            params: vec![TypeID::USIZE],
            ret: ref_u8,
            big_ret: false,
        });
        functions.push(FuncSignature {
            identifier: "crust_munmap".to_owned(),
            params: vec![ref_u8, TypeID::USIZE],
            ret: TypeID::VOID,
            big_ret: false,
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
