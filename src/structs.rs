use std::collections::HashMap;

use crate::{
    error::{ResError, ResErrorKind, ResResult}, lexer::Position, locations::StackLocation, parser::TypeSyntax, ssa::{StackFrame, align_up}, types::{TypeHandler, TypeID}
};

#[derive(Debug, Clone)]
pub struct Field {
    ty: TypeID,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldID(pub usize);

#[derive(Debug, Clone)]
pub struct Struct {
    identifier: String,
    fields: Vec<Field>,
    bindings: HashMap<String, FieldID>,
}

impl Struct {
    fn define_field(&mut self, ident: String, type_id: TypeID) -> FieldID {
        let id = FieldID(self.fields.len());
        let field = Field { ty: type_id };
        self.fields.push(field);
        self.bindings.insert(ident, id);
        id
    }
    pub fn lookup(&self, ident: &String, pos: Option<Position>) -> ResResult<FieldID> {
        self.bindings.get(ident).map(|f| f.clone()).ok_or(ResError {
            kind: ResErrorKind::UnknownField(ident.clone(), self.identifier.clone()),
            position: pos,
            severity: crate::error::Severity::Error,
        })
    }
    pub fn get_type(&self, field_id: FieldID) -> TypeID {
        self.fields[field_id.0].ty
    }
    pub fn get_offset(&self, field_id: FieldID, type_handler: &TypeHandler) -> usize {
        self.frame(type_handler).stack_map[&field_id].offset
    }
    pub fn frame(&self, type_handler: &TypeHandler) -> StructFrame {
        let mut map = HashMap::new();
        let mut used = 0;
        for (i, field) in self.fields.iter().enumerate() {
            if field.ty == TypeID::VOID {
                continue;
            }
            let ty = type_handler.get(&field.ty, None).unwrap();

            let alignment = ty.size;
            
            used = align_up(used, alignment);

            let location = StackLocation {
                offset: used,
                size: ty.size,
                pointer: false,
            };
            map.insert(FieldID(i), location);
            used += ty.size;
        }
        if used % 8 != 0 {
            used += 8 - (used % 8);
        }
        println!("{:?}", map);
        StructFrame {
            stack_map: map,
            size: used,
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructFrame {
    pub stack_map: HashMap<FieldID, StackLocation>,
    pub size: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructID(pub usize);

#[derive(Debug)]
pub struct StructTable {
    structs: Vec<Struct>,
    bindings: HashMap<String, StructID>,
    field_counter: usize,
}

impl StructTable {
    pub fn new(type_handler: &mut TypeHandler) -> Self {
        let mut str_struct = Struct {
            identifier: String::from("Str"),
            fields: Vec::new(),
            bindings: HashMap::new(),
        };
        str_struct.define_field(
            String::from("buffer"),
            type_handler.lookup_or_define(TypeSyntax::Reference {
                mutable: false,
                pointee: Box::new(TypeSyntax::Raw("u8".to_owned())),
            }).unwrap(),
        );
        str_struct.define_field(String::from("length"), TypeID::U64);
        Self {
            structs: vec![str_struct],
            bindings: HashMap::new(),
            field_counter: 0,
        }
    }
    pub fn define(&mut self, identifier: String) -> StructID {
        let id = StructID(self.structs.len());
        let struc = Struct {
            identifier: identifier.clone(),
            fields: Vec::new(),
            bindings: HashMap::new(),
        };
        self.structs.push(struc);
        self.bindings.insert(identifier, id);
        id
    }
    pub fn get(&self, id: StructID) -> Option<Struct> {
        self.structs.get(id.0).map(|s| s.clone())
    }
    pub fn get_mut(&mut self, id: StructID) -> Option<&mut Struct> {
        self.structs.get_mut(id.0)
    }

    pub fn define_fields(
        &mut self,
        id: StructID,
        args: Vec<(String, TypeSyntax)>,
        type_handler: &mut TypeHandler,
    ) {
        let struc = self.structs.get_mut(id.0).unwrap();
        for (ident, type_ident) in args {
            let type_id = type_handler.lookup_or_define(type_ident).unwrap();
            struc.define_field(ident, type_id);
        }
        let frame = struc.frame(type_handler);
        let type_id = type_handler
            .lookup(TypeSyntax::Raw(struc.identifier.clone()))
            .unwrap();
        let type_mut = type_handler.get_mut(&type_id, None).unwrap();
        type_mut.size = frame.size;
    }
}
