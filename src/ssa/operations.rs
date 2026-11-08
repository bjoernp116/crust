use std::fmt::Display;

use crate::{functions::FuncID, parser::Litteral, ssa::{places::Place, values::ValueID, values_to_string}, types::TypeID};


#[derive(Debug, Clone)]
pub enum Operation {
    AddressOf(ValueID, Place),
    Load(ValueID, Place),
    Store(Place, ValueID),
    Move(ValueID, ValueID),
    Cast(ValueID, ValueID),
    Deref(ValueID, ValueID),
    Copy(Place, Place, TypeID),
    Const(ValueID, Litteral),
    ConstStr(ValueID, String),
    HugeConst(Place, Litteral),
    Eq(TypeID, ValueID, ValueID, ValueID),
    G(TypeID, ValueID, ValueID, ValueID),
    L(TypeID, ValueID, ValueID, ValueID),
    GEq(TypeID, ValueID, ValueID, ValueID),
    LEq(TypeID, ValueID, ValueID, ValueID),
    NEq(TypeID, ValueID, ValueID, ValueID),
    Add(TypeID, ValueID, ValueID, ValueID),
    Sub(TypeID, ValueID, ValueID, ValueID),
    Div(TypeID, ValueID, ValueID, ValueID, bool),
    Mul(TypeID, ValueID, ValueID, ValueID),
    Not(TypeID, ValueID, ValueID),
    Neg(TypeID, ValueID, ValueID),
    Call(Option<ValueID>, FuncID, Vec<ValueID>),
    PointerOffset(ValueID, ValueID, ValueID),
    NOP,
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Move(dest, src) => write!(f, "{} = move {}", dest, src),
            Self::Load(dest, src) => write!(f, "{} = load {}", dest, src),
            Self::Store(dest, src) => write!(f, "{} = store {}", dest, src),
            Self::Deref(dest, src) => write!(f, "{} = *{}", dest, src),
            Self::AddressOf(dest, src) => write!(f, "{} = &{}", dest, src),
            Self::Cast(dest, src) => write!(f, "{} = cast {}", dest, src),
            Self::Copy(dest, src, _) => write!(f, "{} = copy {}", dest, src),
            Self::Const(dest, src) => write!(f, "{} = {}", dest, src),
            Self::HugeConst(dest, src) => write!(f, "{} = for all {}", dest, src),
            Self::ConstStr(dest, src) => write!(f, "{} = {:?}", dest, src),
            Self::Call(Some(dest), id, src) => write!(
                f,
                "{} = call f{}({})",
                dest,
                id.0,
                values_to_string(src.clone())
            ),
            Self::Call(None, id, src) => {
                write!(f, "call f{}({})", id.0, values_to_string(src.clone()))
            }
            Self::Add(_, dest, s1, s2) => write!(f, "{} = {} + {}", dest, s1, s2),
            Self::Sub(_, dest, s1, s2) => write!(f, "{} = {} - {}", dest, s1, s2),
            Self::Mul(_, dest, s1, s2) => write!(f, "{} = {} * {}", dest, s1, s2),
            Self::Div(_, dest, s1, s2, false) => write!(f, "{} = {} / {}", dest, s1, s2),
            Self::Div(_, dest, s1, s2, true) => write!(f, "{} = {} % {}", dest, s1, s2),
            Self::Eq(_, dest, s1, s2) => write!(f, "{} = {} == {}", dest, s1, s2),
            Self::NEq(_, dest, s1, s2) => write!(f, "{} = {} != {}", dest, s1, s2),
            Self::LEq(_, dest, s1, s2) => write!(f, "{} = {} <= {}", dest, s1, s2),
            Self::GEq(_, dest, s1, s2) => write!(f, "{} = {} >= {}", dest, s1, s2),
            Self::G(_, dest, s1, s2) => write!(f, "{} = {} > {}", dest, s1, s2),
            Self::L(_, dest, s1, s2) => write!(f, "{} = {} < {}", dest, s1, s2),
            Self::Not(_, dest, src) => write!(f, "{} = !{}", dest, src),
            Self::Neg(_, dest, src) => write!(f, "{} = -{}", dest, src),
            Self::PointerOffset(dest, src, offset) => {
                write!(f, "{} = [{} + {}]", dest, src, offset)
            }
            Self::NOP => write!(f, "NOP"),
        }
    }
}

impl Operation {
    pub fn get_dest_value(&self) -> Option<&ValueID> {
        use Operation::*;
        match &self {
            AddressOf(dest, _) => Some(dest),
            Load(dest, _) => Some(dest),
            Move(dest, _) => Some(dest),
            Cast(dest, _) => Some(dest),
            Deref(dest, _) => Some(dest),
            Const(dest, _) => Some(dest),
            ConstStr(dest, _) => Some(dest),
            Eq(_, dest, _, _) => Some(dest),
            NEq(_, dest, _, _) => Some(dest),
            GEq(_, dest, _, _) => Some(dest),
            LEq(_, dest, _, _) => Some(dest),
            L(_, dest, _, _) => Some(dest),
            G(_, dest, _, _) => Some(dest),
            Add(_, dest, _, _) => Some(dest),
            Sub(_, dest, _, _) => Some(dest),
            Mul(_, dest, _, _) => Some(dest),
            Div(_, dest, _, _, _) => Some(dest),
            Not(_, dest, _) => Some(dest),
            Neg(_, dest, _) => Some(dest),
            Call(Some(dest), _, _) => Some(dest),
            PointerOffset(dest, _, _) => Some(dest),
            _ => None,
        }
    }
    pub fn get_dest_place(&self) -> Option<&Place> {
        use Operation::*;
        match &self {
            Copy(dest, _, _) => Some(dest),
            Store(dest, _) => Some(dest),
            _ => None,
        }
    }
    pub fn get_src_values(&self) -> Vec<ValueID> {
        use Operation::*;
        match &self {
            Store(_, src) => vec![src.clone()],
            Move(_, src) => vec![src.clone()],
            Cast(_, src) => vec![src.clone()],
            Deref(_, src) => vec![src.clone()],
            Eq(_, _, s1, s2) => vec![s1.clone(), s2.clone()],
            NEq(_, _, s1, s2) => vec![s1.clone(), s2.clone()],
            GEq(_, _, s1, s2) => vec![s1.clone(), s2.clone()],
            LEq(_, _, s1, s2) => vec![s1.clone(), s2.clone()],
            G(_, _, s1, s2) => vec![s1.clone(), s2.clone()],
            L(_, _, s1, s2) => vec![s1.clone(), s2.clone()],
            Add(_, _, s1, s2) => vec![s1.clone(), s2.clone()],
            Sub(_, _, s1, s2) => vec![s1.clone(), s2.clone()],
            Mul(_, _, s1, s2) => vec![s1.clone(), s2.clone()],
            Div(_, _, s1, s2, _) => vec![s1.clone(), s2.clone()],
            Not(_, _, src) => vec![src.clone()],
            Neg(_, _, src) => vec![src.clone()],
            Call(_, _, src) => src.clone(),
            PointerOffset(_, base, offset) => vec![base.clone(), offset.clone()],
            _ => Vec::new(),
        }
    }
    pub fn get_src_place(&self) -> Option<Place> {
        use Operation::*;
        match &self {
            Load(_, src) => Some(src.clone()),
            Copy(_, src, _) => Some(src.clone()),
            AddressOf(_, src) => Some(src.clone()),
            _ => None,
        }
    }
}


