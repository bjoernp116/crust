
#[derive(Debug, Clone)]
pub struct Place {
    pub kind: PlaceKind,
    pub offset: usize,
    pub typeid: TypeID,
}

impl Place {
    pub fn offset(&self, offset: usize, typeid: TypeID) -> Self {
        Place {
            kind: self.kind.clone(),
            offset: self.offset + offset,
            typeid,
        }
    }

    pub fn is_pointer(&self) -> bool {
        match self.kind.clone() {
            PlaceKind::Slot(_) => false,
            PlaceKind::Pointer(_) => true,
            PlaceKind::Index { base, index } => false,
        }
    }
}

impl Display for Place {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind.clone() {
            PlaceKind::Slot(s) => write!(f, "[s{} + {}]", s.0, self.offset),
            PlaceKind::Pointer(v) => write!(f, "[*{} + {}]", v, self.offset),
            PlaceKind::Index { base, index } => write!(f, "[{} + {}]", base, index),
        }
    }
}

#[derive(Debug, Clone)]
pub enum PlaceKind {
    Slot(SlotID),
    Pointer(ValueID),
    Index{
        base: Box<Place>,
        index: ValueID
    }
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct SlotID(pub usize);

impl SlotID {
    pub const MAX: SlotID = SlotID(usize::MAX);
}

#[derive(Debug, Clone)]
pub struct StackFrame {
    pub stack_map: HashMap<SlotID, StackLocation>,
    pub size: usize,
}

#[derive(Debug)]
pub struct SlotTable {
    slots: Vec<Slot>,
    bindings: HashMap<SymbolID, SlotID>,
    value_counter: usize,
}

impl SlotTable {
    pub fn new() -> SlotTable {
        SlotTable {
            slots: Vec::new(),
            value_counter: 0,
            bindings: HashMap::new(),
        }
    }

    pub fn new_var(&mut self, type_id: TypeID, symbol_id: SymbolID) -> SlotID {
        let slot_id = SlotID(self.slots.len());
        self.slots.push(Slot {
            ty: type_id,
            dropped: false,
        });
        self.bindings.insert(symbol_id, slot_id);
        slot_id
    }

    pub fn new_temp(&mut self, type_id: TypeID) -> SlotID {
        let slot_id = SlotID(self.slots.len());
        self.slots.push(Slot {
            ty: type_id,
            dropped: false,
        });
        slot_id
    }

    pub fn get(&self, id: SlotID) -> &Slot {
        &self.slots[id.0]
    }
    pub fn get_mut(&mut self, id: SlotID) -> &mut Slot {
        &mut self.slots[id.0]
    }
    pub fn lookup(&self, id: &SymbolID) -> Option<&SlotID> {
        self.bindings.get(id)
    }

    pub fn all_slots(&self) -> Vec<(SlotID, Slot)> {
        self.slots
            .iter()
            .enumerate()
            .map(|(i, s)| (SlotID(i), s.clone()))
            .collect()
    }

    pub fn stack_frame(&self, type_handler: &TypeHandler) -> StackFrame {
        let mut map = HashMap::new();
        let mut used = 0;
        for (i, slot) in self.slots.iter().enumerate() {
            if slot.ty == TypeID::VOID || slot.dropped {
                continue;
            }
            let ty = type_handler.get(&slot.ty, None).unwrap();
            let alignment = ty.size;
            if alignment == 0 {
                continue;
            }
            used = align_up(used, alignment);

            used += ty.size;
            let location = StackLocation {
                offset: used,
                size: ty.size,
                pointer: false,
                type_id: slot.ty,
            };
            map.insert(SlotID(i), location);
        }
        used += 16 - (used % 16);
        StackFrame {
            stack_map: map,
            size: used,
        }
    }
}

pub fn align_up(value: usize, alignment: usize) -> usize {
    (value + alignment - 1) & !(alignment - 1)
}

#[derive(Debug, Clone)]
pub struct Slot {
    pub ty: TypeID,
    pub dropped: bool,
}

