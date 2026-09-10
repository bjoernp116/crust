use std::collections::{HashMap, HashSet};

use crate::{
    asm::ARG_REGISTERS, error::Counter, functions::{self, FuncID}, locations::{Locator, RegisterSet}, parser::Litteral, ssa::{
        BasicBlock, BlockLabel, Function, Operation, Place, PlaceKind, SlotID, SlotTable,
        Terminator, ValueID,
    }, types::{Tables, TypeID}
};

#[derive(Debug, Clone)]
pub struct ValueInfo {
    pub ty: TypeID,
    pub prefered_registers: RegisterSet,
    pub survive_calls: bool,
    pub writes: usize,
    pub reads: usize,
    pub symbol: bool,
    pub input_arg: Option<usize>,
    pub output_arg: Vec<(FuncID, usize)>,
    pub blocks: HashSet<BlockLabel>,
}

impl ValueInfo {
    pub fn new(ty: TypeID) -> Self {
        Self {
            ty,
            prefered_registers: RegisterSet::empty(),
            survive_calls: false,
            writes: 0,
            reads: 0,
            symbol: false,
            input_arg: None,
            output_arg: Vec::new(),
            blocks: HashSet::new(),
        }
    }
}

#[derive(Debug)]
pub struct SlotInfo {
    ty: TypeID,

    reads: usize,
    writes: usize,

    address_taken: bool,
    passed_by_ref: bool,
    used_offsets: HashSet<usize>,

    blocks: HashSet<BlockLabel>,
    written_after_read: bool,

    copied: bool,
}

impl SlotInfo {
    pub fn new(ty: TypeID) -> Self {
        Self {
            ty,
            reads: 0,
            writes: 0,

            address_taken: false,
            passed_by_ref: false,
            used_offsets: HashSet::new(),
            blocks: HashSet::new(),
            written_after_read: false,
            copied: false,
        }
    }
}

#[derive(Debug)]
pub struct FunctionAnalysis {
    pub value_info: HashMap<ValueID, ValueInfo>,
    pub slot_info: HashMap<SlotID, SlotInfo>,
    calls: Vec<HashSet<ValueID>>,
    value_replacements: HashMap<ValueID, ValueID>,
    slot_replacements: HashMap<SlotID, SlotID>,
    slot_to_values: HashMap<SlotID, ValueID>,
    slot_removals: HashSet<SlotID>,
    value_removals: HashSet<ValueID>,
    rematerialized_consts: HashMap<ValueID, Litteral>,
    current_block: Option<BlockLabel>,

    pub changed: bool,
}

impl FunctionAnalysis {
    pub fn analyze(function: &Function, tables: &Tables) -> Self {
        let mut analysis = Self {
            value_info: HashMap::new(),
            slot_info: HashMap::new(),
            calls: vec![HashSet::new()],
            value_replacements: HashMap::new(),
            slot_replacements: HashMap::new(),
            slot_to_values: HashMap::new(),
            slot_removals: HashSet::new(),
            value_removals: HashSet::new(),
            rematerialized_consts: HashMap::new(),
            current_block: None,
            changed: true,
        };
        for (id, value) in function.locator.all_values() {
            let info = ValueInfo::new(value.typeid);
            analysis.value_info.insert(id, info);
        }
        for (id, slot) in function.slots.all_slots() {
            let info = SlotInfo::new(slot.ty);
            analysis.slot_info.insert(id, info);
        }
        for (i, symbol) in function.args.iter().enumerate() {
            if let Some(value_id) = function.locator.lookup(&symbol) {
                let info = analysis.value_info.get_mut(value_id).unwrap();
                info.symbol = true;
                info.input_arg = Some(i);
                info.prefered_registers.insert(ARG_REGISTERS[i].clone());
                analysis.written_value(value_id);
            }
        }
        for block in &function.blocks {
            analysis.current_block = Some(block.label.clone());
            for operation in &block.operations {
                analysis.analyze_op(&operation, tables);
            }
            match &block.terminator {
                Terminator::Return { value: Some(src) } => {
                    analysis.read_value(&src);
                }
                Terminator::Branch {
                    predicate,
                    destination,
                    inverse,
                } => {
                    analysis.read_value(&predicate);
                }
                _ => (),
            }
        }
        analysis
    }
    fn analyze_op(&mut self, operation: &Operation, tables: &Tables) {
        match operation {
            Operation::Const(dest, _) => {
                self.written_value(dest);
            }
            Operation::ConstStr(dest, _) => {
                self.written_value(dest);
            }
            Operation::Load(dest, src) => {
                self.written_value(dest);
                self.read_place(src);
            }
            Operation::AddressOf(dest, src) => {
                self.written_value(dest);
                self.read_place(src);
                match src.kind {
                    PlaceKind::Slot(id) => {
                        self.slot_info.get_mut(&id).unwrap().address_taken = true;
                    }
                    _ => (),
                }
            }
            Operation::Deref(dest, src) => {
                self.written_value(dest);
                self.read_value(src);
            }
            Operation::Store(dest, src) => {
                self.read_value(src);
                self.written_place(dest);
            }
            Operation::Copy(dest, src, _) => {
                self.written_place(dest);
                self.read_place(src);
                if let PlaceKind::Slot(s) = src.kind {
                    self.slot_info.get_mut(&s).unwrap().copied = true;
                }
            }
            Operation::Call(dest_opt, func, values) => {
                for (i, id) in values.iter().enumerate() {
                    println!("{func:?}, {i}: {id}");
                    self.value_info.get_mut(&id).unwrap().output_arg.push((func.clone(), i));
                    self.value_info
                        .get_mut(&id)
                        .unwrap()
                        .prefered_registers
                        .insert(ARG_REGISTERS[i].clone());
                    self.read_value(id);
                }
                self.calls.push(HashSet::new());
                if let Some(dest) = dest_opt {
                    self.written_value(dest);
                }
            }
            Operation::Add(_, dest, s1, s2) => {
                self.written_value(dest);
                self.read_value(s1);
                self.read_value(s2);
            }
            Operation::Sub(_, dest, s1, s2) => {
                self.written_value(dest);
                self.read_value(s1);
                self.read_value(s2);
            }
            Operation::Mul(_, dest, s1, s2) => {
                self.written_value(dest);
                self.read_value(s1);
                self.read_value(s2);
            }
            Operation::Div(_, dest, s1, s2, _) => {
                self.written_value(dest);
                self.read_value(s1);
                self.read_value(s2);
            }
            Operation::Eq(_, dest, s1, s2) => {
                self.written_value(dest);
                self.read_value(s1);
                self.read_value(s2);
            }
            Operation::NEq(_, dest, s1, s2) => {
                self.written_value(dest);
                self.read_value(s1);
                self.read_value(s2);
            }
            Operation::G(_, dest, s1, s2) => {
                self.written_value(dest);
                self.read_value(s1);
                self.read_value(s2);
            }
            Operation::GEq(_, dest, s1, s2) => {
                self.written_value(dest);
                self.read_value(s1);
                self.read_value(s2);
            }
            Operation::L(_, dest, s1, s2) => {
                self.written_value(dest);
                self.read_value(s1);
                self.read_value(s2);
            }
            Operation::LEq(_, dest, s1, s2) => {
                self.written_value(dest);
                self.read_value(s1);
                self.read_value(s2);
            }
            Operation::Move(dest, src) => {
                self.written_value(dest);
                self.read_value(src);
            }
            Operation::Cast(dest, src) => {
                self.written_value(dest);
                self.read_value(src);
            }
            Operation::PointerOffset(dest, base, offset) => {
                self.written_value(dest);
                self.read_value(base);
                self.read_value(offset);
            }
            _ => (),
        }
    }

    fn written_value(&mut self, id: &ValueID) {
        let info = self.value_info.get_mut(&id).unwrap();
        info.writes += 1;
        if let Some(block) = self.current_block.clone() {
            info.blocks.insert(block);
        }
        self.calls.last_mut().unwrap().insert(id.clone());
    }

    fn written_place(&mut self, place: &Place) {
        match place.kind.clone() {
            PlaceKind::Slot(id) => {
                let info = self.slot_info.get_mut(&id).unwrap();
                info.used_offsets.insert(place.offset);
                if let Some(block) = self.current_block.clone() {
                    info.blocks.insert(block);
                }
                if info.reads > 0 {
                    info.written_after_read = true;
                }
                info.writes += 1;
            }
            PlaceKind::Pointer(id) => {
                self.written_value(&id);
            }
            PlaceKind::Index { base, index } => {
                self.written_place(&base);
                self.read_value(&index);
            }
        }
    }

    fn read_value(&mut self, id: &ValueID) {
        let info = self.value_info.get_mut(&id).unwrap();
        let mut cloned_call = self.calls.iter().clone();
        cloned_call.next_back();
        info.reads += 1;
        if let Some(block) = self.current_block.clone() {
            info.blocks.insert(block);
        }
        for call in cloned_call {
            if call.contains(&id) {
                info.survive_calls = true;
            }
        }
        self.calls.last_mut().unwrap().insert(id.clone());
    }

    fn read_place(&mut self, place: &Place) {
        match place.kind.clone() {
            PlaceKind::Slot(id) => {
                let info = self.slot_info.get_mut(&id).unwrap();
                info.used_offsets.insert(place.offset);
                if let Some(block) = self.current_block.clone() {
                    info.blocks.insert(block);
                }
                info.reads += 1;
            }
            PlaceKind::Pointer(id) => {
                self.read_value(&id);
            }
            PlaceKind::Index { base, index } => {
                self.read_place(&base);
                self.read_value(&index);
            }
        }
    }

    pub fn optimize(&mut self, function: &mut Function, tables: &Tables) {
        for block in &mut function.blocks {
            for operation in &block.operations {
                self.move_coalesc(&operation, tables);
            }
            self.apply_replacements(block);
            self.insert_rematerialized(block);
        }
        *self = FunctionAnalysis::analyze(function, tables);

        for block in &mut function.blocks {
            //self.remove_dead_code(block);
        }
        self.drop_unused_slots(function);
    }
    fn move_coalesc(&mut self, operation: &Operation, tables: &Tables) {
        match operation {
            Operation::Move(dest, src) => {
                let dest_info = self.value_info.get(&dest).unwrap();
                let src_info = self.value_info.get(&src).unwrap();
                if dest_info.writes == 1 && dest_info.reads == 1 && !dest_info.survive_calls {
                    if src_info.writes == 1 && src_info.reads == 1 && !src_info.survive_calls {
                        if dest_info.ty == src_info.ty {
                            self.value_replacements.insert(src.clone(), dest.clone());
                        }
                    }
                }
            }
            Operation::Const(dest, litt) => {
                let info = self.value_info.get(&dest).unwrap();
                if info.writes == 1 && info.reads == 1 && info.output_arg.len() == 1 {
                    self.rematerialized_consts.insert(dest.clone(), litt.clone());
                }
            }
            Operation::Copy(dest_place, src_place, _) => {
                if let PlaceKind::Slot(dest_id) = dest_place.kind {
                    if let PlaceKind::Slot(src_id) = src_place.kind {
                        let dest_info = self.slot_info.get(&dest_id).unwrap();
                        let src_info = self.slot_info.get(&src_id).unwrap();
                        let block_intersection: HashSet<_> =
                            dest_info.blocks.intersection(&src_info.blocks).collect();
                        if dest_info.writes <= 1 && dest_info.reads <= 1 {
                            if !src_info.written_after_read
                                && src_info.reads <= 1
                                && !src_info.address_taken
                            {
                                if src_info.ty == dest_info.ty && block_intersection.len() > 1 {
                                    if let Some(key) = self.is_replacing(&src_id) {
                                        let slot = self.slot_replacements.get_mut(&key).unwrap();
                                        *slot = dest_id;
                                    } else {
                                        self.slot_replacements
                                            .insert(src_id.clone(), dest_id.clone());
                                    }
                                }
                            }
                        }
                    }
                }
            }
            Operation::Store(dest, src) => {
                if let PlaceKind::Slot(dest_id) = dest.kind {
                    let dest_info = self.slot_info.get(&dest_id).unwrap();
                    let src_info = self.value_info.get(&src).unwrap();
                    let ty = tables.type_handler.get(&dest_info.ty, None).unwrap();
                    if ty.size <= 8
                        && !dest_info.written_after_read
                        && !dest_info.address_taken
                        && !dest_info.copied
                    {
                        if !src_info.survive_calls && src_info.writes == 1 && src_info.reads == 1 {
                            self.slot_to_values.insert(dest_id, src.clone());
                        }
                    }
                }
            }
            _ => (),
        }
    }

    fn apply_replacements(&mut self, block: &mut BasicBlock) {
        for operation in &mut block.operations {
            match operation {
                Operation::Store(dest, _) => {
                    if let PlaceKind::Slot(s) = dest.kind {
                        if let Some(new_dest) = self.slot_replacements.get(&s) {
                            self.slot_removals.insert(s);
                            dest.kind = PlaceKind::Slot(new_dest.clone());
                            self.changed = true;
                        }
                        if let Some(new_dest) = self.slot_to_values.get(&s) {
                            *operation = Operation::NOP;
                            self.changed = true;
                        }
                    }
                }
                Operation::Const(dest, _) => {
                    if let Some(_) = self.rematerialized_consts.get(&dest) {
                        *operation = Operation::NOP;
                    }
                }
                Operation::AddressOf(dest, _) => {
                    if let Some(new_dest) = self.value_replacements.get(&dest) {
                        self.value_removals.insert(dest.clone());
                        *dest = new_dest.clone();
                        self.changed = true;
                    }
                }
                Operation::Move(dest, src) => {
                    if let Some(_) = self.value_removals.get(&src) {
                        *operation = Operation::NOP;
                        self.changed = true;
                    }
                }
                Operation::Load(dest, src) => {
                    if let PlaceKind::Slot(s) = src.kind {
                        if let Some(value) = self.slot_to_values.get(&s) {
                            *operation = Operation::Move(dest.clone(), value.clone());
                            self.changed = true;
                        }
                    }
                }
                Operation::Copy(dest, src, _) => {
                    if let PlaceKind::Slot(s) = src.kind {
                        if let Some(_) = self.slot_removals.get(&s) {
                            *operation = Operation::NOP;
                            self.changed = true;
                        }
                    }
                }
                _ => (),
            }
        }
    }

    fn insert_rematerialized(&mut self, block: &mut BasicBlock) {
        let mut new_operations: Vec<Operation> = Vec::new();
        for operation in &block.operations {
            match operation {
                Operation::Call(_, _, values) => {
                    for value in values {
                        if let Some(litt) = self.rematerialized_consts.get(value) {
                            println!("{}", value);
                            new_operations.push(Operation::Const(value.clone(), litt.clone()));
                        }
                    }
                },
                Operation::NOP => {
                    continue;
                }
                _ => ()
            }
            new_operations.push(operation.clone());
        }
        block.operations = new_operations;
    }

    fn remove_dead_code(&mut self, block: &mut BasicBlock) {
        for operation in &mut block.operations {
            if let Some(dest_value) = operation.get_dest_value() {
                if self.value_is_dead(dest_value) {
                    *operation = Operation::NOP;
                    self.changed = true;
                }
            }
            if let Some(dest_place) = operation.get_dest_place() {
                if self.place_is_dead(dest_place) {
                    *operation = Operation::NOP;
                    self.changed = true;
                }
            }
        }
    }


    fn place_is_dead(&self, place: &Place) -> bool {
        match place.kind.clone() {
            PlaceKind::Slot(s) => {
                let info = self.slot_info.get(&s).unwrap();
                info.reads == 0 && !info.address_taken
            }
            PlaceKind::Pointer(v) => self.value_is_dead(&v),
            PlaceKind::Index { base, index } => {
                self.place_is_dead(&base)
            }
        }
    }

    fn value_is_dead(&self, value: &ValueID) -> bool {
        let info = self.value_info.get(&value).unwrap();
        info.reads == 0
    }

    fn drop_unused_slots(&mut self, func: &mut Function) {
        let mut slots_remaining: HashSet<SlotID> = func
            .slots
            .all_slots()
            .iter()
            .map(|(i, _)| i.clone())
            .collect();
        let initial_length = slots_remaining.len();
        for block in &func.blocks {
            for operation in block.operations.clone() {
                match operation {
                    Operation::Load(_, src) => {
                        if let PlaceKind::Slot(s) = src.kind {
                            slots_remaining.remove(&s);
                        }
                    }
                    Operation::Store(dest, _) => {
                        if let PlaceKind::Slot(s) = dest.kind {
                            slots_remaining.remove(&s);
                        }
                    }
                    Operation::Copy(dest, src, _) => {
                        if let PlaceKind::Slot(s) = dest.kind {
                            slots_remaining.remove(&s);
                        }
                        if let PlaceKind::Slot(s) = src.kind {
                            slots_remaining.remove(&s);
                        }
                    }
                    Operation::AddressOf(_, src) => {
                        if let PlaceKind::Slot(s) = src.kind {
                            slots_remaining.remove(&s);
                        }
                    }
                    _ => (),
                };
            }
        }
        if slots_remaining.len() != initial_length {
            self.changed = true;
        }
        for slot_id in slots_remaining {
            let s = func.slots.get_mut(slot_id);
            s.dropped = true;
            self.slot_info.remove(&slot_id);
        }
    }

    fn is_replacing(&self, id: &SlotID) -> Option<SlotID> {
        for (key, value) in self.slot_replacements.iter() {
            if value == id {
                return Some(key.clone());
            }
        }
        None
    }

    pub fn allocate(&self, function: &mut Function) {
        let mut reads: Counter<ValueID> = Counter::new();
        for arg in &function.args {
            let value_id = function.locator.lookup(arg).unwrap().clone();
            let info = self.value_info.get(&value_id).unwrap();
            function.locator.allocate(&value_id, info.clone(), &mut function.slots);
        }
        for block in &function.blocks {
            for operation in &block.operations {
                if let Some(dest) = operation.get_dest_value() {
                    let info = self.value_info.get(dest).unwrap();
                    function.locator.allocate(dest, info.clone(), &mut function.slots);
                }
                for src in operation.get_src_values() {
                    let info = self.value_info.get(&src).unwrap();
                    reads.increment(&src);
                    if reads.get(&src) >= info.reads {
                        println!("{} dropped", src);
                        function.locator.drop(src);
                    }
                }
            }
            match block.terminator {
                Terminator::Return { value: Some(src) } => {
                    let info = self.value_info.get(&src).unwrap();
                    reads.increment(&src);
                    if reads.get(&src) >= info.reads {
                        function.locator.drop(src);
                    }
                },
                Terminator::Branch { predicate: src, destination:_, inverse:_ } => {
                    let info = self.value_info.get(&src).unwrap();
                    reads.increment(&src);
                    if reads.get(&src) >= info.reads {
                        function.locator.drop(src);
                    }
                },
                _ => ()
            }
        }
    }
}











