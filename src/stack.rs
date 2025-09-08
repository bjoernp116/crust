


#[derive(Default)]
pub struct Stack {
    inner: Vec<StackElement>,
}

impl Stack {
    pub fn new() -> Stack {
        Stack::default()
    }
    pub fn push(&mut self, identifier: String, size: usize, typename: String) {
        self.inner.push(StackElement { identifier, size, typename });
    }
    pub fn pop(&mut self) -> Option<StackElement> {
        self.inner.pop()
    }
}

struct StackElement {
    identifier: String,
    size: usize,
    typename: String, 
}
