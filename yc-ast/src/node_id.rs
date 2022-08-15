use std::ops::{Add, AddAssign, Sub, SubAssign};

/// A unique identifier for a node in an AST.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NodeId {
    id: usize,
}

impl NodeId {
    pub const DUMMY: Self = NodeId { id: usize::MAX };

    pub fn from_usize(id: usize) -> Self {
        Self { id }
    }

    pub fn as_usize(self) -> usize {
        self.id
    }
}

impl Add<usize> for NodeId {
    type Output = Self;

    fn add(self, offset: usize) -> Self {
        Self::from_usize(self.as_usize() + offset)
    }
}

impl AddAssign<usize> for NodeId {
    fn add_assign(&mut self, offset: usize) {
        self.id += offset;
    }
}

impl Sub<usize> for NodeId {
    type Output = Self;

    fn sub(self, offset: usize) -> Self {
        Self::from_usize(self.as_usize() - offset)
    }
}

impl SubAssign<usize> for NodeId {
    fn sub_assign(&mut self, offset: usize) {
        self.id -= offset;
    }
}

impl From<NodeId> for usize {
    fn from(id: NodeId) -> usize {
        id.as_usize()
    }
}

impl From<usize> for NodeId {
    fn from(id: usize) -> NodeId {
        NodeId::from_usize(id)
    }
}
