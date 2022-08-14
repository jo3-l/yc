use std::ops::Add;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NodeId {
    id: u32,
}

impl NodeId {
    pub const DUMMY: Self = NodeId { id: u32::MAX };

    pub fn new(id: u32) -> Self {
        Self { id }
    }

    pub fn as_u32(self) -> u32 {
        self.id
    }
}

impl From<NodeId> for u32 {
    fn from(id: NodeId) -> u32 {
        id.as_u32()
    }
}

impl From<u32> for NodeId {
    fn from(id: u32) -> NodeId {
        NodeId::new(id)
    }
}

impl Add<u32> for NodeId {
    type Output = Self;

    fn add(self, offset: u32) -> Self {
        Self::new(self.as_u32() + offset)
    }
}
