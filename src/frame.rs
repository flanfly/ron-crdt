use Op;

#[derive(Debug, PartialEq)]
pub struct Frame {
    /// The op list in this frame (uncompressed).
    pub ops: Vec<Op>,
    /// True if frame is terminated by '.'
    pub terminate: bool,
}

impl Frame {
    pub fn to_string(&self) -> String {
        let mut result = String::default();
        for op in &self.ops {
            result += &op.to_string();
        }
        if self.terminate {
            result += &".".to_string();
        }
        result
    }
}
