use smallvec::SmallVec;
use Atom;
use Uuid;

/// An Op (operation) in RON describes part of a specific action on an
/// object, or some other action related to the Swarm protocol (such
/// as a query or handshake). Operations are built from UUIDs and atoms.
///
/// Every op consists of four UUIDs, a (possibly empty) sequence of
/// atoms, and a terminator.
#[derive(Debug)]
pub struct Op {
    pub ty: Uuid,
    pub object: Uuid,
    pub event: Uuid,
    pub location: Uuid,
    pub atoms: SmallVec<[Atom; 3]>,
}

impl Op {
    pub fn to_string(&self) -> String {
        let mut result = String::default();
        result += &("*".to_string() + &self.ty.to_string());
        result += &("#".to_string() + &self.object.to_string());
        result += &("@".to_string() + &self.event.to_string());
        result += &(":".to_string() + &self.location.to_string());
        for atom in &self.atoms {
            result += &atom.to_string();
        }
        result
    }
}

#[derive(Debug)]
pub enum Terminator {
    Raw,
    Query,
    Header,
    Reduced,
}

impl Terminator {
    pub fn to_string(&self) -> &'static str {
        match &self {
            Raw => ";",
            Query => "?",
            Header => "!",
            Reduced => ",",
        }
    }
}

#[derive(Debug)]
pub struct Frame {
    /// True if frame is terminated by '.'
    pub terminate: bool,
    /// The op list in this frame (uncompressed).
    pub ops: Vec<(Op, Terminator)>,
}

impl Frame {
    pub fn to_string(&self) -> String {
        let mut result = String::default();
        for (op, term) in &self.ops {
            result += &op.to_string();
            result += term.to_string();
        }
        if self.terminate {
            result += &".".to_string();
        }
        result
    }
}
