use smallvec::SmallVec;
use Atom;
use Uuid;

#[derive(Debug, PartialEq)]
pub enum Terminator {
    // Raw ops are stand-alone within a frame.
    Raw,
    // Query and header ops, as well as reduced ops following them, create a chunk in a frame.
    Query,
    Header,
    // Reduced ops belong to the query/header op before them.
    Reduced,
}

impl Default for Terminator {
    fn default() -> Terminator {
        Terminator::Reduced
    }
}

impl Terminator {
    pub fn to_string(&self) -> &'static str {
        match &self {
            Terminator::Raw => ";",
            Terminator::Query => "?",
            Terminator::Header => "!",
            Terminator::Reduced => ",",
        }
    }

    pub fn from_string(inp: &str) -> Result<Terminator, &'static str> {
        match &inp {
            &";" => Ok(Terminator::Raw),
            &"?" => Ok(Terminator::Query),
            &"!" => Ok(Terminator::Header),
            &"," => Ok(Terminator::Reduced),
            _ => Err("invalid terminator"),
        }
    }

    pub fn from_char(inp: char) -> Result<Terminator, &'static str> {
        Terminator::from_string(&inp.to_string())
    }
}

/// An Op (operation) in RON describes part of the initial state of an object, or a specific
/// action on an object, or some other part related to the Swarm protocol (such as a query or
/// handshake).
///
/// Every op consists of four UUIDs (type, object, event and re), a (possibly empty) sequence of
/// atoms, and a terminator.
#[derive(Debug, PartialEq)]
pub struct Op {
    pub ty: Uuid,
    pub object: Uuid,
    pub event: Uuid,
    pub location: Uuid,
    pub atoms: SmallVec<[Atom; 3]>,
    pub term: Terminator,
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
        result += &self.term.to_string();
        result
    }
}


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
