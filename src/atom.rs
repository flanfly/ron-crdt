use Uuid;

/// An Atom in RON is an immutable value of one of the types: UUID,
/// integer, string and float.
#[derive(Debug, PartialEq)]
pub enum Atom {
    Uuid(Uuid),
    Integer(i64),
    Float(f64),
    String(String),
}

impl Atom {
    /// Return true if and only if this is an Uuid atom.
    pub fn is_uuid(&self) -> bool {
        match self {
            &Atom::Uuid(_) => true,
            _ => false,
        }
    }

    /// Return true if and only if this is an Integer atom.
    pub fn is_integer(&self) -> bool {
        match self {
            &Atom::Integer(_) => true,
            _ => false,
        }
    }

    /// Return true if and only if this is a Float atom.
    pub fn is_float(&self) -> bool {
        match self {
            &Atom::Float(_) => true,
            _ => false,
        }
    }

    /// Return true if and only if this is a String atom.
    pub fn is_string(&self) -> bool {
        match self {
            &Atom::String(_) => true,
            _ => false,
        }
    }

    /// Render the atom as text.
    pub fn to_string(&self) -> String {
        match self {
            &Atom::Uuid(uuid) => ">".to_string() + &uuid.to_string(),
            &Atom::Integer(nr) => "=".to_string() + &nr.to_string(),
            &Atom::Float(flt) => "^".to_string() + &flt.to_string(),
            &Atom::String(ref str) => "'".to_string() + &str + &"'".to_string(),
        }
    }
}

#[test]
fn atom_uuid() {
    let atom = Atom::Uuid(Uuid::Name { name: 0, scope: 0 });
    assert_eq!(atom.is_uuid(), true);
    assert_eq!(atom.is_integer(), false);
    assert_eq!(atom.is_float(), false);
    assert_eq!(atom.is_string(), false);
    assert_eq!(atom.to_string(), ">0");
}

#[test]
fn atom_integer() {
    let atom = Atom::Integer(42);
    assert_eq!(atom.is_uuid(), false);
    assert_eq!(atom.is_integer(), true);
    assert_eq!(atom.is_float(), false);
    assert_eq!(atom.is_string(), false);
    assert_eq!(atom.to_string(), "=42");
}

#[test]
fn atom_float() {
    let atom = Atom::Float(3.14);
    assert_eq!(atom.is_uuid(), false);
    assert_eq!(atom.is_integer(), false);
    assert_eq!(atom.is_float(), true);
    assert_eq!(atom.is_string(), false);
    assert_eq!(atom.to_string(), "^3.14");
}

#[test]
fn atom_string() {
    let atom = Atom::String("atom".to_string());
    assert_eq!(atom.is_uuid(), false);
    assert_eq!(atom.is_integer(), false);
    assert_eq!(atom.is_float(), false);
    assert_eq!(atom.is_string(), true);
    assert_eq!(atom.to_string(), "'atom'");
}
