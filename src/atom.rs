use Uuid;

/// An Atom in RON is an immutable value of one of the types UUID,
/// integer, string and float.
///
/// Atoms are described by a 128-bit number, where 2 bits describe the
/// type. These bits are in the variant field if the atom is
/// interpreted as a RFC4122 UUID, making RON UUIDs compatible with
/// RFC4122 UUIDs.
#[derive(Debug)]
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

    /// Return true if and only if this is a Integer atom.
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

    pub fn to_string(&self) -> String {
        match self {
            &Atom::Uuid(uuid) => ">".to_string() + &uuid.to_string(),
            &Atom::Integer(nr) => "=".to_string() + &nr.to_string(),
            &Atom::Float(flt) => "^".to_string() + &flt.to_string(),
            &Atom::String(ref str) => "'".to_string() + &str + &"'".to_string(),
        }
    }
}

// #[cfg(test)]
// mod tests {
//     use Uuid;
//
//     #[test]
//     fn ron_uuid() {
//         let scheme = Uuid::EVENT;
//         let variety = 10;
//         let value = 0xdead;
//         let origin = 0xbeef;
//
//         let uuid = Uuid::new_ron_uuid(scheme, variety, value, origin);
//         assert_eq!(uuid.content[0], 0xa00000000000dead);
//         assert_eq!(uuid.content[1], 0x200000000000beef);
//     }
// }
