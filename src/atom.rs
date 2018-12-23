//! Op palyoads

use std::f64;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::i64;
use std::str::FromStr;

use scan_for_float;
use scan_for_integer;
use scan_for_string;
use UUID;

/// An Atom in RON is an immutable value of one of the types: UUID,
/// integer, string and float.
#[derive(Clone)]
pub enum Atom {
    /// References another object
    UUID(UUID),
    /// Signed integer. RON specifies arbitrary pecicion integer, we only support up to 64 bits.
    Integer(i64),
    /// IEEE 754 Floating point number.
    Float(f64),
    /// UTF-8 String
    String(String),
}

impl Hash for Atom {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            &Atom::String(ref s) => s.hash(state),
            &Atom::Integer(ref s) => s.hash(state),
            &Atom::Float(ref s) => format!("{}", s).hash(state),
            &Atom::UUID(ref s) => s.hash(state),
        }
    }
}

impl PartialEq for Atom {
    fn eq(&self, other: &Atom) -> bool {
        match (self, other) {
            (&Atom::String(ref a), &Atom::String(ref b)) => a == b,
            (&Atom::Integer(ref a), &Atom::Integer(ref b)) => a == b,
            (&Atom::UUID(ref a), &Atom::UUID(ref b)) => a == b,
            (&Atom::Float(ref a), &Atom::Float(ref b)) => a - b < 0.0001,
            _ => false,
        }
    }
}

impl Eq for Atom {}

impl fmt::Debug for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Atom::String(ref s) => {
                let s = format!("{:?}", s);
                write!(f, "'{}'", &s[1..s.len() - 1])
            }
            &Atom::Integer(ref s) => write!(f, "={}", s),
            &Atom::Float(ref s) => write!(f, "^{}", s),
            &Atom::UUID(ref s) => write!(f, ">{}", s.to_string()),
        }
    }
}

impl Atom {
    /// Return true if and only if this is an UUID atom.
    pub fn is_uuid(&self) -> bool {
        match self {
            &Atom::UUID(_) => true,
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

    /// Parse a single atom, returning the Atom and the remaining string. If `context` is not
    /// `None` UUID Atoms may be compressed against (`previous column UUID` / `previous row UUID`).
    pub fn parse<'a>(
        input: &'a str, context: Option<(&UUID, &UUID)>,
    ) -> Option<(Self, &'a str)> {
        let input = input.trim_start();
        match input.chars().next() {
            Some('\'') => Self::parse_string(&input[1..]),
            Some('=') => Self::parse_integer(&input[1..]),
            Some('^') => Self::parse_float(&input[1..]),
            Some('>') => {
                UUID::parse(&input[1..], context)
                    .map(|(uu, cdr)| (Atom::UUID(uu), cdr))
            }
            _ => None,
        }
    }

    fn parse_integer<'a>(input: &'a str) -> Option<(Self, &'a str)> {
        let p = scan_for_integer(input).unwrap_or(input.len());
        if p == 0 {
            None
        } else {
            let (car, cdr) = input.split_at(p);
            i64::from_str(car).ok().map(|i| (Atom::Integer(i), cdr))
        }
    }

    fn parse_float<'a>(input: &'a str) -> Option<(Self, &'a str)> {
        let p = scan_for_float(input).unwrap_or(input.len());
        if p == 0 {
            None
        } else {
            let (car, cdr) = input.split_at(p);
            f64::from_str(car).ok().map(|i| (Atom::Float(i), cdr))
        }
    }

    fn parse_string<'a>(input: &'a str) -> Option<(Self, &'a str)> {
        scan_for_string(input).map(|off| {
            let (a, b) = input.split_at(off);
            (Atom::String(a.to_string()), &b[1..])
        })
    }
}

#[test]
fn atom_uuid() {
    let atom = Atom::UUID(UUID::Name { name: 0, scope: 0 });
    assert_eq!(atom.is_uuid(), true);
    assert_eq!(atom.is_integer(), false);
    assert_eq!(atom.is_float(), false);
    assert_eq!(atom.is_string(), false);
    assert_eq!(&format!("{}", atom), ">0");
}

#[test]
fn atom_integer() {
    let atom = Atom::Integer(42);
    assert_eq!(atom.is_uuid(), false);
    assert_eq!(atom.is_integer(), true);
    assert_eq!(atom.is_float(), false);
    assert_eq!(atom.is_string(), false);
    assert_eq!(&format!("{}", atom), "=42");
}

#[test]
fn atom_float() {
    let atom = Atom::Float(3.14);
    assert_eq!(atom.is_uuid(), false);
    assert_eq!(atom.is_integer(), false);
    assert_eq!(atom.is_float(), true);
    assert_eq!(atom.is_string(), false);
    assert_eq!(&format!("{}", atom), "^3.14");
}

#[test]
fn atom_string() {
    let atom = Atom::String("atom".to_string());
    assert_eq!(atom.is_uuid(), false);
    assert_eq!(atom.is_integer(), false);
    assert_eq!(atom.is_float(), false);
    assert_eq!(atom.is_string(), true);
    assert_eq!(&format!("{}", atom), "'atom'");
}
