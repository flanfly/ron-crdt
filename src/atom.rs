use std::fmt;
use std::i64;
use std::f64;
use std::str::FromStr;
use Uuid;

/// An Atom in RON is an immutable value of one of the types: UUID,
/// integer, string and float.
#[derive(Clone)]
pub enum Atom {
    Uuid(Uuid),
    Integer(i64),
    Float(f64),
    String(String),
}

impl PartialEq for Atom {
    fn eq(&self, other: &Atom) -> bool {
        match (self, other) {
            (&Atom::String(ref a), &Atom::String(ref b)) => a == b,
            (&Atom::Integer(ref a), &Atom::Integer(ref b)) => a == b,
            (&Atom::Uuid(ref a), &Atom::Uuid(ref b)) => a == b,
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
            &Atom::Uuid(ref s) => write!(f, ">{}", s.to_string()),
        }
    }
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

    pub fn parse<'a>(input: &'a str, prev_col: &Uuid, prev_row: &Uuid) -> Option<(Self, &'a str)> {
        match input.trim_end().chars().next() {
            Some('\'') => Self::parse_string(&input[1..]),
            Some('=') => Self::parse_integer(&input[1..]),
            Some('^') => Self::parse_float(&input[1..]),
            Some('>') => Uuid::parse(&input[1..], prev_col, prev_row)
                .map(|(uu,cdr)| (Atom::Uuid(uu), cdr)),
            _ => None,
        }
    }

    fn parse_integer<'a>(input: &'a str) -> Option<(Self, &'a str)> {
        let p = input.chars().position(|c| !c.is_ascii_digit()).unwrap_or(input.len());
        if p == 0 {
            None
        } else {
            let (car, cdr) = input.split_at(p);
            i64::from_str(car).ok().map(|i| (Atom::Integer(i), cdr))
        }
    }

    fn parse_float<'a>(input: &'a str) -> Option<(Self, &'a str)> {
        let p = input.chars().position(|c| {
            !c.is_ascii_digit() && c != 'e' && c != 'E' && c != '.'
        }).unwrap_or(input.len());
        if p == 0 {
            None
        } else {
            let (car, cdr) = input.split_at(p);
            f64::from_str(car).ok().map(|i| (Atom::Float(i), cdr))
        }
    }

    fn parse_string<'a>(input: &'a str) -> Option<(Self, &'a str)> {
        let mut escaped = false;
        for (off, ch) in input.char_indices() {
            escaped = match (ch, escaped) {
                ('\'', false) if off != 0 => {
                    let (a, b) = input.split_at(off);
                    return Some((Atom::String(a.to_string()), &b[1..]));
                }
                ('\'', false) => { return None }
                ('\'', true) => false,

                ('\\', false) => true,
                ('\\', true) => false,

                ('n', true) => false,
                ('t', true) => false,

                (_, false) => false,
                (_, true) => false,
            };
        }

        None
    }
}

#[test]
fn atom_uuid() {
    let atom = Atom::Uuid(Uuid::Name { name: 0, scope: 0 });
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
