use std::fmt;

use smallvec::SmallVec;
use Atom;
use Uuid;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Terminator {
    // Raw ops are stand-alone within a frame.
    Raw,
    // Query and header ops, as well as reduced ops following them, create a chunk in a frame.
    Query,
    Header,
    // Reduced ops belong to the query/header op before them.
    Reduced,
}

impl fmt::Debug for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Display for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Terminator::Raw => f.write_str(";"),
            Terminator::Reduced => f.write_str(","),
            Terminator::Header => f.write_str("!"),
            Terminator::Query => f.write_str("?"),
        }
    }
}

impl Default for Terminator {
    fn default() -> Terminator {
        Terminator::Reduced
    }
}

impl Terminator {
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
#[derive(Clone, PartialEq)]
pub struct Op {
    pub ty: Uuid,
    pub object: Uuid,
    pub event: Uuid,
    pub location: Uuid,
    pub atoms: SmallVec<[Atom; 3]>,
    pub term: Terminator,
}

impl Op {
    pub fn parse_inplace<'a>(prev: &mut Op, input: &'a str) -> Option<&'a str> {
        let mut ret = input;
        let mut ty = None;
        let mut event = None;
        let mut object = None;
        let mut location = None;

        {
            let mut prev_uu = &prev.location;

            // type
            ret = ret.trim_left();
            if ret.starts_with('*') {
                match Uuid::parse(&ret[1..], &prev.ty, prev_uu) {
                    Some((t, rest)) => {
                        ret = rest;
                        ty = Some(t);
                        prev_uu = ty.as_ref().unwrap();
                    }
                    None => { return None; }
                }
            } else {
                prev_uu = &prev.ty;
            }


            // object
            ret = ret.trim_left();
            if ret.starts_with('#') {
                match Uuid::parse(&ret[1..], &prev.object, prev_uu) {
                    Some((obj, rest)) => {
                        ret = rest;
                        object = Some(obj);
                        prev_uu = object.as_ref().unwrap();
                    }
                    None => { return None; }
                }
            } else {
                prev_uu = &prev.object;
            }

            // event
            ret = ret.trim_left();
            if ret.starts_with('@') {
                match Uuid::parse(&ret[1..], &prev.event, prev_uu) {
                    Some((ev, rest)) => {
                        ret = rest;
                        event = Some(ev);
                        prev_uu = event.as_ref().unwrap();
                    }
                    None => { return None; }
                }
            } else {
                prev_uu = &prev.event;
            }

            // location
            ret = ret.trim_left();
            if ret.starts_with(':') {
                match Uuid::parse(&ret[1..], &prev.location, prev_uu) {
                    Some((loc, rest)) => {
                        ret = rest;
                        location = Some(loc);
                    }
                    None => { return None; }
                }
            }
        }

        let no_spec = ty.is_none() && event.is_none()
            && object.is_none() && location.is_none();

        if let Some(ty) = ty { prev.ty = ty; }
        if let Some(event) = event { prev.event = event; }
        if let Some(object) = object { prev.object = object; }
        if let Some(location) = location { prev.location = location; }

        // atoms
        prev.atoms.clear();

        {
            let mut prev_uu = prev.location.clone();

            while let Some((atm, rest)) = Atom::parse(ret, &prev.location, &prev_uu) {
                match &atm {
                    &Atom::Uuid(ref uu) => {
                        prev_uu = uu.clone();
                    }
                    _ => {}
                }

                ret = rest;
                prev.atoms.push(atm);
            }
        }

        // Terminator
        ret = ret.trim_left();
        match ret.chars().next() {
            Some('!') => {
                prev.term = Terminator::Header;
                ret = &ret[1..];
            }
            Some('?') => {
                prev.term = Terminator::Query;
                ret = &ret[1..];
            }
            Some(',') => {
                prev.term = Terminator::Reduced;
                ret = &ret[1..];
            }
            Some(';') => {
                prev.term = Terminator::Raw;
                ret = &ret[1..];
            }
            _ if !no_spec || !prev.atoms.is_empty() => {
                prev.term = Terminator::Reduced;
            }
            _ => { return None; }
        }

        Some(ret)
    }

    pub fn compress(&self, context: &Op) -> String {
        let mut ret = String::default();

        if context.ty != self.ty {
            ret += "*";
            ret += &self.ty.compress(&context.ty, &context.ty);
        }

        if context.object != self.object {
            ret += "#";
            ret += &self.object.compress(&context.object, &context.ty);
        }

        if context.event != self.event {
            ret += "@";
            ret += &self.event.compress(&context.event, &context.object);
        }

        if context.location != self.location {
            ret += ":";
            ret += &self.location.compress(&context.location, &context.event);
        }

        let mut prev = &self.location;
        for atm in self.atoms.iter() {
            match atm {
                &Atom::Integer(i) => { ret += &format!("={}", i); }
                &Atom::String(ref i) => { ret += &format!("'{}'", i); }
                &Atom::Float(i) => { ret += &format!("^{}", i); }
                &Atom::Uuid(ref i) => {
                    ret += &i.compress(prev, prev);
                    prev = i;
                }
            }
        }

        ret + &format!("{}", self.term)
    }
}
impl fmt::Debug for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "*{}#{}@{}:{}", self.ty, self.object, self.event,
               self.location)?;

        match self.atoms.len() {
            0 => write!(f, "{}", self.term),
            1 => write!(f, "{}{}", self.atoms[0], self.term),
            _ => {
                write!(f, "{}", self.atoms[0])?;
                for atm in self.atoms[1..].iter() {
                    write!(f, ", {}", atm)?;
                }
                write!(f, "{}", self.term)
            }
        }
    }
}

