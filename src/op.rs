use std::fmt;

use smallvec::SmallVec;
use Atom;
use UUID;

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
    pub ty: UUID,
    pub object: UUID,
    pub event: UUID,
    pub location: UUID,
    pub atoms: SmallVec<[Atom; 3]>,
    pub term: Terminator,
}

impl Op {
    /// Parse a single Op from string `input` into `prev`. If `prev` is not `None` the Op may be
    /// compressed against `prev`.
    pub fn parse_inplace<'a>(
        prev: &mut Option<Op>, input: &'a str,
    ) -> Option<&'a str> {
        let mut ret = input;
        let mut ty = None;
        let mut event = None;
        let mut object = None;
        let mut location = None;

        {
            let mut prev_uu = prev.as_ref().map(|p| &p.location);

            // type
            ret = ret.trim_start();
            if ret.starts_with('*') {
                let ctx = prev.as_ref().map(|p| (&p.ty, prev_uu.unwrap()));

                match UUID::parse(&ret[1..], ctx) {
                    Some((t, rest)) => {
                        ret = rest;
                        ty = Some(t);
                        prev_uu = ty.as_ref();
                    }
                    None => {
                        ret = &ret[1..];
                        ty = prev.as_ref().map(|p| p.ty.clone());
                        prev_uu = prev.as_ref().map(|p| &p.ty);
                    }
                }
            } else {
                prev_uu = prev.as_ref().map(|p| &p.ty);
            }

            // object
            ret = ret.trim_start();
            if ret.starts_with('#') {
                let ctx = prev.as_ref().map(|p| (&p.object, prev_uu.unwrap()));

                match UUID::parse(&ret[1..], ctx) {
                    Some((obj, rest)) => {
                        ret = rest;
                        object = Some(obj);
                        prev_uu = object.as_ref();
                    }
                    None => {
                        ret = &ret[1..];
                        object = prev.as_ref().map(|p| p.object.clone());
                        prev_uu = prev.as_ref().map(|p| &p.object);
                    }
                }
            } else {
                prev_uu = prev.as_ref().map(|p| &p.object);
            }

            // event
            ret = ret.trim_start();
            if ret.starts_with('@') {
                let ctx = prev.as_ref().map(|p| (&p.event, prev_uu.unwrap()));

                match UUID::parse(&ret[1..], ctx) {
                    Some((ev, rest)) => {
                        ret = rest;
                        event = Some(ev);
                        prev_uu = event.as_ref();
                    }
                    None => {
                        ret = &ret[1..];
                        event = prev.as_ref().map(|p| p.event.clone());
                        prev_uu = prev.as_ref().map(|p| &p.event);
                    }
                }
            } else {
                prev_uu = prev.as_ref().map(|p| &p.event);
            }

            // location
            ret = ret.trim_start();
            if ret.starts_with(':') {
                let ctx =
                    prev.as_ref().map(|p| (&p.location, prev_uu.unwrap()));

                match UUID::parse(&ret[1..], ctx) {
                    Some((loc, rest)) => {
                        ret = rest;
                        location = Some(loc);
                    }
                    None => {
                        ret = &ret[1..];
                        location = prev.as_ref().map(|p| p.location.clone());
                    }
                }
            }
        }

        let no_spec = ty.is_none()
            && event.is_none()
            && object.is_none()
            && location.is_none();

        let prev = match prev {
            &mut Some(ref mut prev) => {
                if let Some(ty) = ty {
                    prev.ty = ty;
                }
                if let Some(event) = event {
                    prev.event = event;
                }
                if let Some(object) = object {
                    prev.object = object;
                }
                if let Some(location) = location {
                    prev.location = location;
                }

                prev
            }
            &mut None
                if ty.is_some() && object.is_some() && event.is_some() =>
            {
                *prev = Some(Op {
                    ty: ty.unwrap(),
                    object: object.unwrap(),
                    event: event.unwrap(),
                    location: location.unwrap_or_else(UUID::zero),
                    atoms: Default::default(),
                    term: Terminator::Header,
                });
                prev.as_mut().unwrap()
            }
            &mut None => {
                return None;
            }
        };

        // atoms
        prev.atoms.clear();

        {
            let mut prev_uu = prev.object.clone();

            while let Some((atm, rest)) =
                Atom::parse(ret, Some((&prev.object, &prev_uu)))
            {
                match &atm {
                    &Atom::UUID(ref uu) => {
                        prev_uu = uu.clone();
                    }
                    _ => {}
                }

                ret = rest;
                prev.atoms.push(atm);
            }
        }

        // Terminator
        ret = ret.trim_start();
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
            _ => {
                return None;
            }
        }

        Some(ret)
    }

    /// Serialize Op to text optionally compressing it against `context`.
    pub fn compress(&self, context: Option<&Op>) -> String {
        let mut ret = String::default();

        if context.map(|o| o.ty != self.ty).unwrap_or(true) {
            let ctx = context.map(|p| (&p.ty, &p.ty));

            ret += "*";
            ret += &self.ty.compress(ctx);
        }

        if context.map(|o| o.object != self.object).unwrap_or(true) {
            let ctx = context.map(|p| (&p.object, &p.ty));

            ret += "#";
            ret += &self.object.compress(ctx);
        }

        if context.map(|o| o.event != self.event).unwrap_or(true) {
            let ctx = context.map(|p| (&p.event, &p.object));

            ret += "@";
            ret += &self.event.compress(ctx);
        }

        if context.map(|o| o.location != self.location).unwrap_or(true) {
            let ctx = context.map(|p| (&p.location, &p.event));

            ret += ":";
            ret += &self.location.compress(ctx);
        }

        if ret.is_empty() {
            ret = "@".to_string();
        }

        let mut prev = &self.object;
        for atm in self.atoms.iter() {
            match atm {
                &Atom::Integer(i) => {
                    ret += &format!("={}", i);
                }
                &Atom::String(ref i) => {
                    ret += &format!("'{}'", i);
                }
                &Atom::Float(i) => {
                    ret += &format!("^{}", i);
                }
                &Atom::UUID(ref i) => {
                    ret += ">";
                    ret += &i.compress(Some((prev, prev)));
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
        write!(
            f,
            "*{}#{}@{}:{}",
            self.ty, self.object, self.event, self.location
        )?;

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

#[test]
fn compress_roundtrip() {
    use Frame;
    let f = Frame::parse(
        "
        *set#mice@1YKDXO3201+1YKDXO!
                 @>mouse$1YKDXO
                 @(WBF901(WBY>mouse$1YKDWBY
                 @[67H01[6>mouse$1YKDW6
                 @(Uh4j01(Uh>mouse$1YKDUh
                 @(S67V01(S6>mouse$1YKDS6
                 @(Of(N3:1YKDN3DS01+1YKDN3,
                 @(MvBV01(IuJ:0>mouse$1YKDIuJ
                 @(LF:1YKDIuEY01+1YKDIuJ,
                 :{A601,
                 @(Io5l01[oA:0>mouse$1YKDIoA
                 @[l7_01[l>mouse$1YKDIl
                 @(57(4B:1YKD4B3f01+1YKD4B,
                 @(0bB401+1YKCsd:0>mouse$1Y",
    );

    let ops = f.into_iter().collect::<Vec<_>>();
    for win in ops.windows(2) {
        let ctx = &win[0];
        let op = &win[1];
        let txt = op.compress(Some(ctx));
        let mut back = Some(ctx.clone());

        Op::parse_inplace(&mut back, &txt[..]);

        eprintln!("ctx: {}", ctx);
        eprintln!("op : {}", op);
        eprintln!("txt: {}", txt);
        eprintln!("rt : {}", back.as_ref().unwrap());
        eprintln!("");
        assert_eq!(back.unwrap(), *op);
    }
}
