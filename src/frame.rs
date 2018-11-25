use std::borrow::Cow;
use {Op, Terminator};

#[derive(Debug)]
pub struct Frame<'a> {
    body: Cow<'a, str>,
    ptr: usize,
    op: Op,
}

impl<'a> Frame<'a> {
    pub fn parse<S>(s: S) -> Frame<'a> where S: Into<Cow<'a, str>> {
        let op = Op{
            ty: Default::default(),
            event: Default::default(),
            object: Default::default(),
            location: Default::default(),
            atoms: Default::default(),
            term: Terminator::Reduced,
        };
        let mut ret = Frame{
            body: s.into(),
            ptr: 0,
            op: op,
        };

        ret.advance();
        ret
    }

    pub fn compress(mut ops: Vec<Op>) -> Self {
        let mut txt = String::default();
        let op = Op{
            ty: Default::default(),
            event: Default::default(),
            object: Default::default(),
            location: Default::default(),
            atoms: Default::default(),
            term: Terminator::Reduced,
        };

        ops.insert(0, op);

        for win in ops[..].windows(2) {
            txt += &win[1].compress(&win[0]);
        }

        Self::parse(txt)
    }

    pub fn peek<'b>(&'b self) -> Option<&'b Op> {
        if self.ptr > self.body.len() {
            None
        } else {
            Some(&self.op)
        }
    }

    fn advance(&mut self) {
        if self.ptr < self.body.len() {
            let input = &self.body[self.ptr..];
            match Op::parse_inplace(&mut self.op, input) {
                Some(p) => {
                    self.ptr = p.as_ptr() as usize - self.body[..].as_ptr() as usize;
                }
                None => {
                    self.ptr = self.body.len() + 1;
                }
            }
        } else {
            self.ptr = self.body.len() + 1;
        }
    }
}

impl<'a> Iterator for Frame<'a> {
    type Item = Op;

    fn next(&mut self) -> Option<Self::Item> {
        if self.ptr > self.body.len() {
            None
        } else {
            let op = self.op.clone();
            self.advance();
            Some(op)
        }
    }
}

#[test]
fn count() {
    let frame = "*lww#test@0:0! @1:key'value' @2:number=1 *rga#text@3:0'T'! *rga#text@6:3, @4'e' @5'x' @6't' *lww#more:a=1;.";
    let frame = Frame::parse(frame);
    assert_eq!(frame.count(), 9);
}

#[test]
fn iter() {
    let frame = "*lww#test@0:0!@1:key'value'@2:number=1*rga#text@3:0'T'!*rga#text@6:3,@4'e'@5'x'@6't'*lww#more:a=1;.";
    let mut frame = Frame::parse(frame);

    while let op@Some(_) = frame.peek().cloned() {
        assert_eq!(op, frame.next());
    }
}

#[test]
fn iter2() {
    let frame = "*rga#test:0!@4'D'@5'E'";
    let mut frame = Frame::parse(frame);

    while let op@Some(_) = frame.peek().cloned() {
        assert_eq!(op, frame.next());
    }
}

#[test]
fn compress() {
    let frame = "*lww#test@0:0!@1:key'value'@2:number=1*rga#text@3:0'T'!*rga#text@6:3,@4'e'@5'x'@6't'*lww#more:a=1;.";
    let compressed = Frame::compress(Frame::parse(frame).into_iter().collect::<Vec<_>>());
    let frame = Frame::parse(frame);

    assert!(frame.eq(compressed));
}
