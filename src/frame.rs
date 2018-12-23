//! Frame

use std::borrow::Cow;
use Op;

/// A Frame is an ordered, immutable sequence of Ops.
///
/// Frames support iterating over the Ops contained.
#[derive(Debug, Clone)]
pub struct Frame<'a> {
    body: Cow<'a, str>,
    ptr: usize,
    op: Option<Op>,
}

impl<'a> Frame<'a> {
    /// Create a new frame from text encoded Ops `s`.
    pub fn parse<S>(s: S) -> Frame<'a>
    where
        S: Into<Cow<'a, str>>,
    {
        let mut ret = Frame { body: s.into(), ptr: 0, op: None };

        ret.advance();
        ret
    }

    /// Encode and compress `ops` into the text format.
    pub fn compress(ops: Vec<Op>) -> Self {
        if ops.is_empty() {
            return Self::parse("");
        }

        let mut txt = ops[0].compress(None);

        for win in ops[..].windows(2) {
            txt += &win[1].compress(Some(&win[0]));
        }

        Self::parse(txt)
    }

    /// Returns the first Op in this frame.
    pub fn peek<'b>(&'b self) -> Option<&'b Op> {
        if self.ptr > self.body.len() {
            None
        } else {
            self.op.as_ref()
        }
    }

    /// Returns the text encoding of all Ops in the Frame.
    pub fn body(&self) -> &str {
        &self.body
    }

    fn advance(&mut self) {
        if self.ptr < self.body.len() {
            let input = &self.body[self.ptr..];
            match Op::parse_inplace(&mut self.op, input) {
                Some(p) => {
                    self.ptr =
                        p.as_ptr() as usize - self.body[..].as_ptr() as usize;
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
            if let Some(op) = self.op.clone() {
                self.advance();
                Some(op)
            } else {
                None
            }
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

    while let op @ Some(_) = frame.peek().cloned() {
        assert_eq!(op, frame.next());
    }
}

#[test]
fn iter2() {
    let frame = "*rga#test:0!@4'D'@5'E'";
    let mut frame = Frame::parse(frame);

    while let op @ Some(_) = frame.peek().cloned() {
        assert_eq!(op, frame.next());
    }
}
