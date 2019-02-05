//! Frame Heap

use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::fmt::Debug;

use crate::{Frame, Op, Terminator};

/// Helper trait for configuring `Heap`s order.
pub trait FrameOrd<'a>: From<Frame<'a>> + Iterator<Item = Op> {
    /// Return the first Op in the Frame.
    fn peek<'b>(&'b self) -> Option<&'b Op>;
    /// Compare `a` and `b` using the primary comparison function.
    fn primary_cmp(a: &Op, b: &Op) -> Ordering;
    /// Compare `a` and `b` using the secondary comparison function.
    fn secondary_cmp(a: &Op, b: &Op) -> Ordering;
}

#[derive(Debug)]
struct Wrapper<T> {
    inner: T,
}

impl<'a, T> PartialOrd for Wrapper<T>
where
    T: FrameOrd<'a>,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a, T> Ord for Wrapper<T>
where
    T: FrameOrd<'a>,
{
    fn cmp(&self, other: &Self) -> Ordering {
        match (self.inner.peek(), other.inner.peek()) {
            (None, None) => Ordering::Equal,
            (None, Some(_)) => Ordering::Less,
            (Some(_), None) => Ordering::Greater,
            (Some(a), Some(b)) => {
                let ret = T::primary_cmp(a, b);
                if ret == Ordering::Equal {
                    T::secondary_cmp(a, b)
                } else {
                    ret
                }
            }
        }
        .reverse()
    }
}

impl<'a, T> PartialEq for Wrapper<T>
where
    T: FrameOrd<'a>,
{
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl<'a, T> Eq for Wrapper<T> where T: FrameOrd<'a> {}

/// Am Iterator heap of Frames.
pub struct Heap<T> {
    heap: BinaryHeap<Wrapper<T>>,
    top: Option<Op>,
}

impl<'a, T> Heap<T>
where
    T: FrameOrd<'a> + Debug,
{
    /// Create a new Heap from `batch`.
    pub fn new(batch: Vec<Frame<'a>>) -> Self {
        let mut heap = BinaryHeap::default();

        for mut f in batch {
            loop {
                match f.peek().cloned() {
                    Some(Op { term: Terminator::Header, .. })
                    | Some(Op { term: Terminator::Query, .. }) => {
                        f.next();
                    }
                    Some(_) => {
                        heap.push(Wrapper { inner: f.into() });
                        break;
                    }
                    None => {
                        break;
                    }
                }
            }
        }

        let mut ret = Heap { heap: heap, top: None };
        let top = ret.advance();
        ret.top = top;
        ret
    }

    /// Returns the smallest of all initial Ops of all Frames.
    pub fn top<'b>(&'b self) -> Option<&'b Op> {
        self.top.as_ref()
    }

    /// Returns and removes the smallest of all initial Ops of all Frames.
    pub fn pop(&mut self) -> Option<Op> {
        use std::mem;

        if self.top.is_some() {
            let mut ret = None;

            mem::swap(&mut ret, &mut self.top);
            self.top = self.advance();
            ret
        } else {
            None
        }
    }

    fn advance(&mut self) -> Option<Op> {
        loop {
            match self.heap.pop() {
                Some(mut frm) => {
                    match frm.inner.peek().cloned() {
                        Some(Op { term: Terminator::Header, .. })
                        | Some(Op { term: Terminator::Query, .. }) => {
                            frm.inner.next();
                            self.heap.push(frm);
                            /* continue */
                        }
                        Some(op) => {
                            self.heap.push(frm);
                            self.drain(&op);
                            return Some(op);
                        }
                        None => {}
                    }
                }
                None => {
                    return None;
                }
            }
        }
    }

    fn drain(&mut self, op: &Op) {
        let mut frames = Vec::with_capacity(self.heap.len());

        while let Some(mut frm) = self.heap.pop() {
            loop {
                match frm.inner.peek().cloned() {
                    Some(ref p) if T::primary_cmp(p, op) == Ordering::Equal => {
                        frm.inner.next();
                    }
                    _ => {
                        break;
                    }
                }
            }
            frames.push(frm);
        }

        self.heap.extend(frames);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use uuid::UUID;

    use crate::Heap;

    #[derive(Debug)]
    struct HeapOrd<'a>(Frame<'a>);

    impl<'a> FrameOrd<'a> for HeapOrd<'a> {
        fn primary_cmp(a: &Op, b: &Op) -> Ordering {
            if a.event == b.event {
                UUID::weak_cmp(&a.location, &b.location)
            } else {
                UUID::weak_cmp(&b.event, &a.event)
            }
        }

        fn secondary_cmp(a: &Op, b: &Op) -> Ordering {
            UUID::weak_cmp(&a.location, &b.location)
        }

        fn peek(&self) -> Option<&Op> {
            self.0.peek()
        }
    }

    impl<'a> Iterator for HeapOrd<'a> {
        type Item = Op;

        fn next(&mut self) -> Option<Op> {
            self.0.next()
        }
    }

    impl<'a> From<Frame<'a>> for HeapOrd<'a> {
        fn from(frame: Frame<'a>) -> Self {
            HeapOrd(frame)
        }
    }

    #[test]
    fn merge() {
        let frame_a = Frame::parse("*rga#test@0:0!@1'A'@2'B'"); //  D E A C B
        let frame_b = Frame::parse("*rga#test@0:0!@1'A'@3'C'");
        let frame_c = Frame::parse("*rga#test@0:0!@4'D'@5'E'");
        let mut frame_r = Frame::parse("*rga#test@4'D',@5'E'@1'A'@3'C'@2'B'");
        let mut heap = Heap::<HeapOrd>::new(vec![frame_a, frame_b, frame_c]);

        while let op @ Some(_) = heap.pop() {
            assert_eq!(frame_r.next(), op);
        }

        assert!(frame_r.next().is_none());
    }
}
