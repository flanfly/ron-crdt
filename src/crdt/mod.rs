//! Conflict-free replicated datatypes.

use std::fmt::Debug;
use std::iter;

use uuid::UUID;

use crate::{Frame, FrameOrd, Heap, Op, Terminator};

mod lww;
pub use self::lww::LWW;

mod set;
pub use self::set::Set;

/// Operations common to all Conflict-free Replicated Datatypes modeled by RON.
pub trait CRDT {
    /// Rust type this CRDT can be mapped to.
    type T;

    /// Returns the state Frame of a new, empty CRDT instance with UUID `obj`.
    fn new<'a>(obj: UUID) -> Frame<'a>;
    /// Reduce state Frame `state` and update Frames `updates` to a new state Frame.
    fn reduce<'a>(
        state: Frame<'a>, updates: Vec<Frame<'a>>,
    ) -> Option<Frame<'a>>;
    /// Maps the state Frame `state` to a Rust type.
    fn map<'a>(state: Frame<'a>) -> Option<Self::T>;
}

fn merge<'a, F>(state: Frame<'a>, updates: Vec<Frame<'a>>) -> Option<Frame<'a>>
where
    F: FrameOrd<'a> + Debug,
{
    let (ty, obj, full_state) = match state.peek() {
        Some(&Op { ref ty, ref object, ref location, .. }) => {
            (ty.clone(), object.clone(), location.is_zero())
        }
        None => {
            return None;
        }
    };
    let (min, max) = {
        let events = iter::once(&state)
            .chain(updates.iter())
            .filter_map(|frm| frm.peek().as_ref().map(|op| op.event.clone()));
        let min = events
            .clone()
            .min_by(|a, b| UUID::weak_cmp(a, b))
            .unwrap_or_else(UUID::zero);
        let max = events
            .max_by(|a, b| UUID::weak_cmp(a, b))
            .unwrap_or_else(UUID::zero);

        (min, max)
    };
    let mut heap = Heap::<F>::new(
        iter::once(state).chain(updates.into_iter()).collect::<Vec<_>>(),
    );
    let mut out = vec![Op {
        ty: ty,
        object: obj,
        event: max,
        location: if full_state { UUID::zero() } else { min },
        term: Terminator::Header,
        atoms: Default::default(),
    }];

    while let Some(mut op) = heap.pop() {
        op.term = Terminator::Reduced;
        out.push(op);
    }

    Some(Frame::compress(out))
}
