use std::fmt::Debug;
use std::iter;
use {Frame, FrameOrd, Heap, Op, Terminator, UUID};

mod lww;
pub use self::lww::LWW;

mod set;
pub use self::set::Set;

pub trait CRDT {
    type T;

    fn new<'a>(obj: UUID) -> Frame<'a>;
    fn reduce<'a>(
        state: Frame<'a>, updates: Vec<Frame<'a>>,
    ) -> Option<Frame<'a>>;
    fn map<'a>(state: Frame<'a>) -> Option<Self::T>;
}

pub fn merge<'a, F>(
    state: Frame<'a>, updates: Vec<Frame<'a>>,
) -> Option<Frame<'a>>
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
        let mut events = iter::once(&state)
            .chain(updates.iter())
            .filter_map(|frm| frm.peek().as_ref().map(|op| op.event.clone()));
        let mut min = events
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
