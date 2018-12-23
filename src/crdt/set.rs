//! 2-Phase Set

use std::cmp::Ordering;
use std::collections::HashSet;
use std::default::Default;
use std::str::FromStr;
use {Atom, Frame, FrameOrd, Op, Terminator, CRDT, UUID};

/// 2-Phase sets are sets of atoms. Each element is associated with the timestamp of the Op that
/// inserted it. Removing elements is done my inserting a tombstone. Thus, deletions override
/// insertions.
pub struct Set;

impl Set {
    /// Inserts `value` into the Set `state`. Returns the update Frame without modifing the Set.
    pub fn insert<'a>(state: &Frame<'a>, value: Atom) -> Option<Frame<'a>> {
        state.peek().map(|op| {
            let &Op { ref ty, ref object, .. } = op;

            Frame::compress(vec![Op {
                ty: ty.clone(),
                object: object.clone(),
                event: UUID::now(),
                location: UUID::zero(),
                atoms: vec![value].into(),
                term: Terminator::Raw,
            }])
        })
    }

    /// Removes `value` from the Set `state` by inserting a tombstone for all insertions of
    /// `value`. Returns the update Frame without modifing the Set.
    pub fn remove<'a>(state: Frame<'a>, value: &Atom) -> Option<Frame<'a>> {
        Some(Frame::compress(
            state
                .filter_map(|op| {
                    let Op { ty, object, event, location, atoms, .. } = op;

                    if atoms.get(0) == Some(value) && location.is_zero() {
                        Some(Op {
                            ty: ty,
                            object: object,
                            event: UUID::now(),
                            location: event,
                            atoms: Default::default(),
                            term: Terminator::Raw,
                        })
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>(),
        ))
    }
}

impl CRDT for Set {
    type T = HashSet<Atom>;

    fn new<'a>(obj: UUID) -> Frame<'a> {
        Frame::compress(vec![Op {
            ty: UUID::from_str("set").unwrap(),
            object: obj,
            event: UUID::now(),
            location: UUID::zero(),
            atoms: Default::default(),
            term: Terminator::Header,
        }])
    }

    fn reduce<'a>(
        state: Frame<'a>, updates: Vec<Frame<'a>>,
    ) -> Option<Frame<'a>> {
        super::merge::<SetOrd>(state, updates)
    }

    fn map<'a>(state: Frame<'a>) -> Option<Self::T> {
        use std::iter::FromIterator;
        use Terminator::*;

        Some(HashSet::from_iter(state.filter_map(|mut op| {
            match op {
                Op { term: Header, .. } | Op { term: Query, .. } => None,
                Op { ref location, ref mut atoms, .. }
                    if location.is_zero() && atoms.len() == 1 =>
                {
                    Some(atoms.pop().unwrap())
                }
                Op { .. } => None,
            }
        })))
    }
}

#[derive(Debug)]
struct SetOrd<'a>(Frame<'a>);

impl<'a> FrameOrd<'a> for SetOrd<'a> {
    fn primary_cmp(a: &Op, b: &Op) -> Ordering {
        let a = if a.location.is_zero() { &a.event } else { &a.location };
        let b = if b.location.is_zero() { &b.event } else { &b.location };
        UUID::weak_cmp(b, a)
    }

    fn secondary_cmp(a: &Op, b: &Op) -> Ordering {
        UUID::weak_cmp(&b.location, &a.location)
    }

    fn peek(&self) -> Option<&Op> {
        self.0.peek()
    }
}

impl<'a> Iterator for SetOrd<'a> {
    type Item = Op;

    fn next(&mut self) -> Option<Op> {
        self.0.next()
    }
}

impl<'a> From<Frame<'a>> for SetOrd<'a> {
    fn from(frame: Frame<'a>) -> Self {
        SetOrd(frame)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn set_basic_1() {
        let f1 = Frame::parse("*set#test1@1=1;");
        let f2 = Frame::parse("*set#test1@2=2;");
        let exp = Frame::parse("*set#test1@2:0!:0=2@1=1");
        let r = Set::reduce(f1, vec![f2]).unwrap();

        eprintln!(
            "expected: {:?}",
            exp.clone().map(|x| format!("{} ", x)).collect::<Vec<_>>()
        );
        eprintln!(
            "     got: {:?}",
            r.clone().map(|x| format!("{} ", x)).collect::<Vec<_>>()
        );
        assert_eq!(exp.collect::<Vec<_>>(), r.collect::<Vec<_>>());
    }

    #[test]
    fn set_basic_2() {
        let f1 = Frame::parse("*set#test2@1!@=1");
        let f2 = Frame::parse("*set#test2@2:1;");
        let exp = Frame::parse("*set#test2@2!:1,");
        let r = Set::reduce(f1, vec![f2]).unwrap();

        eprintln!(
            "expected: {:?}",
            exp.clone().map(|x| format!("{} ", x)).collect::<Vec<_>>()
        );
        eprintln!(
            "     got: {:?}",
            r.clone().map(|x| format!("{} ", x)).collect::<Vec<_>>()
        );
        assert_eq!(exp.collect::<Vec<_>>(), r.collect::<Vec<_>>());
    }

    #[test]
    fn set_basic_3() {
        let f1 = Frame::parse("*set#test3@3:1;");
        let f2 = Frame::parse("*set#test3@4:2;");
        let exp = Frame::parse("*set#test3@4:3!:2,@3:1,");
        let r = Set::reduce(f1, vec![f2]).unwrap();

        eprintln!(
            "expected: {:?}",
            exp.clone().map(|x| format!("{} ", x)).collect::<Vec<_>>()
        );
        eprintln!(
            "     got: {:?}",
            r.clone().map(|x| format!("{} ", x)).collect::<Vec<_>>()
        );
        assert_eq!(exp.collect::<Vec<_>>(), r.collect::<Vec<_>>());
    }

    #[test]
    fn set_basic_4() {
        let f1 = Frame::parse("*set#test4@2:0!@=2@1=1");
        let f2 = Frame::parse("*set#test4@5!@=5@3:2,@4:1,");
        let exp = Frame::parse("*set#test4@5:0!@=5@3:2,@4:1,");
        let r = Set::reduce(f1, vec![f2]).unwrap();

        eprintln!(
            "expected: {:?}",
            exp.clone().map(|x| format!("{} ", x)).collect::<Vec<_>>()
        );
        eprintln!(
            "     got: {:?}",
            r.clone().map(|x| format!("{} ", x)).collect::<Vec<_>>()
        );
        assert_eq!(exp.collect::<Vec<_>>(), r.collect::<Vec<_>>());
    }

    #[test]
    fn set_basic_5() {
        let f1 = Frame::parse("*set#test5@2:0!@=2@1=1");
        let f2 = Frame::parse("*set#test5@4!@3:2,@4:1,");
        let f3 = Frame::parse("*set#test5@5:0!@=5");
        let exp = Frame::parse("*set#test5@5:0!@5=5@3:2,@4:1,");
        let r = Set::reduce(f1, vec![f2, f3]).unwrap();

        eprintln!(
            "expected: {:?}",
            exp.clone().map(|x| format!("{} ", x)).collect::<Vec<_>>()
        );
        eprintln!(
            "     got: {:?}",
            r.clone().map(|x| format!("{} ", x)).collect::<Vec<_>>()
        );
        assert_eq!(exp.collect::<Vec<_>>(), r.collect::<Vec<_>>());
    }

    #[test]
    fn set_basic_6() {
        let f1 = Frame::parse("*set#test6@3!@:2,@4:1,");
        let f2 = Frame::parse("*set#test6@5:0!@=5");
        let f3 = Frame::parse("*set#test6@2!@=2@1=1");
        let exp = Frame::parse("*set#test6@5:0!@5=5@3:2,@4:1,");
        let r = Set::reduce(f1, vec![f2, f3]).unwrap();

        eprintln!(
            "expected: {:?}",
            exp.clone().map(|x| format!("{} ", x)).collect::<Vec<_>>()
        );
        eprintln!(
            "     got: {:?}",
            r.clone().map(|x| format!("{} ", x)).collect::<Vec<_>>()
        );
        assert_eq!(exp.collect::<Vec<_>>(), r.collect::<Vec<_>>());
    }

    #[test]
    fn set_basic_7() {
        let f1 = Frame::parse("*set#mice@1YKDY54a01+1YKDY5!@>mouse$1YKDY5");
        let f2 = Frame::parse("*set#mice@1YKDXO3201+1YKDXO!@>mouse$1YKDXO@(WBF901(WBY>mouse$1YKDWBY@[67H01[6>mouse$1YKDW6@(Uh4j01(Uh>mouse$1YKDUh@(S67V01(S6>mouse$1YKDS6@(Of(N3:1YKDN3DS01+1YKDN3,@(MvBV01(IuJ:0>mouse$1YKDIuJ@(LF:1YKDIuEY01+1YKDIuJ,:{A601,@(Io5l01[oA:0>mouse$1YKDIoA@[l7_01[l>mouse$1YKDIl@(57(4B:1YKD4B3f01+1YKD4B,@(0bB401+1YKCsd:0>mouse$1YKCsd        @1YKCu6+:1YKCsd7Q01+1YKCsd,");
        let exp = Frame::parse("*set#mice@1YKDY54a01+1YKDY5!@(Y54a01(Y5>mouse$1YKDY5@(XO3201(XO>mouse$1YKDXO@(WBF901(WBY>mouse$1YKDWBY@[67H01[6>mouse$1YKDW6@(Uh4j01(Uh>mouse$1YKDUh@(S67V01(S6>mouse$1YKDS6@(Of(N3:1YKDN3DS01+1YKDN3,@(MvBV01(IuJ:0>mouse$1YKDIuJ@(LF:1YKDIuEY01+1YKDIuJ,:{A601,@(Io5l01[oA:0>mouse$1YKDIoA@[l7_01[l>mouse$1YKDIl@(57(4B:1YKD4B3f01+1YKD4B,@(0bB401+1YKCsd:0>mouse$1YKCsd  @1YKCu6+:1YKCsd7Q01+1YKCsd,");
        let r = Set::reduce(f1, vec![f2]).unwrap();

        eprintln!(
            "expected: {:#?}",
            exp.clone().map(|x| format!("{} ", x)).collect::<Vec<_>>()
        );
        eprintln!(
            "     got: {:#?}",
            r.clone().map(|x| format!("{} ", x)).collect::<Vec<_>>()
        );
        assert_eq!(exp.collect::<Vec<_>>(), r.collect::<Vec<_>>());
    }

    #[test]
    fn map_basic_7() {
        use std::str::FromStr;

        let frm = Frame::parse("*set#mice@1YKDY54a01+1YKDY5!@(Y54a01(Y5>mouse$1YKDY5@(XO3201(XO>mouse$1YKDXO@(WBF901(WBY>mouse$1YKDWBY@[67H01[6>mouse$1YKDW6@(Uh4j01(Uh>mouse$1YKDUh@(S67V01(S6>mouse$1YKDS6@(Of(N3:1YKDN3DS01+1YKDN3,@(MvBV01(IuJ:0>mouse$1YKDIuJ@(LF:1YKDIuEY01+1YKDIuJ,:{A601,@(Io5l01[oA:0>mouse$1YKDIoA@[l7_01[l>mouse$1YKDIl@(57(4B:1YKD4B3f01+1YKD4B,@(0bB401+1YKCsd:0>mouse$1YKCsd@1YKCu6+:1YKCsd7Q01+1YKCsd,");
        let r = Set::map(frm).unwrap();
        let mut exp = HashSet::default();

        exp.insert(Atom::UUID(UUID::from_str("mouse$1YKDY5").unwrap()));
        exp.insert(Atom::UUID(UUID::from_str("mouse$1YKDXO").unwrap()));
        exp.insert(Atom::UUID(UUID::from_str("mouse$1YKDWBY").unwrap()));
        exp.insert(Atom::UUID(UUID::from_str("mouse$1YKDW6").unwrap()));
        exp.insert(Atom::UUID(UUID::from_str("mouse$1YKDUh").unwrap()));
        exp.insert(Atom::UUID(UUID::from_str("mouse$1YKDS6").unwrap()));
        exp.insert(Atom::UUID(UUID::from_str("mouse$1YKDIuJ").unwrap()));
        exp.insert(Atom::UUID(UUID::from_str("mouse$1YKDIoA").unwrap()));
        exp.insert(Atom::UUID(UUID::from_str("mouse$1YKDIl").unwrap()));
        exp.insert(Atom::UUID(UUID::from_str("mouse$1YKCsd").unwrap()));

        assert_eq!(exp, r);
    }

    #[test]
    fn map_empty() {
        let frm = Set::new(UUID::now());
        let r = Set::map(frm).unwrap();
        let exp = HashSet::default();

        assert_eq!(exp, r);
    }

    #[test]
    fn map_insert_remove() {
        use std::iter::FromIterator;

        // empty set
        let st0 = Set::new(UUID::now());
        let exp0 = HashSet::default();
        assert_eq!(exp0, Set::map(st0.clone()).unwrap());

        // insert =1
        let ch1 = Set::insert(&st0, Atom::Integer(1)).unwrap();
        let st1 = Set::reduce(st0, vec![ch1]).unwrap();
        let exp1 = HashSet::from_iter(vec![Atom::Integer(1)].into_iter());
        assert_eq!(exp1, Set::map(st1.clone()).unwrap());

        // insert =1
        let ch2 = Set::insert(&st1, Atom::Integer(1)).unwrap();
        let st2 = Set::reduce(st1, vec![ch2]).unwrap();
        let exp2 = HashSet::from_iter(vec![Atom::Integer(1)].into_iter());
        assert_eq!(exp2, Set::map(st2.clone()).unwrap());

        // insert =2
        let ch3 = Set::insert(&st2, Atom::Integer(2)).unwrap();
        let st3 = Set::reduce(st2, vec![ch3]).unwrap();
        let exp3 = HashSet::from_iter(
            vec![Atom::Integer(1), Atom::Integer(2)].into_iter(),
        );
        assert_eq!(exp3, Set::map(st3.clone()).unwrap());

        // remove =1
        let ch4 = Set::remove(st3.clone(), &Atom::Integer(1)).unwrap();
        let st4 = Set::reduce(st3, vec![ch4]).unwrap();
        let exp4 = HashSet::from_iter(vec![Atom::Integer(2)].into_iter());
        assert_eq!(exp4, Set::map(st4.clone()).unwrap());

        //insert =1
        let ch5 = Set::insert(&st4, Atom::Integer(1)).unwrap();
        let st5 = Set::reduce(st4, vec![ch5]).unwrap();
        let exp5 = HashSet::from_iter(
            vec![Atom::Integer(1), Atom::Integer(2)].into_iter(),
        );
        assert_eq!(exp5, Set::map(st5.clone()).unwrap());

        // remove =1
        let ch6 = Set::remove(st5.clone(), &Atom::Integer(1)).unwrap();
        let st6 = Set::reduce(st5, vec![ch6]).unwrap();
        let exp6 = HashSet::from_iter(vec![Atom::Integer(2)].into_iter());
        eprintln!("{:?}", st6);
        assert_eq!(exp6, Set::map(st6.clone()).unwrap());

        // remove =2
        let ch7 = Set::remove(st6.clone(), &Atom::Integer(2)).unwrap();
        let st7 = Set::reduce(st6, vec![ch7]).unwrap();
        let exp7 = HashSet::default();
        assert_eq!(exp7, Set::map(st7).unwrap());
    }
}
