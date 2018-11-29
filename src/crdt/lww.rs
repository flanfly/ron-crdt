use std::cmp::Ordering;
use std::collections::HashMap;
use std::str::FromStr;
use {Atom, Frame, FrameOrd, Op, Terminator, CRDT, UUID};

pub struct LWW;

impl LWW {
    pub fn set<'a>(
        state: &Frame<'a>, key: UUID, value: Atom,
    ) -> Option<Frame<'a>> {
        state.peek().map(|op| {
            let &Op { ref object, .. } = op;

            Frame::compress(vec![Op {
                ty: UUID::from_str("lww").unwrap(),
                object: object.clone(),
                event: UUID::now(),
                location: key,
                atoms: vec![value].into(),
                term: Terminator::Raw,
            }])
        })
    }
}

impl CRDT for LWW {
    type T = HashMap<UUID, Atom>;

    fn new<'a>(obj: UUID) -> Frame<'a> {
        Frame::compress(vec![Op {
            ty: UUID::from_str("lww").unwrap(),
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
        super::merge::<LWWOrd>(state, updates)
    }

    fn map<'a>(state: Frame<'a>) -> Option<Self::T> {
        use std::iter::FromIterator;
        use Terminator::*;

        Some(HashMap::from_iter(state.filter_map(|mut op| {
            match op {
                Op { term: Header, .. } | Op { term: Query, .. } => None,
                Op { ref location, ref mut atoms, .. }
                    if !location.is_zero() && atoms.len() == 1 =>
                {
                    Some((location.clone(), atoms.pop().unwrap()))
                }
                Op { .. } => None,
            }
        })))
    }
}

#[derive(Debug)]
struct LWWOrd<'a>(Frame<'a>);

impl<'a> FrameOrd<'a> for LWWOrd<'a> {
    fn primary_cmp(a: &Op, b: &Op) -> Ordering {
        UUID::weak_cmp(&a.location, &b.location)
    }

    fn secondary_cmp(a: &Op, b: &Op) -> Ordering {
        UUID::weak_cmp(&b.event, &a.event)
    }

    fn peek(&self) -> Option<&Op> {
        self.0.peek()
    }
}

impl<'a> Iterator for LWWOrd<'a> {
    type Item = Op;

    fn next(&mut self) -> Option<Op> {
        self.0.next()
    }
}

impl<'a> From<Frame<'a>> for LWWOrd<'a> {
    fn from(frame: Frame<'a>) -> Self {
        LWWOrd(frame)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lww_basic_1() {
        let f1 = Frame::parse("*lww#test1@0!");
        let f2 = Frame::parse("*lww#test1@time:a'A';");
        let exp = Frame::parse("*lww#test1@time:0!        :a      'A' ,");
        let r = LWW::reduce(f1, vec![f2]).unwrap();

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
    fn lww_basic_2() {
        let f1 = Frame::parse("*lww#test2@1:0!:a'A'");
        let f2 = Frame::parse("*lww#test2@2:b'B';");
        let exp = Frame::parse(
            "*lww#test2@2:0!
    @1  :a      'A' ,
    @2  :b      'B' ,",
        );
        let r = LWW::reduce(f1, vec![f2]).unwrap();

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
    fn lww_basic_3() {
        let f1 = Frame::parse("*lww#test3@1:a'A1';");
        let f2 = Frame::parse("*lww#test3@2:a'A2';");
        let exp = Frame::parse(
            "*lww#test3@2:1!
        :a      'A2' ,",
        );
        let r = LWW::reduce(f1, vec![f2]).unwrap();

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
    fn lww_basic_4() {
        let f1 = Frame::parse(
            "*lww#test4@2:1!
    :a  'A1'
    :b  'B1'
    :c  'C1'",
        );
        let f2 = Frame::parse(
            "*lww#test4@3:1!
    :a  'A2'
    :b  'B2'",
        );
        let exp = Frame::parse(
            "*lww#test4@3:2!
        :a      'A2' ,
        :b      'B2' ,
    @2  :c      'C1' ,",
        );
        let r = LWW::reduce(f1, vec![f2]).unwrap();

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
    fn lww_basic_5() {
        let f1 = Frame::parse(
            "*lww#array@1:0!
    :0%0 =0,
    :)1%0 =-1",
        );
        let f2 = Frame::parse(
            "*lww#array@2:0!
    :0%)1 '1',
    :)1%0 =1,
    :)1%)1 =65536",
        );
        let exp = Frame::parse(
            "*lww#array@2:0!
    @1  :0%0      =0  ,
    @2  :0%0000000001    '1' ,
        :0000000001%0    =1  ,
        :0000000001%0000000001    =65536  ,",
        );
        let r = LWW::reduce(f1, vec![f2]).unwrap();

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
    fn lww_basic_6() {
        let f1 = Frame::parse("*lww#weird@0:0!");
        let f2 = Frame::parse("*lww#weird@1 :longString 'While classic databases score 0 on the ACID\\' scale, I should probably reserve the value of -1 for one data sync system based on Operational Transforms.\\n Because of the way its OT mechanics worked, even minor glitches messed up the entire database through offset corruption. That was probably the worst case I observed in the wild. Some may build on quicksand, others need solid bedrock… but that system needed a diamond plate to stay still.' ;");
        let f3 = Frame::parse("*lww#weird@2 :pi ^3.141592653589793 ;");
        let f4 = Frame::parse("*lww#weird@3 :minus =-9223372036854775808 ;");
        let exp = Frame::parse("*lww#weird@3:0!
	@1 :longString 'While classic databases score 0 on the ACID\\' scale, I should probably reserve the value of -1 for one data sync system based on Operational Transforms.\\n Because of the way its OT mechanics worked, even minor glitches messed up the entire database through offset corruption. That was probably the worst case I observed in the wild. Some may build on quicksand, others need solid bedrock… but that system needed a diamond plate to stay still.' ,
	@3 :minus =-9223372036854775808 ,
	@2 :pi ^3.141592653589793 ,");
        let r = LWW::reduce(f1, vec![f2, f3, f4]).unwrap();

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
    fn lww_basic_7() {
        let f1 = Frame::parse("*lww#raw@1:one=1;");
        let f2 = Frame::parse("*lww#raw@2:two^2.0;");
        let f3 = Frame::parse("*lww#raw@2:three'три';");
        let exp = Frame::parse(
            "*lww#raw@2:1!
	@1 :one =1 ,
	@2 :three 'три' ,
	:two ^2.000000e+00 ,",
        );
        let r = LWW::reduce(f1, vec![f2, f3]).unwrap();

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
    fn map_basic_7() {
        use std::str::FromStr;
        let frm = Frame::parse(
            "*lww#raw@2:1!@1 :one =1 ,@2 :three 'три' ,:two ^2.000000e+00 ,",
        );
        let r = LWW::map(frm).unwrap();
        let mut exp = HashMap::default();

        exp.insert(UUID::from_str("one").unwrap(), Atom::Integer(1));
        exp.insert(UUID::from_str("two").unwrap(), Atom::Float(2.0));
        exp.insert(
            UUID::from_str("three").unwrap(),
            Atom::String("три".to_string()),
        );

        assert_eq!(exp, r);
    }

    #[test]
    fn map_empty() {
        let frm = LWW::new(UUID::now());
        let r = LWW::map(frm).unwrap();
        let exp = HashMap::default();

        assert_eq!(exp, r);
    }

    #[test]
    fn map_insert() {
        use std::iter::FromIterator;

        let key = UUID::from_str("a").unwrap();

        // empty set
        let st0 = LWW::new(UUID::now());
        let exp0 = HashMap::default();
        assert_eq!(exp0, LWW::map(st0.clone()).unwrap());

        // set 'a' to =1
        let ch1 = LWW::set(&st0, key, Atom::Integer(1)).unwrap();
        let st1 = LWW::reduce(st0, vec![ch1]).unwrap();
        let exp1 =
            HashMap::from_iter(vec![(key, Atom::Integer(1))].into_iter());
        assert_eq!(exp1, LWW::map(st1.clone()).unwrap());

        // set 'a' to =1
        let ch2 = LWW::set(&st1, key, Atom::Integer(1)).unwrap();
        let st2 = LWW::reduce(st1, vec![ch2]).unwrap();
        let exp2 =
            HashMap::from_iter(vec![(key, Atom::Integer(1))].into_iter());
        assert_eq!(exp2, LWW::map(st2.clone()).unwrap());

        // set 'a' to =2
        let ch3 = LWW::set(&st2, key, Atom::Integer(2)).unwrap();
        let st3 = LWW::reduce(st2, vec![ch3]).unwrap();
        let exp3 = HashMap::from_iter(
            vec![(key, Atom::Integer(1)), (key, Atom::Integer(2))].into_iter(),
        );
        assert_eq!(exp3, LWW::map(st3.clone()).unwrap());
    }
}
