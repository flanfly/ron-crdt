extern crate ron;

use ron::Frame;
use ron::Op;
use ron::Terminator;
use ron::UUID;

#[test]
fn op() {
    let _op = Op {
        ty: UUID::Name { name: 0, scope: 0 },
        object: UUID::Event { timestamp: 0, origin: 0 },
        event: UUID::Event { timestamp: 0, origin: 0 },
        location: UUID::Name { name: 0, scope: 0 },
        atoms: vec![].into(),
        term: Terminator::Reduced,
    };
}

#[test]
fn frame() {
    let op = Op {
        ty: UUID::Name {
            name: 824893205576155136, // "inc"
            scope: 0,
        },
        object: UUID::Event { timestamp: 0, origin: 0 },
        event: UUID::Event { timestamp: 0, origin: 0 },
        location: UUID::Name { name: 0, scope: 0 },
        atoms: vec![].into(),
        term: Terminator::Raw,
    };

    let frame = Frame::compress(vec![op]);

    assert_eq!(frame.body(), "*inc#0+0@0+0:0;");
}
