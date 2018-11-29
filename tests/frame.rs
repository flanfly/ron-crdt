extern crate ron;

use ron::Frame;
use ron::Op;
use ron::Terminator;
use ron::Uuid;

#[test]
fn op() {
    let _op = Op {
        ty: Uuid::Name { name: 0, scope: 0 },
        object: Uuid::Event { timestamp: 0, origin: 0 },
        event: Uuid::Event { timestamp: 0, origin: 0 },
        location: Uuid::Name { name: 0, scope: 0 },
        atoms: vec![].into(),
        term: Terminator::Reduced,
    };
}

#[test]
fn frame() {
    let op = Op {
        ty: Uuid::Name {
            name: 824893205576155136, // "inc"
            scope: 0,
        },
        object: Uuid::Event { timestamp: 0, origin: 0 },
        event: Uuid::Event { timestamp: 0, origin: 0 },
        location: Uuid::Name { name: 0, scope: 0 },
        atoms: vec![].into(),
        term: Terminator::Raw,
    };

    let frame = Frame::compress(vec![op]);

    assert_eq!(frame.body(), "*inc#+@+;");
}
