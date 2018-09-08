extern crate ron;

use ron::Atom;
use ron::Uuid;

#[test]
fn atom_uuid() {
    let atom = Atom::Uuid(Uuid::Name { name: 0, scope: 0 });
    assert_eq!(atom.is_uuid(), true);
    assert_eq!(atom.is_integer(), false);
    assert_eq!(atom.is_float(), false);
    assert_eq!(atom.is_string(), false);
}
