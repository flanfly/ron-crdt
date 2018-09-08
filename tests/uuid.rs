extern crate ron;

use ron::Uuid;

#[test]
fn name_uuid() {
    let uuid = Uuid::Name {
        name: 10,
        scope: 20,
    };

    assert_eq!(uuid.is_name(), true);
    assert_eq!(uuid.is_number(), false);
    assert_eq!(uuid.is_event(), false);
    assert_eq!(uuid.is_derived(), false);
}

#[test]
fn number_uuid() {
    let uuid = Uuid::Number {
        value1: 10,
        value2: 20,
    };

    assert_eq!(uuid.is_name(), false);
    assert_eq!(uuid.is_number(), true);
    assert_eq!(uuid.is_event(), false);
    assert_eq!(uuid.is_derived(), false);
}

#[test]
fn event_uuid() {
    let uuid = Uuid::Event {
        timestamp: 0,
        origin: 0,
    };

    assert_eq!(uuid.is_name(), false);
    assert_eq!(uuid.is_number(), false);
    assert_eq!(uuid.is_event(), true);
    assert_eq!(uuid.is_derived(), false);
}

#[test]
fn derived_uuid() {
    let uuid = Uuid::Derived {
        timestamp: 0,
        origin: 0,
    };

    assert_eq!(uuid.is_name(), false);
    assert_eq!(uuid.is_number(), false);
    assert_eq!(uuid.is_event(), false);
    assert_eq!(uuid.is_derived(), true);
}
