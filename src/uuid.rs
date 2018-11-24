use std::fmt;

/// UUIDs are used by RON to identify types, objects, events, etc.
///
/// There are different kinds of UUIDs: Name, number, event and derived.
///
/// Any UUID has four parts, two related to its content, and two related to its type.
///
/// The type of an UUID is a 2-bit number called the *scheme*. The
/// subtype of an UUID is a 4-bit number called the
/// *variety* (currently unsupported). Together, the scheme and variety specify the kind of
/// UUID.
///
/// Two other fields describe the actual content of the UUID. Both are 60-bit numbers that can be
/// represented as ten-digit Base64 characters.
///
/// That leaves 2 bits to make up a 128-bit number. These 2 bits are
/// always 0 and are provided for backwards compatibility with RFC4122
/// (variant field).
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Uuid {
    /// Name UUIDs are often used to encode short string atoms, such as types (e.g. `lww`).
    /// The string is read as a Base64-literal to determine the actual (numeric) UUID component.
    /// Name UUIDs can be *global* (e.g. `lww` = `lww$0`) or *scoped* (e.g. `dbtest$client1`)
    Name { name: u64, scope: u64 },

    /// Number UUIDs encode numbers (e.g. indices into a matrix) and hash values. The meaning of
    /// the two values is context-dependent.
    Number { value1: u64, value2: u64 },

    /// Event UUIDs are Lamport-timestamps that are used to identify operations and objects.
    /// The timestamp is clock-dependent, while the origin is a replica ID.
    Event { timestamp: u64, origin: u64 },

    /// Derived UUIDs refer to an event ID, without being the event ID.
    Derived { timestamp: u64, origin: u64 },
}

/// This is the alphabet for the Swarm variant of Base64 encoding.
pub const BASE_PUNCT: &'static [u8] =
    b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~";

/// Format a 60-bit integer as Base64.
fn format_int(value: u64) -> String {
    if value == 0 {
        return "0".to_string();
    }

    // Trailing zeros are skipped.
    let tail = value.trailing_zeros() / 6;
    let mut result = String::default();
    for idx in 0..(10 - tail) {
        let digit = (value >> (9 - idx) * 6) & 0x3f;
        result.push(BASE_PUNCT[digit as usize] as char);
    }
    result
}

impl Uuid {
    /// Return true if and only if this is a name Uuid.
    pub fn is_name(&self) -> bool {
        match self {
            &Uuid::Name { .. } => true,
            _ => false,
        }
    }

    /// Return true if and only if this is a number Uuid.
    pub fn is_number(&self) -> bool {
        match self {
            &Uuid::Number { .. } => true,
            _ => false,
        }
    }

    /// Return true if and only if this is an event Uuid.
    pub fn is_event(&self) -> bool {
        match self {
            &Uuid::Event { .. } => true,
            _ => false,
        }
    }

    /// Return true if and only if this is a derived Uuid.
    pub fn is_derived(&self) -> bool {
        match self {
            &Uuid::Derived { .. } => true,
            _ => false,
        }
    }
}

impl fmt::Display for Uuid {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Uuid::Name{ name: 0, scope: 0,.. } => f.write_str("0"),
            &Uuid::Name{ name, scope: 0 } =>
                f.write_str(&format_int(name)),
            &Uuid::Name{ name, scope } => {
                f.write_str(&format_int(name))?;
                f.write_str("$")?;
                f.write_str(&format_int(scope))
            }
            &Uuid::Number{ value1, value2 } => {
                f.write_str(&format_int(value1))?;
                f.write_str("%")?;
                f.write_str(&format_int(value2))
            }
            &Uuid::Derived{ timestamp, origin } => {
                f.write_str(&format_int(timestamp))?;
                f.write_str("-")?;
                f.write_str(&format_int(origin))
            }
            &Uuid::Event{ timestamp, origin } => {
                f.write_str(&format_int(timestamp))?;
                f.write_str("+")?;
                f.write_str(&format_int(origin))
            }
        }
    }
}

#[test]
fn global_name_uuid() {
    let uuid = Uuid::Name {
        name: 824893205576155136,
        scope: 0,
    };

    assert_eq!(uuid.is_name(), true);
    assert_eq!(uuid.is_number(), false);
    assert_eq!(uuid.is_event(), false);
    assert_eq!(uuid.is_derived(), false);
    assert_eq!(format!("{}", uuid), "inc");
}

#[test]
fn scoped_name_uuid() {
    let uuid = Uuid::Name {
        name: 1023340966896992256,
        scope: 893360337800134656,
    };

    assert_eq!(uuid.is_name(), true);
    assert_eq!(uuid.is_number(), false);
    assert_eq!(uuid.is_event(), false);
    assert_eq!(uuid.is_derived(), false);
    assert_eq!(format!("{}", uuid), "todo$marcus");
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
    assert_eq!(format!("{}", uuid), "000000000A%000000000K");
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
    assert_eq!(format!("{}", uuid), "0+0");
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
    assert_eq!(format!("{}", uuid), "0-0");
}

#[test]
fn well_known() {
    let error = Uuid::Name{ name: 1152921504606846975, scope: 0 };
    let never = Uuid::Name{ name: 1134907106097364992, scope: 0 };
    let inc = Uuid::Name{ name: 824893205576155136, scope: 0 };

    assert_eq!(format!("{}", error), "~~~~~~~~~~");
    assert_eq!(format!("{}", never), "~");
    assert_eq!(format!("{}", inc), "inc");
}
