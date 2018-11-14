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

/// Format an UUID with the given scheme and values.
fn format_uuid(punct: &'static str, v1: u64, v2: u64) -> String {
    format_int(v1) + punct + &format_int(v2)
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

    /// Render a UUID as text (without compression).
    pub fn to_string(&self) -> String {
        match self {
            &Uuid::Name { name, scope: 0 } => format_int(name),
            &Uuid::Name { name, scope } => format_uuid("$", name, scope),
            &Uuid::Number { value1, value2 } => format_uuid("%", value1, value2),
            &Uuid::Event { timestamp, origin } => format_uuid("+", timestamp, origin),
            &Uuid::Derived { timestamp, origin } => format_uuid("-", timestamp, origin),
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
    assert_eq!(uuid.to_string(), "inc");
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
    assert_eq!(uuid.to_string(), "todo$marcus");
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
    assert_eq!(uuid.to_string(), "000000000A%000000000K");
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
    assert_eq!(uuid.to_string(), "0+0");
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
    assert_eq!(uuid.to_string(), "0-0");
}
