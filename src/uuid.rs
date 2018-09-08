/// UUIDs are used by RON to identify types, objects, events, etc.
///
/// There are different kinds of UUIDs: Name, number, event and derived.
///
/// Any UUID has four parts, two related to its content, and two related to its type.
///
/// The type of an UUID is a 2-bit number called the *scheme*. The
/// subtype of an UUID is a 4-bit number called the
/// *variety*. Together, the scheme and variety specify the kind of
/// UUID.
///
/// Two other fields, the *value* and the *origin*, describe the
/// actual content of the UUID. Both are 60-bit numbers that can be
/// represented as six Base64 characters. Note that depending on the
/// UUID type, the names "value" and "origin" can be a bit of a
/// misnomer.
///
/// That leaves 2 bits to make up a 128-bit number. These 2 bits are
/// always 0 and are provided for backwards compatibility with RFC4122
/// (variant field).
#[derive(Copy, Clone, Debug)]
pub enum Uuid {
    Name { name: u64, scope: u64 },
    Number { value1: u64, value2: u64 },
    Event { timestamp: u64, origin: u64 },
    Derived { timestamp: u64, origin: u64 },
}

const BASE_PUNCT: &'static [u8] =
    b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~";

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

    pub fn to_string(&self) -> String {
        match self {
            &Uuid::Name {
                name: name,
                scope: 0,
            } => format_int(name),
            &Uuid::Name {
                name: name,
                scope: scope,
            } => format_uuid("$", name, scope),
            &Uuid::Number {
                value1: v1,
                value2: v2,
            } => format_uuid("%", v1, v2),
            &Uuid::Event {
                timestamp: ts,
                origin: origin,
            } => format_uuid("+", ts, origin),
            &Uuid::Derived {
                timestamp: ts,
                origin: origin,
            } => format_uuid("-", ts, origin),
        }
    }
}
