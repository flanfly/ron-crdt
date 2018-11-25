use std::fmt;
use std::time::SystemTime;
use std::cmp::Ordering;
use std::sync::atomic::{self, AtomicUsize, ATOMIC_USIZE_INIT};

use chrono::{Utc, DateTime, Datelike, Timelike};

static UUID_NODE_ID: AtomicUsize = ATOMIC_USIZE_INIT;
static UUID_SEQUENCE: AtomicUsize = ATOMIC_USIZE_INIT;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Scheme { Name, Event, Number, Derived, }

impl fmt::Display for Scheme {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Scheme::Name => f.write_str("$"),
            Scheme::Derived => f.write_str("-"),
            Scheme::Number => f.write_str("%"),
            Scheme::Event => f.write_str("+"),
        }
    }
}

#[derive(Debug)]
enum ParserState {
    Start,
    NameOrValue{
        char_index: usize,
        partial: u64,
        is_full: bool,
    },
    Sign{
        value: u64,
        is_full: bool,
    },
    Origin{
        value: u64,
        partial: u64,
        char_index: usize,
        scheme: Scheme,
    },
}

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
     /// Sets the default UUID origin. `s` must not be longer than 10 characters and only consist of
    /// [0-9a-zA-Z~_].
    pub fn set_node_id(val: u64) {
        UUID_NODE_ID.store(val as usize, atomic::Ordering::Relaxed);
    }

    /// The current default UUID origin. Initially "0".
    pub fn node_id() -> u64 {
        UUID_NODE_ID.load(atomic::Ordering::Relaxed) as u64
    }

    /// New UUID with the default origin (see `node_id` and `set_node_id`) and the current time.
    /// Ignoring leap seconds and other events that mess with the system time all calls to this
    /// functions return unique UUID (duh).
    pub fn now() -> Self {
        Uuid::Event{
            origin: Self::node_id(),
            timestamp:  Self::timestamp(),
        }
    }

    pub fn zero() -> Self {
        Uuid::Name{ name: 0, scope: 0 }
    }

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

    pub fn parse<'a>(input: &'a str, prev_col: &Uuid, prev_row: &Uuid)
        -> Option<(Uuid, &'a str)> {

        let mut state = ParserState::Start;
        let mut prev = prev_col;

        for (off, ch) in input.char_indices() {
            eprintln!("ch: {} state: {:?}", ch, state);
            match ch as u8 {
                // base64 char
                b'0'...b'9' | b'A'...b'Z' | b'_' | b'a'...b'z' | b'~' => {
                    let val = match ch as u8 {
                        ch@b'0'...b'9' => ch - b'0',
                        ch@b'A'...b'Z' => ch - b'A' + 10,
                        b'_' => 36,
                        ch@b'a'...b'z' => ch - b'a' + 37,
                        b'~' => 63,
                        _ => unreachable!(),
                    };

                    match state {
                        ParserState::Start => {
                            state = ParserState::NameOrValue{
                                char_index: 8,
                                partial: (val as u64) << (9 * 6),
                                is_full: true,
                            };
                        }
                        ParserState::NameOrValue{ partial, char_index, is_full }
                        if char_index > 0 => {
                            state = ParserState::NameOrValue{
                                char_index: char_index - 1,
                                partial: partial | ((val as u64) << (char_index * 6)),
                                is_full: is_full,
                            };
                        }
                        ParserState::NameOrValue{ partial, char_index: 0, is_full } => {
                            state = ParserState::Sign{
                                value: partial | (val as u64),
                                is_full: is_full,
                            };
                        }
                        ParserState::Origin{ value, scheme, partial,
                                             char_index } if char_index > 0 => {
                            state = ParserState::Origin{
                                value: value,
                                scheme: scheme,
                                char_index: char_index - 1,
                                partial: partial | ((val as u64) << (char_index * 6)),
                            };
                        }
                        ParserState::Origin{ value, scheme, partial,
                                             char_index: 0 } => {
                            let lo = partial | (val as u64);
                            let uu = Uuid::new(value, lo, scheme);
                            return Some((uu, &input[off..]));
                        }

                        _ => { return None; }
                    }
                }
                // backref to column
                b'(' | b'[' | b'{' | b'}' | b']' | b')' => {
                    let (mask,ch) = match ch as u8 {
                        b'(' => (0xffffff000000000,5),
                        b'[' => (0xfffffffc0000000,6),
                        b'{' => (0xfffffffff000000,7),
                        b'}' => (0xffffffffffc0000,8),
                        b']' => (0xffffffffffff000,9),
                        b')' => (0xfffffffffffffc0,10),
                        _ => unreachable!(),
                    };

                    match state {
                        ParserState::Start => {
                            state = ParserState::NameOrValue{
                                partial: prev.high() & mask,
                                char_index: 10 - ch,
                                is_full: false,
                            };
                        }
                        ParserState::NameOrValue{ partial,.. } => {
                            state = ParserState::Origin{
                                scheme: prev.scheme(),
                                value: partial,
                                partial: prev.low() & mask,
                                char_index: 10 - ch,
                            };
                        }
                        ParserState::Sign{ value,.. } => {
                            state = ParserState::Origin{
                                scheme: prev.scheme(),
                                value: value,
                                partial: prev.low() & mask,
                                char_index: 10 - ch,
                            };
                        }
                        ParserState::Origin{ value, scheme, char_index: 9,.. } => {
                            state = ParserState::Origin{
                                scheme: scheme,
                                value: value,
                                partial: prev.low() & mask,
                                char_index: 10 - ch,
                            };
                        }
                        _ => { return None; }
                    }
                }

                // backref to row
                b'`' => {
                    prev = prev_row;
                }

                // hi/lo division
                b'+' | b'%' | b'-' | b'$' => {
                    let sch = match ch as u8 {
                        b'+' => Scheme::Event,
                        b'%' => Scheme::Number,
                        b'-' => Scheme::Derived,
                        b'$' => Scheme::Name,
                        _ => unreachable!(),
                    };

                    match state {
                        ParserState::Start => {
                            state = ParserState::Origin{
                                scheme: sch,
                                value: prev.high(),
                                partial: 0,
                                char_index: 9,
                            };
                        }
                        ParserState::NameOrValue{ partial,.. } => {
                            state = ParserState::Origin{
                                scheme: sch,
                                value: partial,
                                partial: 0,
                                char_index: 9,
                            };
                        }
                        ParserState::Sign{ value,.. } => {
                            state = ParserState::Origin{
                                scheme: sch,
                                value: value,
                                partial: 0,
                                char_index: 9,
                            };
                        }
                        _ => { return None; }
                    }
                }

                // unrecognized char
                _ => {
                    match state {
                        ParserState::Start => {
                            let uu = Uuid::zero();
                            return Some((uu, &input[off..]));
                        }
                        ParserState::NameOrValue{ partial, is_full: false,.. } => {
                            let uu = Uuid::new(partial, prev.low(), prev.scheme());
                            return Some((uu, &input[off..]));
                        }
                        ParserState::NameOrValue{ partial,.. } => {
                            let uu = Uuid::Name{ name: partial, scope: 0 };
                            return Some((uu, &input[off..]));
                        }
                        ParserState::Sign{ value, is_full: false } => {
                            let uu = Uuid::new(value, prev.low(), prev.scheme());
                            return Some((uu, &input[off..]));
                        }
                        ParserState::Sign{ value,.. } => {
                            let uu = Uuid::Name{ name: value, scope: 0 };
                            return Some((uu, &input[off..]));
                        }
                        ParserState::Origin{ value, partial, scheme,.. } => {
                            let uu = Uuid::new(value, partial, scheme);
                            return Some((uu, &input[off..]));
                        }
                    }
                }
            }
        }

        eprintln!("EOF state: {:?}", state);
        // stream ended
        match state {
            ParserState::Start => {
                let uu = Uuid::zero();
                return Some((uu, &input[0..0]));
            }
            ParserState::NameOrValue{ partial, is_full: false,.. } => {
                let uu = Uuid::new(partial, prev.low(), prev.scheme());
                return Some((uu, &input[0..0]));
            }
            ParserState::NameOrValue{ partial,.. } => {
                let uu = Uuid::Name{ name: partial, scope: 0 };
                return Some((uu, &input[0..0]));
            }
            ParserState::Sign{ value, is_full: false } => {
                let uu = Uuid::new(value, prev.low(), prev.scheme());
                return Some((uu, &input[0..0]));
            }
            ParserState::Sign{ value,.. } => {
                let uu = Uuid::Name{ name: value, scope: 0 };
                return Some((uu, &input[0..0]));
            }
            ParserState::Origin{ value, scheme, char_index: 9,.. } => {
                let uu = Uuid::new(value, prev.low(), scheme);
                return Some((uu, &input[0..0]));
            }
            ParserState::Origin{ value, partial, scheme,.. } => {
                let uu = Uuid::new(value, partial, scheme);
                return Some((uu, &input[0..0]));
            }
        }
    }

    pub fn compress(&self, prev_col: &Uuid, _prev_row: &Uuid) -> String {
        if self.high() >> 60 != prev_col.high() >> 60 {
            // don't compress UUIDs with different varities
            return format!("{}", self)
        }

        let ret = Self::compress_int64(self.high(), prev_col.high());
        if self.low() == 0 && self.scheme() == Scheme::Name {
            ret
        } else {
            let origin = Self::compress_int64(self.low(), prev_col.low());
            let orig_is_compr = origin.bytes().next().map(|c| {
                c == b'{' || c == b'[' || c == b'(' ||
                c == b'}' || c == b']' || c == b')'
            }).unwrap_or(false);

            if self.scheme() == prev_col.scheme() && orig_is_compr && !ret.is_empty() {
                ret + &origin
            } else {
                ret + &format!("{}{}", self.scheme(), origin)
            }
        }
    }

    fn compress_int64(value: u64, ctx: u64) -> String {
        if value == ctx { return "".to_string(); }

        let value = Self::int64_to_str(value, false);
        let ctx = Self::int64_to_str(ctx, false);
        let zip = value.bytes().zip(ctx.bytes()).collect::<Vec<_>>();
        let prfx_len = zip.iter().position(|(a,b)| a != b).unwrap_or(zip.len());

        let ret = match prfx_len {
            4 => format!("({}", &value[4..]),
            5 => format!("[{}", &value[5..]),
            6 => format!("{{{}", &value[6..]),
            7 => format!("}}{}", &value[7..]),
            8 => format!("]{}", &value[8..]),
            9 => format!("){}", &value[9..]),
            _ => value,
        };

        let ret = ret.trim_end_matches('0').to_string();
        if ret.is_empty() { "0".to_string() } else { ret }
    }

    fn int64_to_str(int: u64, truncate: bool) -> String {
        let mut ret = String::default();

        for idx in 0..10 {
            let idx = (9 - idx) * 6;
            match ((int >> idx) & 63) as u8 {
                ch@0...9 => { ret += &format!("{}", (b'0' + ch) as char); }
                ch@10...35 => { ret += &format!("{}", (b'A' + ch - 10) as char); }
                36 => { ret.push('_'); }
                ch@37...62 => { ret += &format!("{}", (b'a' + ch - 37) as char); }
                63 => { ret.push('~'); }
                _ => unreachable!(),
            }
        }

        if truncate {
            let ret = ret.trim_end_matches('0').to_string();
            if ret.is_empty() { "0".to_string() } else { ret }
        } else {
            ret
        }
    }

    fn scheme(&self) -> Scheme {
        match self {
            Uuid::Name{ .. } => Scheme::Name,
            Uuid::Event{ .. } => Scheme::Event,
            Uuid::Number{ .. } => Scheme::Number,
            Uuid::Derived{ .. } => Scheme::Derived,
        }
    }

    fn high(&self) -> u64 {
        match self {
            &Uuid::Name{ name,.. } => name,
            &Uuid::Event{ timestamp,.. } => timestamp,
            &Uuid::Number{ value1,.. } => value1,
            &Uuid::Derived{ timestamp,.. } => timestamp,
        }
    }

    fn low(&self) -> u64 {
        match self {
            &Uuid::Name{ scope,.. } => scope,
            &Uuid::Event{ origin,.. } => origin,
            &Uuid::Number{ value2,.. } => value2,
            &Uuid::Derived{ origin,.. } => origin,
        }
    }

    fn new(hi: u64, lo: u64, sch: Scheme) -> Self {
        match sch {
            Scheme::Name => Uuid::Name{ name: hi, scope: lo },
            Scheme::Event => Uuid::Event{ timestamp: hi, origin: lo },
            Scheme::Derived => Uuid::Derived{ timestamp: hi, origin: lo },
            Scheme::Number => Uuid::Number{ value1: hi, value2: lo },
        }
    }

    fn timestamp() -> u64 {
        use std::time::Duration;
        use std::thread::sleep;

        let now = SystemTime::now();
        let seq = (UUID_SEQUENCE.fetch_add(1, atomic::Ordering::SeqCst) & 0b111111_111111) as u64;

        // we wait for the clock to advance after sequence number overflows to avoid creating
        // duplicated uuids. This can happen if we try to generate more than 4k instances in
        // under one ms.
        if seq == 0b111111_111111 {
            while now == SystemTime::now() {
                sleep(Duration::from_millis(1));
            }
        }

        let dt: DateTime<Utc> = now.into();
        let months = (2010 + dt.year() as u64 * 12 + dt.month0() as u64) & 0b111111_111111;;
        let ts = (months << (8 * 6))
               | ((dt.day0() as u64   & 0b111111) << (7 * 6))
               | ((dt.hour() as u64   & 0b111111) << (6 * 6))
               | ((dt.minute() as u64 & 0b111111) << (5 * 6))
               | ((dt.second() as u64 & 0b111111) << (4 * 6))
               | (((dt.nanosecond() as u64 / 1_000_000) & 0b111111_111111) << (2 * 6))
               | seq;

        ts
    }

}

impl Default for Uuid {
    fn default() -> Self { Uuid::zero() }
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

#[test]
fn compress() {
    let tris = vec![
        ("}DcR-L8w", "}IYI-", "}IYI-0"),
        ("0$author", "name$author2", "name{2"),
        ("0", "1", "1"),
        ("0", "123-0", "123-"),
        ("0", "0000000001-orig", ")1-orig"),
        ("1time01-src", "1time02+src", "{2+"),
        ("hash%here", "hash%there", "%there"),
        ("1", ")1", "0000000001"), //7
        ("0", "name$0", "name"),
        ("time+orig", "time1+orig2", "(1(2"),
        ("time-orig", "time1+orig2", "(1+(2"),
        ("[1s9L3-[Wj8oO", "[1s9L3-(2Biejq", "-(2Biejq"),
        ("}DcR-}L8w", "}IYI-", "}IYI}"), //12
        ("A$B", "A-B", "-"),
    ];
    let z = Uuid::zero();

    for (ctx, uu, exp) in tris {
        let ctx = Uuid::parse(ctx, &z, &z).unwrap().0;
        let uu = Uuid::parse(uu, &z, &z).unwrap().0;
        let comp = uu.compress(&ctx, &ctx);

        assert_eq!(comp, exp);
    }
}

#[test]
fn parse_some() {
    let tris = vec![
        ("0", "1", "1"), // 0
        ("1-x", ")1", "1000000001-x"),
        ("test-1", "-", "test-1"),
        ("hello-111", "[world", "helloworld-111"),
        ("helloworld-111", "[", "hello-111"),
        ("100001-orig", "[", "1-orig"), // 5
        ("1+orig", "(2-", "10002-orig"),
        ("time+orig", "(1(2", "time1+orig2"),
        // TODO		("name$user", "$scoped", "scoped$user"),
        ("any-thing", "hash%here", "hash%here"),
        ("[1s9L3-[Wj8oO", "-(2Biejq", "[1s9L3-(2Biejq"), // 9
        ("0123456789-abcdefghij", ")~)~", "012345678~-abcdefghi~"),
        ("(2-[1jHH~", "-[00yAl", "(2-}yAl"),
        ("0123G-abcdb", "(4566(efF", "01234566-abcdefF"),
    ];
    let z = Uuid::zero();

    for (ctx, uu, exp) in tris {
        eprintln!("{}, {}, {}", ctx, uu, exp);
        let ctx = Uuid::parse(ctx, &z, &z).unwrap().0;
        eprintln!("ctx: {:?}", ctx);
        eprintln!("parse ctx: {}", ctx);
        let uu = Uuid::parse(uu, &ctx, &ctx).unwrap().0;
        eprintln!("uu: {:?}", uu);
        eprintln!("parse uu: {}", uu);

        assert_eq!(Uuid::compress(&uu, &z, &z), exp);
    }
}

#[test]
fn parse_all() {
    let pairs = vec![
        ("-", "0123456789-abcdefghi"),   // 00000
        ("B", "B"),                      // 00001
        ("(", "0123-abcdefghi"),         // 00010
        ("(B", "0123B-abcdefghi"),       // 00011
        ("+", "0123456789+abcdefghi"),   // 00100
        ("+B", "0123456789+B"),          // 00101
        ("+(", "0123456789+abcd"),       // 00110
        ("+(B", "0123456789+abcdB"),     // 00111
        ("A", "A"),                      // 01000 8
        ("AB", "AB"),                    // 01001
        ("A(", "A-abcd"),                // 01010
        ("A(B", "A-abcdB"),              // 01011
        ("A+", "A+abcdefghi"),           // 01100
        ("A+B", "A+B"),                  // 01101
        ("A+(", "A+abcd"),               // 01110
        ("A+(B", "A+abcdB"),             // 01111
        (")", "012345678-abcdefghi"),    // 10000 16
        (")B", "012345678B-abcdefghi"),  // 10001
        (")(", "012345678-abcd"),        // 10010
        (")(B", "012345678-abcdB"),      // 10011
        (")+", "012345678+abcdefghi"),   // 10100
        (")+B", "012345678+B"),          // 10101
        (")+(", "012345678+abcd"),       // 10110
        (")+(B", "012345678+abcdB"),     // 10111
        (")A", "012345678A-abcdefghi"),  // 11000
        (")AB", ""),                     // 11001 error - length
        (")A(", "012345678A-abcd"),      // 11010
        (")A(B", "012345678A-abcdB"),    // 11011
        (")A+", "012345678A+abcdefghi"), // 11100
        (")A+B", "012345678A+B"),        // 11101
        (")A+(", "012345678A+abcd"),     // 11110
        (")A+(B", "012345678A+abcdB"),   // 11111
        ];
    let z = Uuid::zero();
    let ctx = Uuid::parse("0123456789-abcdefghi", &z, &z).unwrap().0;
    for (uu, exp) in pairs {
        if exp.is_empty() {
            assert!(Uuid::parse(uu, &ctx, &ctx).is_none());
        } else {
            let uu = Uuid::parse(uu, &ctx, &ctx).unwrap().0;
            assert_eq!(format!("{}", uu), exp);
        }
    }
}

