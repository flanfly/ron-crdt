use uuid;
use Atom;
use Uuid;
use Op;
use Terminator;
use Frame;

use std::str;

fn create_op(ty: Uuid, object: Uuid, event: Uuid, location: Uuid, term: Terminator) -> Op {
    Op {
        ty: ty,
        object: object,
        event: event,
        location: location,
        atoms: vec![].into(),
        term: term,
    }
}

fn create_uuid(scheme: char, val1: u64, val2: u64) -> Result<Uuid, &'static str> {
    match scheme {
        '$' => Ok(Uuid::Name {
            name: val1,
            scope: val2,
        }),
        '%' => Ok(Uuid::Number {
            value1: val1,
            value2: val2,
        }),
        '+' => Ok(Uuid::Event {
            timestamp: val1,
            origin: val2,
        }),
        '-' => Ok(Uuid::Derived {
            timestamp: val1,
            origin: val2,
        }),
        _ => Err("invalid scheme"),
    }
}

fn create_base64(input: &Vec<char>) -> u64 {
    let mut left = 10;
    let mut result = 0u64;

    for chr in input {
        let mut cp = [0; 1];
        chr.encode_utf8(&mut cp);

        let val = match chr {
            '0'...'9' => cp[0] - 48,
            'A'...'Z' => cp[0] - 65 + 10,
            '_' => 36,
            'a'...'z' => cp[0] - 97 + 37,
            '~' => 63,
            _ => panic!("never happens"),
        };
        result <<= 6;
        result += val as u64;
        left -= 1;
    }
    result << left * 6
}

named!(base64<&[u8], u64>, map!(complete!(many_m_n!(0, 10, one_of!(uuid::BASE_PUNCT))),
 |arg| create_base64(&arg)));

named!(uuid<&[u8], Uuid>,
       alt_complete!(
           do_parse!(val1: base64
               >> scheme: one_of!(&b"$%+-"[..])
               >> val2: base64 >> (create_uuid(scheme, val1, val2).unwrap()))
               | do_parse!(val1: base64 >> (create_uuid('$', val1, 0).unwrap()))
       )
);

// fn to_s(i:Vec<u8>) -> String {
//   String::from_utf8_lossy(&i).into_owned()
// }

// named!(string_atom_core < String >,
//    map!(
//      escaped_transform!(call!(alpha), '\\',
//        alt!(
//            tag!("\\")       => { |_| &b"\\"[..] }
//          | tag!("\'")       => { |_| &b"\'"[..] }
//          | tag!("n")        => { |_| &b"\n"[..] }
//          | tag!("t")        => { |_| &b"\t"[..] }
//          | tag!("r")        => { |_| &b"\r"[..] }
//        )
//      ), to_s
//    )
//  );

// named!(string_atom<&str, Atom>,
//        do_parse!(tag!("'") >> string: string_atom_core >> tag!("'")
//                            >> (Atom::String(string[..].to_string())))
// );

// named!(uuid_atom<&str, Atom>,
//        do_parse!(tag!(">") >> uuid: uuid >> (Atom::Uuid(uuid)))
// );

// named!(int_atom<&str, Atom>,
//        do_parse!(tag!("=") >> uuid: uuid >> (Atom::Integer(0)))
// );

// named!(float_atom<&str, Atom>,
//        do_parse!(tag!("^") >> uuid: uuid >> (Atom::Float(0.0)))
// );

// named!(atom<&str, Atom>,
//        alt_complete!(uuid_atom | integer_atom | float_atom | string_atom)
// );

named!(opterm<&[u8], Terminator>,
       map!(opt!(complete!(map_res!(one_of!(&b";?!,"[..]), Terminator::from_char))),
       |arg| arg.unwrap_or_default()));

// next TODO: atoms!
named!(op<&[u8], Op>,
       do_parse!(tag!(b"*") >> ty: uuid
                 >> tag!(b"#") >> object: uuid
                 >> tag!(b"@") >> event: uuid
                 >> tag!(b":") >> location: uuid
                 >> term: opterm
                 >> (create_op(ty, object, event, location, term))));

// A frame is a sequence of ops, followed optionally by a terminator.
named!(frame<&[u8], Frame>,
  do_parse!(
      ops: many0!(complete!(op)) >>
      dot: opt!(complete!(tag!(b"."))) >>
      (Frame { terminate: dot.is_some(), ops: ops })
    )
);

#[test]
fn parse_text() {
    assert_eq!(
        frame(b"*inc#mouse$client@xyz+client:xyz1-client?."),
        Ok((
            &b""[..],
            Frame {
                terminate: true,
                ops: vec![
                    Op {
                        ty: Uuid::Name {
                            name: 824893205576155136,
                            scope: 0,
                        },
                        object: Uuid::Event {
                            timestamp: 0,
                            origin: 0,
                        },
                        event: Uuid::Event {
                            timestamp: 0,
                            origin: 0,
                        },
                        location: Uuid::Name { name: 0, scope: 0 },
                        atoms: vec![].into(),
                        term: Terminator::Query,
                    },
                ],
            },
        ))
    );
}

#[test]
fn parse_text2() {
    assert_eq!(
        frame(b""),
        Ok((
            &b""[..],
            Frame {
                terminate: false,
                ops: vec![],
            },
        ))
    );
}
