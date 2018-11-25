//! This is documentation for the `ron` crate.
//!
//! RON is the Replicated Object Notation, a distributed live data format
//! by [Victor Grishchenko](https://github.com/gritzko).
//!
//! RON is the language in which object states and mutations, as well
//! as all other parts of the Swarm protocol, are expressed in
//! Swarm. RON consists (solely!) of a series of UUIDs (128-bit
//! numbers), but the order of UUIDs matter, and there are many
//! different kinds of UUIDs, depending on the context.
//!
//! UUIDs provide sufficient metadata to object and their mutations to
//! allow the implementation of CRDTs in a network of peers.
//!
//! RON features two different wire formats: text and binary. Both
//! offer several ways of compression, adding to the complexity. We
//! will handle compression later, but note here that efficient
//! compression is what makes RON and Swarm practical. Compression
//! reduces the metadata overhead.
//!
//! One particular combination of four UUIDs makes up an operation
//! (short: ops) with one UUID each for the type, object, event and
//! value. Several operations make up the state or mutation of an
//! object, we call this a frame.
//!
//! Special kinds of RON ops are used for protocol handshakes and
//! frame headers (metadata for frames). These operations have special
//! meaning, and often omit some of the metadata that is usually
//! included in an operation (for example, a handshake query does not
//! have a timestamp).

extern crate smallvec;
extern crate chrono;

pub mod uuid;
pub mod op;
pub mod atom;
pub mod frame;

pub use uuid::Uuid;
pub use atom::Atom;
pub use op::{Op, Terminator};
pub use frame::Frame;
