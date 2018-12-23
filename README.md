Replicated Object Notation (RON) for Rust
=========================================

[RON](https://github.com/gritzko/ron) is a protocol for synchronizing
Conflict-free Replicated Datatypes (CmRDT). The
[documentation](https://docs.rs/ron-crdt) is hosted on `docs.rs`.

Differences with the [reference implementation](https://github.com/gritzko/ron):

1. Number vs Hash for UUID with variety 0?
1. NewIntAtom(0)

Usage
-----

```toml
# Cargo.toml
[dependencies]
ron-crdt = "0.1"
```

License
-------

This project is licensed under either of

 * GNU Lesser General Public License, Version 2.1 or later, ([LICENSE-LGPL2](LICENSE-LGPL2) or
   https://www.gnu.org/licenses/old-licenses/lgpl-2.1.en.html
 * MIT license, ([LICENSE-MIT](LICENSE-MIT)

at your option.

