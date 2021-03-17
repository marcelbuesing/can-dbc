# can-dbc
[![LICENSE](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![VERSION](https://img.shields.io/crates/v/can-dbc.svg)](https://crates.io/crates/can-dbc)
[![Actions Status](https://github.com/marcelbuesing/can-dbc/workflows/Continuous%20integration/badge.svg)](https://github.com/marcelbuesing/can-dbc/actions?query=workflow%3A"Continuous+integration")
[![codecov](https://codecov.io/gh/marcelbuesing/can-dbc/branch/dev/graph/badge.svg)](https://codecov.io/gh/marcelbuesing/can-dbc)
[![docs](https://docs.rs/can-dbc/badge.svg)](https://docs.rs/can-dbc)
[![Cargo Deny Status](https://img.shields.io/badge/cargo--deny-license%20checked-green)](https://github.com/marcelbuesing/can-dbc/actions?query=workflow%3A"Continuous+integration")

A CAN-dbc format parser written with Rust's [nom](https://github.com/Geal/nom) parser combinator library.

# 1. Example

Read dbc file and generate Rust structs based on the messages/signals defined in the dbc.

```rust
use can_dbc::DBC;
use codegen::Scope;

use std::fs::File;
use std::io;
use std::io::prelude::*;

fn main() -> io::Result<()> {
    let mut f = File::open("./examples/sample.dbc")?;
    let mut buffer = Vec::new();
    f.read_to_end(&mut buffer)?;

    let dbc = can_dbc::DBC::from_slice(&buffer).expect("Failed to parse dbc file");

    let mut scope = Scope::new();
    for message in dbc.messages() {
        for signal in message.signals() {

            let mut scope = Scope::new();
            let message_struct = scope.new_struct(message.message_name());
            for signal in message.signals() {
                message_struct.field(signal.name().to_lowercase().as_str(), "f64");
            }
        }
    }

    println!("{}", scope.to_string());
    Ok(())
}
```

# 2. Example

The file parser simply parses a dbc input file and prints the parsed content.
```
cargo test && ./target/debug/examples/file_parser -i examples/sample.dbc
```

# Installation
can-dbc is available on crates.io and can be included in your Cargo enabled project like this:

```yml
[dependencies]
can-dbc = "3.0"
```

# Implemented DBC parts

- [x] version
- [x] new_symbols
- [x] bit_timing *(deprecated but mandatory)*
- [x] nodes
- [x] value_tables
- [x] messages
- [x] message_transmitters
- [x] environment_variables
- [x] environment_variables_data
- [x] signal_types
- [x] comments
- [x] attribute_definitions
- [ ] sigtype_attr_list *(format missing documentation)*
- [x] attribute_defaults
- [x] attribute_values
- [x] value_descriptions
- [ ] category_definitions *(deprecated)*
- [ ] categories *(deprecated)*
- [ ] filter *(deprecated)*
- [x] signal_type_refs
- [x] signal_groups
- [x] signal_extended_value_type_list

# Deviating from standard
- multispace between parsers instead of single space allowing e.g. (two spaces) `SIG_GROUP  13`.
- `VAL_` suffix may be ` ;` or `;`

# Alternatives
- [canparse](https://github.com/jmagnuson/canparse)

# Credits
Test dbcs files were copied from the [cantools](https://github.com/eerimoq/cantools) project.

# License Checks

This project uses [cargo-deny](https://github.com/EmbarkStudios/cargo-deny) for checking the licenses of dependencies. To run the check locally run the following:

```
cargo install cargo-deny
cargo deny check
```
