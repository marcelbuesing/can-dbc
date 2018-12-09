# dbc-rs
[![Build Status](https://travis-ci.org/marcelbuesing/can-dbc.svg?branch=dev)](https://travis-ci.org/marcelbuesing/can-dbc)
[![codecov](https://codecov.io/gh/marcelbuesing/can-dbc/branch/dev/graph/badge.svg)](https://codecov.io/gh/marcelbuesing/dbc-rs)

A CAN-dbc format parser written with Rust's nom parser.

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
- [ ] sigtype_attr_list
- [x] attribute_defaults
- [x] attribute_values
- [x] value_descriptions
- [ ] category_definitions *(deprecated)*
- [ ] categories *(deprecated)*
- [ ] filter *(deprecated)*
- [x] signal_type_refs
- [x] signal_groups
- [x] signal_extended_value_type_list
# Example
The file parser simply parses a dbc input file.
`cargo test && ./target/debug/examples/file_parser -i examples/sample.dbc`