# dbc-rs
[![Build Status](https://travis-ci.org/marcelbuesing/dbc-parser.svg?branch=dev)](https://travis-ci.org/marcelbuesing/dbc-parser)
[![codecov](https://codecov.io/gh/marcelbuesing/dbc-rs/branch/dev/graph/badge.svg)](https://codecov.io/gh/marcelbuesing/dbc-rs)

A CAN-dbc format parser written with Rust's nom parser.

# Implemented DBC parts

- [x] BA_ (NetworkNode Attribute)
- [x] BA_ (MessageDefinition Attribute)
- [x] BA_ (Signal Attribute)
- [x] BA_ (Env Var Attribute)
- [x] BA_ (Raw Attribute)
- [x] BA_DEF_ BU_
- [x] BA_DEF_ BO_
- [x] BA_DEF_DEF_
- [ ] BA_DEF_SGTYPE_
- [ ] BA_SGTYPE_
- [x] BU_
- [x] BO_
- [ ] CAT_
- [ ] CAT_DEF_
- [x] CM_ SG_
- [x] CM_ BO_
- [x] ENVVAR_DATA_
- [x] EV_
- [ ] EV_DATA_
- [ ] FILTER
- [x] NS_
- [ ] NS_DESC_
- [ ] SGTYPE_
- [ ] SGTYPE_VAL_
- [ ] SIG_GROUP_
- [ ] SIG_TYPE_REF_
- [ ] SIG_VALTYPE_
- [ ] SIGTYPE_VALTYPE_
- [x] VAL_ (ENV_VAR)
- [x] VAL_ (SIGNAL)
- [ ] VAL_TABLE_
- [x] VERSION

# Example
The file parser simply parses a dbc input file.
`cargo test && ./target/debug/examples/file_parser -i examples/sample.dbc`