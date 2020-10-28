# Change Log
## [3.0.2](https://github.com/marcelbuesing/can-dbc/tree/3.0.1) (2020-10-28)
- Fix: Allow trailing whitespaces in dbc. Thanks @killercup.

## [3.0.1](https://github.com/marcelbuesing/can-dbc/tree/3.0.1) (2019-05-17)
- Fix: Allow zero time occurence of BU name
- Remove Cargo.lock file

## [3.0.0](https://github.com/marcelbuesing/can-dbc/tree/3.0.0) (2019-11-25)
- BREAKING: Pass `MessageId`'s by value
- Cleanup clippy warnings

## [2.0.0](https://github.com/marcelbuesing/can-dbc/tree/2.0.0) (2019-04-09)
- Change CAN message id type from `u64` to `u32`.
- Update dependencies

## [1.1.0](https://github.com/marcelbuesing/can-dbc/tree/1.1.0) (2019-01-18)
- Add optional feature `with-serde` and derive Serialize for structs and enums.

## [1.0.1](https://github.com/marcelbuesing/can-dbc/tree/1.0.1) (2019-01-15)

### dbcc
- Add first version of dbc to rust compiler

### can-dbc
- Fix plain attribute definition
- Replace singlespace with multispace seperators (less strict)
- Allow multiple signal groups in DBC document
- Accept signal-less messages
- Accept lists in message transmitters
- Lists may now be empty
