# Change Log

## [5.0.0](https://github.com/marcelbuesing/can-dbc/tree/5.0.0) (2022-06-22)
- Add support for dbcs with [extended multiplex messages](https://cdn.vector.com/cms/content/know-how/_application-notes/AN-ION-1-0521_Extended_Signal_Multiplexing.pdf). Thank you @pbert519!
- Breaking: `fn message_multiplexor_switch` now returns `Result<Option<&Signal>, Error>` instead of `Option<&Signal>`.
          This is necessary due to the possibility of a message having multiple multiplexor switches.

## [4.0.0](https://github.com/marcelbuesing/can-dbc/tree/4.0.0) (2021-11-15)
- Migrate from nom 4.2 to nom 7.1.
- Allow "random" order of elements in DBC. They still have to be in a block e.g. all messages or comments.
- Remove verbose error in returned error
- Return `&str` of remaining unparsed content instead of `Vec<u8>`.
- Mark `DBC::from_str` as deprecated and replace it with TryFrom::from_str

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
