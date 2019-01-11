# can-dbc-codegen

# Install
```
cargo install can-dbc-codegen
```

# Run
```
can-dbc-codegen j1939_utf8.dbc > j1939_utf8.rs
```

# Include
- Move the generated rust file to your projects src folder.
- Add the following dependency to your project
```
[dependencies]
byteorder = "1.2"
```

# Use
