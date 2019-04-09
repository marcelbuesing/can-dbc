# dbcc [![Build Status](https://travis-ci.org/marcelbuesing/can-dbc.svg?branch=dev)](https://travis-ci.org/marcelbuesing/can-dbc) [![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Fmarcelbuesing%2Fcan-dbc.svg?type=shield)](https://app.fossa.com/projects/git%2Bgithub.com%2Fmarcelbuesing%2Fcan-dbc?ref=badge_shield)
=============

dbcc can translate `data base CAN` files into Rust code.
The generated code allows interacting with CAN signals in a type safe manner by e.g. matching against signal value enum types.
Furthermore it provides a convenient way to use [SocketCAN BCM Sockets](https://crates.io/crates/tokio-socketcan-bcm), via [tokio](https://crates.io/crates/tokio) streams, to filter for a specified message by can identifier.

## Features
- [x] Generate message, signal decoder code
- [x] Generate message id constants
- [x] Generate enums for matching against signal values
- [x] Generate tokio streams for CAN messages
- [ ] Generate message, signal encoders

## Option 1 - Run CLI

Install
```
cargo install dbcc
```

Generate code using the CLI.
```
dbcc --input dbcc j1939.dbc > j1939.rs
```

For warnings during the generation run with:

```
RUST_LOG=info dbcc j1939.dbc > j1939.rs
```

## Option 2 - build.rs

Generate code at build time. Add the following to your [build.rs](https://doc.rust-lang.org/cargo/reference/build-scripts.html).
Adapt the dbc input path and target path according to your needs.


```Rust
use dbcc::{DbccOpt, can_code_gen};
use can_dbc;

use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn main() -> std::io::Result<()> {
    let dbcs = &[
        ("./dbcs/j1939.dbc", "./src/lib.rs"),
    ];
    generate_code_for_dbc(dbcs)?;
    Ok(())
}

fn generate_code_for_dbc<P: AsRef<Path>>(input_output: &[(P, P)]) -> std::io::Result<()> {

    for (input_path, output_path) in input_output {
        let mut f = File::open(input_path).expect("Failed to open input file");
        let mut buffer = Vec::new();
        f.read_to_end(&mut buffer).expect("Failed to read file");

        let opt = DbccOpt {
            with_tokio: true,
        };

        let dbc_content = can_dbc::DBC::from_slice(&buffer).expect("Failed to read DBC file");
        let code = can_code_gen(&opt, &dbc_content).expect("Failed to generate rust code");

        let mut f = File::create(output_path)?;
        f.write_all(&code.to_string().into_bytes())?;
    }

    Ok(())
}
```

## Include
- Move the generated rust file to your project's `src/` folder.
- Add the following dependency to your project's `Cargo.toml`
```YAML
[dependencies]
byteorder = "1.2"
```

## Use
```Rust
/// If you are using Rust 2018 no `external crate byteorder;` is necessary
/// Generated module
mod j1939;

fn main() {
    // J1939 - Operators External Light Controls Message Id
    let can_message_id = 2365443326u32;
    // can frame data field (0-8 bytes)
    let can_frame_data: Vec<u8> = vec![0x00, 0x50, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];

    // CAN Message ID constant from generated code
    if can_message_id == j1939::MESSAGE_ID_OEL {
        // J1939 - Operators External Light Controls Message
        let oel = j1939::Oel::new(can_frame_data);

        // Signal indicate the selected position of the operator's hazard light switch.
        match oel.hazardlightswitch() {
            j1939::HazardLightSwitch2365443326::HazardLampsToBeFlashing => println!("Hazard Lamps To Be Flashing"),
            j1939::HazardLightSwitch2365443326::HazardLampsToBeOff => println!("Hazard Lamps To Be Off"),
            j1939::HazardLightSwitch2365443326::NotAvailable => println!("Not available"),
            j1939::HazardLightSwitch2365443326::Error => println!("Error"),
            j1939::HazardLightSwitch2365443326::XValue(_) => unreachable!(),
        }
    }
}
```

## Including SocketCAN Streams
- Make sure you pass the `--with-tokio` flag when invoking dbcc.
- Move the generated rust file to your project's `src/` folder.
- Add the following dependencies to your project's `Cargo.toml`
```YAML
[dependencies]
byteorder = "1.3"
futures = "0.1"
tokio = "0.1"
tokio-socketcan-bcm = "0.3"
```

```Rust
mod j1939;

use futures::future::Future;
use futures::stream::Stream;
use std::io;
use std::time::Duration;
use tokio;

fn main() -> io::Result<()> {
    let ival = Duration::from_secs(0);

    let f = j1939::Oel::stream("vcan0", &ival, &ival)?
        .for_each(|oel| {
            // Signal indicates the selected position of the operator's hazard light switch.
            match oel.hazardlightswitch() {
                j1939::HazardLightSwitch2365443326::HazardLampsToBeFlashing => {
                    println!("Hazard Lamps To Be Flashing")
                }
                j1939::HazardLightSwitch2365443326::HazardLampsToBeOff => {
                    println!("Hazard Lamps To Be Off")
                }
                j1939::HazardLightSwitch2365443326::NotAvailable => println!("Not available"),
                j1939::HazardLightSwitch2365443326::Error => println!("Error"),
                j1939::HazardLightSwitch2365443326::XValue(_) => unreachable!(),
            }
            Ok(())
        });

    tokio::run(f.map_err(|_| ()));

    Ok(())
}
```

## Naming
Recommendation: Value descriptions aka `VAL_ ...` should contain only
alphanumeric characters or underscores and should start with an alphabetic character.
E.g. `VAL_ 100 "111 Wunderschön Inc" 255` should be `VAL_ 100 " Wunderschoen Inc 111" 255`

- Enums: Generated names are prefixed with an `X` if the name does not start with an alphabetic character.
- Enums: Characters that are not alphanumeric or `_` are replaced with an `X`
- Enums: An `XValue(f64)` variant is added to each enum since value descriptions often do not cover all possibilities.
