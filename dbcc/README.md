# dbcc
dbcc can translate `data base CAN` files into Rust code.

## Features
- [x] Generate message, signal decoder code
- [x] Geneate message id constants
- [x] Generate enums for matching against signal values
- [ ] Generate message, signal encoders

## Install
```
cargo install dbcc
```

## Run
```
dbcc --input j1939.dbc > j1939.rs
```

For warnings during the generation run with:

```
RUST_LOG=info j1939.dbc > j1939.rs
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

## Include SocketCan
- Move the generated rust file to your project's `src/` folder.
- Add the following dependency to your project's `Cargo.toml`
```YAML
[dependencies]
byteorder = "1.3"
futures = "0.1"
tokio = "0.1"
tokio-socketcan-bcm = { version = "0.2", features = ["try_from"] }
```

```
mod j1939;

use futures::future::Future;
use futures::stream::Stream;
use std::time::Duration;
use tokio;

fn main() {
    let ival = Duration::from_secs(0);
    let oel_stream = j1939::Oel::stream("vcan0", &ival, &ival);

    let f = oel_stream.for_each(|oel| {
        // Signal indicates the selected position of the operator's hazard light switch.
        match oel.hazardlightswitch() {
            j1939::HazardLightSwitch2365443326::HazardLampsToBeFlashing => println!("Hazard Lamps To Be Flashing"),
            j1939::HazardLightSwitch2365443326::HazardLampsToBeOff => println!("Hazard Lamps To Be Off"),
            j1939::HazardLightSwitch2365443326::NotAvailable => println!("Not available"),
            j1939::HazardLightSwitch2365443326::Error => println!("Error"),
            j1939::HazardLightSwitch2365443326::XValue(_) => unreachable!(),
        }
        Ok(())
    });

    tokio::run(f.map_err(|_| ()));
}
```

## Naming
Recommendation: Value descriptions aka `VAL_ ...` should contain only
alphanumeric characters or underscores and should start with an alphabetic character.
E.g. `VAL_ 100 "111 Wunderschön Inc" 255` should be `VAL_ 100 " Wunderschoen Inc 111" 255`

- Enums: Generated names are prefixed with an `X` if the name does not start with an alphabetic character.
- Enums: Characters that are not alphanumeric or `_` are replaced with an `X`
- Enums: An `XValue(f64)` variant is added to each enum since value descriptions often do not cover all possibilities.