#![feature(async_await, await_macro)]

//! If you are using Rust 2018 no `external crate byteorder;` is necessary
//! The `gen` module exists because `cargo test` fails
//! if `j1939.rs` is directly in the examples folder
//! (since it's treated like an example and a `main` is expected).
//! Usually you could directly use `mod j1939;` here.
//! T
//!
//! Hazard Lamps Flashing
//! ```
//! cansend vcan0 0CFDCCFE#11111111111111;
//! ```
//!
//! ```
//!cansend vcan0 0CFDCCFE#00000000000000;
//! ```
//! Hazard Lights To Be Off

mod gen;

/// Generated module
use crate::gen::j1939;

use futures_util::stream::StreamExt;
use std::io;
use std::time::Duration;


#[runtime::main]
async fn main() -> io::Result<()> {
    let ival = Duration::from_secs(0);
    let mut oel_stream = j1939::Oel::stream("vcan0", &ival, &ival)?;
    let oel = await!(oel_stream.next()).expect("No next value")?;

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
}
