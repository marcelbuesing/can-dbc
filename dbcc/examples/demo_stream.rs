/// If you are using Rust 2018 no `external crate byteorder;` is necessary
// The `gen` module exists because `cargo test` fails
// if `j1939.rs` is directly in the examples folder
// (since it's treated like an example and a `main` is expected).
// Usually you could directly use `mod j1939;` here.
mod gen;

/// Generated module
use crate::gen::j1939;

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
}
