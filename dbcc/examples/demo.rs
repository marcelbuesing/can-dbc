/// If you are using Rust 2018 no `external crate byteorder;` is necessary
// The `gen` module exists because `cargo test` fails
// if `j1939.rs` is directly in the examples folder
// (since it's treated like an example and a `main` is expected).
// Usually you could directly use `mod j1939;` here.
mod gen;

/// Generated module
use crate::gen::j1939;

fn main() {
    // J1939 - Operators External Light Controls Message Id
    let can_message_id = 2365443326u32;
    // can frame data field (0-8 bytes)
    let can_frame_data: Vec<u8> = vec![0x00, 0x50, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];

    // CAN Message ID constant from generated code
    if can_message_id == j1939::MESSAGE_ID_OEL {
        // J1939 - Operators External Light Controls Message
        let oel = j1939::Oel::new(can_frame_data);

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
    }
}
