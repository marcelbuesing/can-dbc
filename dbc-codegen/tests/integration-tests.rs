use dbc_codegen::message_reader;
use std::fs::File;
use std::io::prelude::*;

#[test]
fn generate_message_parser() {

    let mut f = File::open("../dbc-parser/examples/sample.dbc").expect("Failed to open file");
    let mut buffer = Vec::new();
    f.read_to_end(&mut buffer).expect("Failed to read file");
    let dbc = dbc_parser::DBC::from_slice(&buffer).unwrap();

    for message in dbc.messages() {
        let code = message_reader(message);
        println!("{}", code.to_string());
    }
    assert_eq!(2 + 2, 4);
}