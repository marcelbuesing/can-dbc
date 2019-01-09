use clap::{Arg, App};
use can_dbc_codegen::can_reader;
use std::fs::File;
use std::io::prelude::*;
use std::cmp;

use nom;
use nom::verbose_errors;

fn main() {

    let matches = App::new("can-dbc-codegen")
                          .arg(Arg::with_name("INPUT")
                               .help("Sets the dbc input path file to use")
                               .required(true)
                               .index(1))
                          .get_matches();

    let input_path = matches.value_of("INPUT").expect("INPUT missing");

    let mut f = File::open(input_path).expect("Failed to open input file");
    let mut buffer = Vec::new();
    f.read_to_end(&mut buffer).expect("Failed to read file");
    match can_dbc::DBC::from_slice(&buffer) {
        Ok(dbc_content) => {
            let code = can_reader(&dbc_content).expect("Failed to generate rust code");
            println!("{}", code.to_string());
        },
        Err(e) => {
            match e {
                can_dbc::Error::NomError(nom::Err::Incomplete(needed)) => eprintln!("Error incomplete input, needed: {:?}", needed),
                can_dbc::Error::NomError(nom::Err::Error(ctx)) => {
                    match ctx {
                        verbose_errors::Context::Code(i, kind) => eprintln!("Error Kind: {:?}, Code: {:?}", kind, String::from_utf8(i.to_vec())),
                        verbose_errors::Context::List(l)=> eprintln!("Error List: {:?}", l),
                    }
                }
                can_dbc::Error::NomError(nom::Err::Failure(ctx)) => eprintln!("Failure {:?}", ctx),
                can_dbc::Error::Incomplete(dbc, remaining) => eprintln!("Not all data in buffer was read {:#?}, remaining unparsed (length: {}): {}\n...(truncated)", dbc, remaining.len(), String::from_utf8_lossy(&remaining[0..cmp::min(100, remaining.len())]).to_string())
            }
        }
    }
}