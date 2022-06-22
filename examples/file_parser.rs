extern crate clap;

use can_dbc::{self};
use clap::{App, Arg};

use std::convert::TryFrom;
use std::fs::File;
use std::io;
use std::io::prelude::*;

fn main() -> io::Result<()> {
    let matches = App::new("DBC Parser")
        .version("1.0")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("FILE")
                .help("DBC file path")
                .takes_value(true),
        )
        .get_matches();
    let path = matches.value_of("input").unwrap_or("./examples/sample.dbc");

    let mut f = File::open(path)?;
    let mut buffer = Vec::new();
    f.read_to_end(&mut buffer)?;
    let dbc_in = std::str::from_utf8(&buffer).unwrap();

    match can_dbc::DBC::try_from(dbc_in) {
        Ok(dbc_content) => println!("DBC Content{:#?}", dbc_content),
        Err(e) => {
            match e {
                can_dbc::Error::Nom(nom::Err::Error(e)) => eprintln!("{:?}", e),
                can_dbc::Error::Nom(nom::Err::Failure(e)) => eprintln!("{:?}", e),
                can_dbc::Error::Nom(nom::Err::Incomplete(needed)) => eprintln!("Nom incomplete needed: {:#?}", needed),
                can_dbc::Error::Incomplete(dbc, remaining) => eprintln!("Not all data in buffer was read {:#?}, remaining unparsed (length: {}): {}\n...(truncated)", dbc, remaining.len(), remaining),
                can_dbc::Error::MultipleMultiplexors => eprintln!("Multiple multiplexors defined"),
            }
        }
    }

    Ok(())
}
