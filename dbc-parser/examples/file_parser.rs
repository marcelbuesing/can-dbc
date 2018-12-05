
extern crate clap;

use clap::{Arg, App};
use dbc_parser;
use nom;
use nom::verbose_errors;

use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::cmp;

fn main() -> io::Result<()> {

    let matches = App::new("DBC Parser")
                          .version("1.0")
                          .arg(Arg::with_name("input")
                               .short("i")
                               .long("input")
                               .value_name("FILE")
                               .help("DBC file path")
                               .takes_value(true))
                          .get_matches();
    let path = matches.value_of("input").unwrap_or("./examples/sample.dbc");

    let mut f = File::open(path)?;
    let mut buffer = Vec::new();
    f.read_to_end(&mut buffer)?;

    match dbc_parser::DBC::from_slice(&buffer) {
        Ok(dbc_content) => println!("DBC Content{:#?}", dbc_content),
        Err(e) => {
            match e {
                dbc_parser::Error::NomError(nom::Err::Incomplete(needed)) => eprintln!("Error incomplete input, needed: {:?}", needed),
                dbc_parser::Error::NomError(nom::Err::Error(ctx)) => {
                    match ctx {
                        verbose_errors::Context::Code(i, kind) => eprintln!("Error Kind: {:?}, Code: {:?}", kind, String::from_utf8(i.to_vec())),
                        verbose_errors::Context::List(l)=> eprintln!("Error List: {:?}", l),
                    }
                }
                dbc_parser::Error::NomError(nom::Err::Failure(ctx)) => eprintln!("Failure {:?}", ctx),
                dbc_parser::Error::Incomplete(dbc, remaining) => eprintln!("Not all data in buffer was read {:#?}, remaining unparsed (length: {}): {}\n...(truncated)", dbc, remaining.len(), String::from_utf8_lossy(&remaining[0..cmp::min(100, remaining.len())]).to_string())
            }
        }
    }

    Ok(())
}