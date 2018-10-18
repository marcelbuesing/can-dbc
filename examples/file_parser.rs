
extern crate clap;

use clap::{Arg, App};
use dbc_parser;
use nom;
use nom::verbose_errors;

use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::str;

fn main() -> io::Result<()> {

    let matches = App::new("My Super Program")
                          .version("1.0")
                          .author("Kevin K. <kbknapp@gmail.com>")
                          .about("Does awesome things")
                          .arg(Arg::with_name("input")
                               .short("i")
                               .long("input")
                               .value_name("FILE")
                               .help("DBC file path")
                               .takes_value(true))
                          .get_matches();
    let path = matches.value_of("input").unwrap();

    let mut f = File::open(path)?;
    let mut buffer = Vec::new();
    f.read_to_end(&mut buffer)?;

    match dbc_parser::dbc_definition(&buffer) {
        Ok((remaining, dbc_content)) => {
            println!("Remaining {:?}", remaining);
            println!("DBC Content{:?}", dbc_content);
        }, 
        Err(e) => {
            match e {
                nom::Err::Incomplete(needed) => eprintln!("Error incomplete input, needed: {:?}", needed),
                nom::Err::Error(ctx) => {
                    match ctx {
                        verbose_errors::Context::Code(i, kind) => eprintln!("Error Kind: {:?}, Code: {:?}", kind, str::from_utf8(i)),
                        verbose_errors::Context::List(l)=> eprintln!("Error List: {:?}", l),
                    }
                }
                nom::Err::Failure(ctx) => eprintln!("Failure {:?}", ctx),
            }
        }
    }

    Ok(())
}