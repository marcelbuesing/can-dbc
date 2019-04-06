use dbcc::{can_code_gen, DbccOpt};
use nom;
use nom::verbose_errors;

use pretty_env_logger;
use structopt::StructOpt;

use std::cmp;
use std::fs::File;
use std::io::prelude::*;
use std::path::PathBuf;

#[derive(StructOpt, Debug)]
#[structopt(name = "dbcc", about = "DBC to rust code compiler")]
pub struct Opt {
    /// File input
    #[structopt(short = "i", long = "input", parse(from_os_str), help = "DBC file")]
    pub input: PathBuf,

    /// Should tokio SocketCan BCM streams be generated.
    /// This requires the `tokio-socketcan-bcm` crate.
    #[structopt(long = "with-tokio", help = "Generate Tokio streams.")]
    pub with_tokio: bool,
}

fn main() {
    pretty_env_logger::init();
    let opt = Opt::from_args();

    let mut f = File::open(opt.input.clone()).expect("Failed to open input file");
    let mut buffer = Vec::new();
    f.read_to_end(&mut buffer).expect("Failed to read file");
    match can_dbc::DBC::from_slice(&buffer) {
        Ok(dbc_content) => {
            let opt = DbccOpt { with_tokio: opt.with_tokio };
            let code = can_code_gen(&opt, &dbc_content).expect("Failed to generate rust code");
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
