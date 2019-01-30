#![feature(no_panic_pow)]
#![feature(test)]

extern crate test;
#[macro_use]
extern crate structopt;

use codegen::{Const, Enum, Function, Struct, Scope, Impl};
use can_dbc::{DBC, ByteOrder, Message, MessageId, Signal, ValueDescription};
use heck::{CamelCase, ShoutySnakeCase};
use log::{warn, info};

use std::fmt::Write;
use std::path::PathBuf;

#[cfg(test)]
mod tests {
    use test::{Bencher, black_box};
    use byteorder;
    use byteorder::{ByteOrder, LE};

    #[bench]
    fn bench_read_signal(b: &mut Bencher) {
        const byte_payload: &[u8] = &[0x4, 0x2, 0xA, 0xA, 0xF, 0xF, 0xE, 0xE, 0xD, 0xD, 0xA, 0xA, 0xF, 0xF, 0xD, 0xD];


        b.iter(|| {
            let frame_payload: u64 = LE::read_u64(byte_payload);
            let bit_msk_const = 2u64.saturating_pow(8 as u32) - 1;
            let factor: f64 = test::black_box(2.0);
            let start_bit: u64 = test::black_box(8);
            let offset: f64 = test::black_box(10.0);

            (((frame_payload >> start_bit) & bit_msk_const) as f64) * factor + offset
        });
    }
}

/// Character that is prefixed before type names that are
/// are not starting with an alphabetic char.
const PREFIX_CHAR: char = 'X';

/// Character that is used to replace invalid characters
/// in type names.
const REPLACEMENT_CHAR: char = 'X';

type Result<T> = std::result::Result<T, std::fmt::Error>;

#[derive(StructOpt, Debug)]
#[structopt(name = "dbcc", about = "DBC to rust code compiler")]
pub struct Opt {
    #[structopt(short = "i", long = "input", parse(from_os_str), help = "DBC file")]
    pub input: PathBuf,

    #[structopt(long = "with-tokio", help = "Generate Tokio streams.")]
    pub with_tokio: bool,
}

pub trait TypeName: ToOwned {
    fn to_type_name(&self) -> Self::Owned;
}

impl TypeName for str {
    fn to_type_name(&self) -> String {
        let mut out = String::with_capacity(self.len() + 1);
        let mut chars = self.chars();
        if let Some(first) = chars.next() {
            if !first.is_alphabetic() && first != '_' {
                warn!("string: {} is prefixed with `{}`", self, PREFIX_CHAR);
                out.push(PREFIX_CHAR);
            }
            out.push(first);
        }

        while let Some(chr) = chars.next() {
            if chr.is_digit(10) || chr.is_alphabetic() || chr == '_' {
                out.push(chr);
            } else {
                warn!("`{}` character in string: {} is replaced by `{}`", chr, self, REPLACEMENT_CHAR);
                out.push(REPLACEMENT_CHAR);
            }
        }

        out
    }
}

fn to_enum_name(message_id: &MessageId, signal_name: &str) -> String {
     format!("{}{}", &signal_name.to_camel_case(), message_id.0)
}

pub fn signal_enum(val_desc: &ValueDescription) -> Option<Enum> {
    if let ValueDescription::Signal{ ref message_id, ref signal_name, ref value_descriptions } = val_desc {

        let mut sig_enum = Enum::new(&to_enum_name(message_id, signal_name));
        sig_enum.allow("dead_code");
        sig_enum.vis("pub");
        sig_enum.repr("u64");
        sig_enum.derive("Debug");
        sig_enum.derive("Clone");
        sig_enum.derive("Copy");
        sig_enum.derive("PartialEq");
        sig_enum.derive("Eq");
        for desc in value_descriptions {
            sig_enum.new_variant(&desc.b().to_camel_case().to_type_name());
        }
        sig_enum.new_variant("XValue(u64)");
        return Some(sig_enum);
    }
    None
}

pub fn signal_enum_impl(val_desc: &ValueDescription) -> Option<Impl> {
    if let ValueDescription::Signal{ ref message_id, ref signal_name, ref value_descriptions } = val_desc {
        let enum_name = to_enum_name(message_id, signal_name);
        let mut enum_impl = Impl::new(codegen::Type::new(&enum_name));
        enum_impl.impl_trait("From<u64>");

        let from_fn = enum_impl.new_fn("from");
        from_fn.allow("dead_code");
        from_fn.arg("val", codegen::Type::new("u64"));

        let mut matching = String::new();
        write!(&mut matching, "match val {{\n").unwrap();
        for value_description in value_descriptions {
            write!(&mut matching, "    {} => {}::{},\n", value_description.a(), enum_name, value_description.b().to_camel_case().to_type_name()).unwrap();
        }
        write!(&mut matching, "    value => {}::XValue(value),\n", enum_name).unwrap();
        write!(&mut matching, "}}").unwrap();

        from_fn.line(matching);
        from_fn.ret(codegen::Type::new("Self"));

        return Some(enum_impl);
    }
    None
}

pub fn signal_fn(dbc: &DBC, signal: &Signal, message_id: &MessageId) -> Result<Function> {
    let mut signal_fn = codegen::Function::new(&signal.name().to_lowercase());
    signal_fn.allow("dead_code");
    signal_fn.vis("pub");
    signal_fn.arg_ref_self();

    // Attempt to find a matching enum return type, default to `f64` otherwise
    let ret_enum_type = dbc.value_descriptions_for_signal(message_id, signal.name()).map(|_| codegen::Type::new(&to_enum_name(message_id, signal.name())));
    signal_fn.ret(ret_enum_type.clone().unwrap_or_else(|| codegen::Type::new("f64")));

    let default_signal_comment = format!("Read {} signal from can frame", signal.name());
    let signal_comment = dbc.signal_comment(message_id, signal.name()).unwrap_or(&default_signal_comment);
    signal_fn.doc(signal_comment);

    let read_byte_order = match signal.byte_order() {
        ByteOrder::LittleEndian => "let frame_payload: u64 = LE::read_u64(&self.frame_payload);",
        ByteOrder::BigEndian => "let  frame_payload: u64 = BE::read_u64(&self.frame_payload);",
    };
    signal_fn.line(read_byte_order);

    let bit_msk_const = 2u64.saturating_pow(*signal.signal_size() as u32) - 1;
    let mut calc = String::new();

    if ret_enum_type.is_some() {
       write!(&mut calc, "{}::from((", to_enum_name(message_id, signal.name()))?; // TODO to_valid_upper_case called multiple times
    }

    let signal_shift = shift_amount(*signal.byte_order(), *signal.start_bit(), *signal.signal_size());

    // No shift required if start_bit == 0
    let shift = if signal_shift != 0 {
        format!("(frame_payload >> {})", signal_shift)
    } else {
        format!("frame_payload")
    };

    write!(&mut calc, "({} & {:#X}) as f64", shift, bit_msk_const)?;

    if *signal.factor() != 1.0 {
        write!(&mut calc, " * {:.6}", signal.factor())?;
    }

    if *signal.offset() != 0.0 {
        write!(&mut calc, " + {}f64", signal.offset())?;
    }

    if ret_enum_type.is_some() {
        write!(&mut calc, ") as u64)")?;
    }

    signal_fn.line(calc);
    Ok(signal_fn)
}

fn shift_amount(byte_order: ByteOrder, start_bit: u64, signal_size: u64) -> u64 {
    match byte_order {
        ByteOrder::LittleEndian => start_bit,
        ByteOrder::BigEndian => 64 - signal_size - ((start_bit / 8) * 8 + (7 - (start_bit % 8))),
    }
}

fn message_const(message: &Message) -> Const {
    let const_name = format!("MESSAGE_ID_{}", message.message_name().to_shouty_snake_case());
    let mut c = Const::new(&const_name, codegen::Type::new("u32"), message.message_id().0.to_string());
    c.allow("dead_code");
    c.vis("pub");
    c
}

fn message_struct(dbc: &DBC, message: &Message) -> Struct {
    let mut message_struct = Struct::new(&message.message_name().to_camel_case());
    if let Some(message_comment) = dbc.message_comment(message.message_id()) {
        message_struct.doc(message_comment);
    }
    message_struct.allow("dead_code");
    message_struct.derive("Debug");
    message_struct.vis("pub");
    message_struct.field("frame_payload", "Vec<u8>");
    message_struct
}

fn message_impl(opt: &Opt, dbc: &DBC, message: &Message) -> Result<Impl> {

    let mut msg_impl = Impl::new(codegen::Type::new(&message.message_name().to_camel_case()));

    let new_fn = msg_impl.new_fn("new");
    new_fn.allow("dead_code");
    new_fn.vis("pub");
    new_fn.arg("frame_payload", codegen::Type::new("Vec<u8>"));

    new_fn.line(format!("{} {{ frame_payload }}", message.message_name().to_camel_case()));
    new_fn.ret(codegen::Type::new(&message.message_name().to_camel_case()));

    if opt.with_tokio {
        msg_impl.push_fn(message_stream(message));
    }

    for signal in message.signals() {
        msg_impl.push_fn(signal_fn(dbc, signal, message.message_id())?);
    }

    Ok(msg_impl)
}

/// Generate message stream using socketcan's Broadcast Manager filters via socketcan-tokio.
fn message_stream(message: &Message) -> Function {
    let mut stream_fn = codegen::Function::new("stream");
    stream_fn.allow("dead_code");
    stream_fn.vis("pub");

    //stream_fn.generic("'b");
    stream_fn.arg("can_interface", codegen::Type::new("String"));
    stream_fn.arg("ival1", codegen::Type::new("std::time::Duration"));
    stream_fn.arg("ival2", codegen::Type::new("std::time::Duration"));

    let ret = format!("Box<Stream<Item = {}, Error = std::io::Error> + Sync + Send>", message.message_name().to_camel_case());
    stream_fn.ret(ret);

    stream_fn.line("let socket = CanBCMSocket::open_nb(&can_interface).unwrap();");
    stream_fn.line(format!("let message_id = CanMessageId::try_from({} as u32).unwrap();", message.message_id().0.to_string()));
    stream_fn.line("let frame_stream = socket.filter_id_incoming_frames(message_id, ival1, ival2).unwrap();");
    stream_fn.line(format!("let f = frame_stream.map(|frame| {}::new(frame.data().to_vec()));",  message.message_name().to_camel_case()));
    stream_fn.line("Box::new(f)");

    stream_fn
}

pub fn can_reader(opt: &Opt, dbc: &DBC) -> Result<Scope> {

    let mut scope = Scope::new();
    scope.import("byteorder", "{ByteOrder, LE, BE}");

    if opt.with_tokio {
        scope.import("socketcan", "CanMessageId");
        scope.import("socketcan_tokio::bcm", "CanBCMSocket");
        scope.import("futures", "Stream");
        scope.import("std::convert", "TryFrom");
    }

    for message in dbc.messages() {
        scope.push_const(message_const(message));
    }

    for value_description in dbc.value_descriptions() {
        if let Some(signal_enum) = signal_enum(value_description) {
            scope.push_enum(signal_enum);
        }

        if let Some(enum_impl) = signal_enum_impl(value_description) {
            scope.push_impl(enum_impl);
        }
    }

    for message in dbc.messages() {
        scope.push_struct(message_struct(&dbc, message));
        scope.push_impl(message_impl(opt, &dbc, message)?);
    }

    Ok(scope)
}