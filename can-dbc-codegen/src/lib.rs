#![feature(no_panic_pow)]
#![feature(test)]

extern crate test;

use codegen::{Const, Enum, Function, Struct, Scope, Impl};
use can_dbc::{DBC, ByteOrder, Message, MessageId, Signal, ValueDescription};
use std::fmt::Write;
use heck::{CamelCase, ShoutySnakeCase};


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

type Result<T> = std::result::Result<T, std::fmt::Error>;

fn to_enum_name(message_id: &MessageId, signal_name: &str) -> String {
     format!("{}{}", &signal_name.to_camel_case(), message_id.0)
}

pub fn signal_enum(val_desc: &ValueDescription) -> Option<Enum> {
    if let ValueDescription::Signal{ ref message_id, ref signal_name, ref value_descriptions } = val_desc {

        let mut sig_enum = Enum::new(&to_enum_name(message_id, signal_name));
        sig_enum.vis("pub");
        sig_enum.repr("u64");
        sig_enum.derive("Debug");
        for desc in value_descriptions {
            sig_enum.new_variant(&desc.b().to_camel_case());
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

        let new_fn = enum_impl.new_fn("from");
        new_fn.arg("val", codegen::Type::new("u64"));

        let mut matching = String::new();
        write!(&mut matching, "match val {{\n").unwrap();
        for value_description in value_descriptions {
            write!(&mut matching, "    {} => {}::{},\n", value_description.a(), enum_name, value_description.b().to_camel_case()).unwrap();
        }
        write!(&mut matching, "    value => {}::XValue(value),\n", enum_name).unwrap();
        write!(&mut matching, "}}").unwrap();

        new_fn.line(matching);
        new_fn.ret(codegen::Type::new("Self"));

        return Some(enum_impl);
    }
    None
}

pub fn signal_fn(dbc: &DBC, signal: &Signal, message_id: &MessageId) -> Result<Function> {
    let mut signal_fn = codegen::Function::new(&signal.name().to_lowercase());
    signal_fn.vis("pub");
    signal_fn.arg_ref_self();

    // Attempt to find a matching enum return type, default to `f64` otherwise
    let ret_enum_type = dbc.value_descriptions_for_signal(message_id, signal.name()).map(|_| codegen::Type::new(&to_enum_name(message_id, signal.name())));
    signal_fn.ret(ret_enum_type.clone().unwrap_or_else(|| codegen::Type::new("f64")));

    let default_signal_comment = format!("Read {} signal from can frame", signal.name());
    let signal_comment = dbc.signal_comment(message_id, signal.name()).unwrap_or(&default_signal_comment);
    signal_fn.doc(signal_comment);

    let read_byte_order = match signal.byte_order() {
        ByteOrder::LittleEndian => "let frame_payload: u64 = LE::read_u64(self.frame_payload);",
        ByteOrder::BigEndian => "let  frame_payload: u64 = BE::read_u64(self.frame_payload);",
    };
    signal_fn.line(read_byte_order);

    let bit_msk_const = 2u64.saturating_pow(*signal.signal_size() as u32) - 1;
    let mut calc = String::new();

    if ret_enum_type.is_some() {
       write!(&mut calc, "{}::from((", to_enum_name(message_id, signal.name()))?; // TODO to_valid_upper_case called multiple times
    }

    write!(&mut calc, "((frame_payload >> {}) & {:#X}) as f64", signal.start_bit(), bit_msk_const)?;

    if *signal.factor() != 1.0 {
        write!(&mut calc, " * {:.6}", signal.factor())?;
    }

    if *signal.offset() != 0.0 {
        write!(&mut calc, " + {} as f64", signal.offset())?;
    }

    if ret_enum_type.is_some() {
        write!(&mut calc, ") as u64)")?;
    }

    signal_fn.line(calc);
    Ok(signal_fn)
}

fn message_const(message: &Message) -> Const {
    let const_name = format!("MESSAGE_ID_{}", message.message_name().to_shouty_snake_case());
    let mut c = Const::new(&const_name, codegen::Type::new("u32"), message.message_id().0.to_string());
    c.vis("pub");
    c
}

fn message_struct(dbc: &DBC, message: &Message) -> Struct {
    let mut message_struct = Struct::new(&message.message_name().to_camel_case());
    if let Some(message_comment) = dbc.message_comment(message.message_id()) {
        message_struct.doc(message_comment);
    }
    message_struct.derive("Debug");
    message_struct.vis("pub");
    message_struct.generic("'a");
    message_struct.field("frame_payload", "&'a[u8]");
    message_struct
}

fn message_impl(dbc: &DBC, message: &Message) -> Result<Impl> {

    let mut msg_impl = Impl::new(codegen::Type::new(&message.message_name().to_camel_case()));
    msg_impl.generic("'a");
    msg_impl.target_generic("'a");
    let new_fn = msg_impl.new_fn("new");
    new_fn.vis("pub");
    new_fn.arg("frame_payload", codegen::Type::new("&[u8]"));

    new_fn.line(format!("{} {{ frame_payload }}", message.message_name().to_camel_case()));
    new_fn.ret(codegen::Type::new(&message.message_name().to_camel_case()));

    for signal in message.signals() {
        msg_impl.push_fn(signal_fn(dbc, signal, message.message_id())?);
    }

    Ok(msg_impl)
}

pub fn can_reader(dbc: &DBC) -> Result<Scope> {

    let mut scope = Scope::new();
    scope.raw("#[allow(dead_code, unused_imports)]\n");
    scope.import("byteorder", "{ByteOrder, LE, BE}");

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
        scope.push_impl(message_impl(&dbc, message)?);
    }

    Ok(scope)
}