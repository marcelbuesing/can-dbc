extern crate codegen;
extern crate dbc_parser;

use codegen::{Scope, Type};
use dbc_parser::Message;

#[cfg(test)]
mod tests {

    use crate::message_reader;
    use dbc_parser::message;

    #[test]
    fn it_works() {
        let message_lines = b"BO_ 1840 WebData_1840: 4 PC
    SG_ Signal_4 : 24|8@1+ (1,0) [0|255] \"\" Vector__XXX
    SG_ Signal_3 : 16|8@1+ (1,5) [0|255] \"\" Vector__XXX
    SG_ Signal_2 : 8|8@1+ (4,0) [0|255] \"\" Vector__XXX
    SG_ Signal_1 : 0|8@1+ (1,3) [0|0] \"\" Vector__XXX
    
    X";

        let (_, message1) = message(message_lines).unwrap();
        let code = message_reader(&message1);
        println!("{}", code.to_string());
        assert_eq!(2 + 2, 4);
    }
}

fn message_reader(message: &Message) -> Scope {
    let mut scope = Scope::new();

    //let scope_mod = scope.new_module();

    // Message struct
    let message_struct = scope.new_struct(message.message_name());
    for signal in message.signals() {
        message_struct.field(signal.name().to_lowercase().as_str(), "f64");
    }

    let message_struct_impl = scope.new_impl(message.message_name());

    let signal_fn = message_struct_impl.new_fn("read");
    signal_fn.ret(Type::new(message.message_name()));
    
    signal_fn.line(format!(
        "named!({}<&[u8], {}>,",
        message.message_name().to_lowercase(),
        message.message_name()
    ));

    signal_fn.line("    do_parse!(");

    for signal in message.signals() {
        signal_fn.line(format!(
            "        {}: take_bits!(u8, {}) >>",
            signal.name().to_lowercase(),
            signal.signal_size
        ));
    }

    signal_fn.line(format!("    ({} {{", message.message_name()));
    for signal in message.signals() {
        signal_fn.line(format!(
            "        {}: {} * {} + {},",
            signal.name().to_lowercase(),
            signal.name().to_lowercase(),
            signal.factor,
            signal.offset
        ));
    }
    signal_fn.line("   }))");

    signal_fn.line(");");
    scope
}
