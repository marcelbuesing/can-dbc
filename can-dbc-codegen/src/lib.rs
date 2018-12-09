extern crate codegen;
extern crate can_dbc;
extern crate nom;

use codegen::Scope;
use can_dbc::Message;

pub fn message_reader(message: &Message) -> Scope {
    let mut scope = Scope::new();

    // Message struct
    let message_struct = scope.new_struct(message.message_name());
    for signal in message.signals() {
        message_struct.field(signal.name().to_lowercase().as_str(), "f64");
    }

    let mut message_parser = String::new();

    message_parser.push_str(format!(
        "named!({}<&[u8], {}>,\n",
        message.message_name().to_lowercase(),
        message.message_name()
    ).as_str());

    message_parser.push_str("    do_parse!(\n");
    message_parser.push_str("        bits!(\n");

    for signal in message.signals() {
        message_parser.push_str(format!(
            "            {}: take_bits!(u8, {}) >>\n",
            signal.name().to_lowercase(),
            signal.signal_size
        ).as_str());
    }

    message_parser.push_str(format!("        ({} {{\n", message.message_name()).as_str());
    for signal in message.signals() {
        let add_offset = if signal.offset == 0.0 {
            "".to_string()
        } else {
            format!(" + {}", signal.offset)
        };

        let factor = if signal.factor == 1.0 {
            "".to_string()
        } else {
            format!(" * {}", signal.factor)
        };

        message_parser.push_str(format!(
            "            {}: {}{}{},\n",
            signal.name().to_lowercase(),
            signal.name().to_lowercase(),
            factor,
            add_offset
        ).as_str());
    }
    message_parser.push_str("        })\n");
    message_parser.push_str("    ))\n");
    message_parser.push_str(");\n");

    scope.raw(message_parser.as_str());
    scope
}
