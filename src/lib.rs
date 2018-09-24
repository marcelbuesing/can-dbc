use nom::*;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn c_ident_test() {
        let cid = "EALL_DUSasb18 ";
        let (_, cid1) = c_ident(cid).unwrap();

        assert_eq!("EALL_DUSasb18", cid1);
    }

    #[test]
    fn c_ident_vec_test() {
        let cid = "FZHL_DUSasb18 ";
        let (_, cid1) = c_ident_vec(cid).unwrap();

        assert_eq!(vec!("FZHL_DUSasb18".to_string()), cid1);

        let cid_vec = "FZHL_DUSasb19,xkask_3298 ";
        let (_, cid2) = c_ident_vec(cid_vec).unwrap();

        assert_eq!(
            vec!("FZHL_DUSasb19".to_string(), "xkask_3298".to_string()),
            cid2
        );
    }

    #[test]
    fn signal_test() {
        let signal_line = " SG_ NAME : 6|2@1- (1,0) [0|0] \"x\" ECALL_RUS\r\n\r\n";
        let signal1 = signal(signal_line).unwrap();
    }
    #[test]
    fn endianess_test() {
        let (_, big_endian) = endianess("0").expect("Failed to parse big endian");
        assert_eq!(Endianess::BigEndian, big_endian);

        let (_, little_endian) = endianess("1").expect("Failed to parse little endian");
        assert_eq!(Endianess::LittleEndian, little_endian);
    }

    #[test]
    fn signal_type_test() {
        let (_, multiplexer) = signal_type("m34920 ").expect("Failed to parse multiplexer");
        assert_eq!(SignalType::MultiplexedSignal(34920), multiplexer);

        let (_, multiplexor) = signal_type("M ").expect("Failed to parse multiplexor");
        assert_eq!(SignalType::Multiplexor, multiplexor);

        let (_, plain) = signal_type(" ").expect("Failed to parse plain");
        assert_eq!(SignalType::Plain, plain);
    }

    #[test]
    fn value_type_test() {
        let (_, vt) = value_type("- ").expect("Failed to parse value type");
        assert_eq!(ValueType::Signed, vt);

        let (_, vt) = value_type("+ ").expect("Failed to parse value type");
        assert_eq!(ValueType::Unsigned, vt);
    }

    #[test]
    fn message_definition_test() {
        let def = "BO_ 1 MCA_A1: 6 MFA
        SG_ ABC_1 : 9|2@1+ (1,0) [0|0] \"x\" XYZ_OUS
        SG_ BasL2 : 3|2@0- (1,0) [0|0] \"x\" DFA_FUS\r\n\r\n";
        let (_, message_def) = message_definition(def).expect("Failed to parse message definition");
    }
}

#[derive(Debug, PartialEq)]
pub struct Dbc(String);

#[derive(Debug, PartialEq)]
pub struct Label(String);

#[derive(Debug, PartialEq)]
pub struct Signal {
    name: String,
    signal_type: SignalType,
    offset: u64,
    length: u64,
    endianess: Endianess,
    value_type: ValueType,
    slope: f64,
    intercept: f64,
    min: f64,
    max: f64,
    unit: String,
    receivers: Vec<String>,
}

#[derive(Debug, PartialEq)]
pub struct SignalCommentId(i64);

#[derive(Debug, PartialEq)]
pub struct MessageId(u64);

#[derive(Debug, PartialEq)]
pub struct MessageTransmitter(String);

#[derive(Debug, PartialEq)]
pub struct Version(String);

#[derive(Debug, PartialEq)]
pub struct Symbol(String);

#[derive(Debug, PartialEq)]
pub enum SignalType {
    /// Multiplexor switch
    Multiplexor,
    /// Signal us being multiplexed by the multiplexer switch.
    MultiplexedSignal(u64),
    /// Normal signal
    Plain,
}

#[derive(Debug, PartialEq)]
pub enum Endianess {
    LittleEndian,
    BigEndian,
}

#[derive(Debug, PartialEq)]
pub enum ValueType {
    Signed,
    Unsigned,
}

#[derive(Debug, PartialEq)]
pub enum EnvType {
    EnvTypeFloat,
    EnvTypeu64,
    EnvTypeData,
}

#[derive(Debug, PartialEq)]
pub struct ECUDef(String);

type KeyValue = (String, i64);

#[derive(Debug, PartialEq)]
pub struct LabelDescription {
    id: u64,
    signalName: String,
    labels: Vec<Label>,
    extended: bool,
}

#[derive(Debug, PartialEq)]
pub enum Transmitter {
    TransmitterNodeName(String),
    TransmitterNoSender,
}

#[derive(Debug, PartialEq)]
pub enum AccessType {
    DUMMY_NODE_VECTOR0,
    DUMMY_NODE_VECTOR1,
    DUMMY_NODE_VECTOR2,
    DUMMY_NODE_VECTOR3,
}

#[derive(Debug, PartialEq)]
pub enum AccessNode {
    AccessNodeVectorXXX,
    AccessNodeName(String),
}

#[derive(Debug, PartialEq)]
pub enum SignalAttributeValue {
    Text(String),
    Int(i64),
}

#[derive(Debug, PartialEq)]
pub enum AttributeValuedForObjectType {
    RawAttributeValue(AttributeValue),
    NetworkNodeAttributeValue(String, AttributeValue),
    MessageDefinitionAttributeValue(MessageId, AttributeValue),
    SignalAttributeValue(MessageId, String, AttributeValue),
    EnvVariableAttributeValue(String, AttributeValue),
}

#[derive(Debug, PartialEq)]
pub enum AttributeValueType {
    AttributeValueTypeInt(i64, i64),
    AttributeValueTypeHex(i64, i64),
    AttributeValueTypeFloat(f64, f64),
    AttributeValueTypeString,
    AttributeValueTypeEnum(Vec<String>),
}

#[derive(Debug, PartialEq)]
pub struct ValueDescription {
    a: f64,
    b: String,
}

#[derive(Debug, PartialEq)]
pub struct AttributeDefault {
    name: String,
    value: AttributeValue,
}

#[derive(Debug, PartialEq)]
pub enum AttributeValue {
    AttributeValueUu64(u64),
    AttributeValueu64(u64),
    AttributeValuef64(f64),
    AttributeValueCharString(String),
}

#[derive(Debug, PartialEq)]
pub enum DbcElement {
    ValueTable(String, Vec<KeyValue>),
    NetworkNodeComment(String),
    MessageDefinitionComment(MessageId, String, String, bool),
    SignalComment(SignalCommentId, String, String, bool),
    EnvVarComment(String),
    Message(MessageId, bool, String, u64, String, Vec<Signal>),
    EnvVariable(
        String,
        EnvType,
        i64,
        i64,
        String,
        f64,
        i64,
        AccessType,
        Vec<AccessNode>,
    ),
    BitTimingSection,
    EnvVarData(String, u64),
    NetworkNode(Vec<String>),
    AttributeValueForObject(String, AttributeValuedForObjectType),
    Def,
    BODef,
    BUDef,
    ValueDescriptionsForSignal(MessageId, String, Vec<ValueDescription>),
    ValueDescriptionsForEnvVar(String, Vec<ValueDescription>),
}

#[derive(Debug, PartialEq)]
pub struct DbcDefinition {
    version: Version,
    symbols: Vec<Symbol>,
    elements: Vec<DbcElement>,
}

fn is_colon(chr: char) -> bool {
    chr == ':'
}

fn is_c_string_char(chr: char) -> bool {
    chr.is_digit(10) || chr.is_alphabetic() || chr == '_'
}

fn is_quote(chr: char) -> bool {
    chr == '"'
}

/// Single space
named!(ss<&str, char>, char!(' '));

/// Colon
named!(colon<&str,  char>, char!(':'));

/// Comma aka ','
named!(comma<&str,  char>, char!(','));

/// Quote aka '"'
named!(quote<&str,  char>, char!('"'));

named!(pipe<&str,  char>, char!('|'));

named!(at<&str,  char>, char!('@'));

/// brace open aka '('
named!(brc_open<&str,  char>, char!('('));

/// brace close aka '('
named!(brc_close<&str,  char>, char!(')'));

/// bracket open aka '['
named!(brk_open<&str,  char>, char!('['));

/// bracket close aka ']'
named!(brk_close<&str,  char>, char!(']'));

// TODO fix first character
/// C_String
/// a valid C_identifier. C_identifiers have to start with a  alphacharacter or an underscore
/// and may further consist of alpha­numeric, characters and underscore
named!(c_ident<&str, &str>, take_while!(is_c_string_char));

named!(c_ident_vec<&str, Vec<String>>,
    map!(separated_nonempty_list!(comma, c_ident), |li| li.iter().map(|s| s.to_string()).collect())
);

named!(u64_digit<&str, u64>,
    map_res!(
        digit,
        std::str::FromStr::from_str
    )
);

named!(quoted<&str, &str>,
    do_parse!(
            quote                     >>
        s: take_till_s!(is_quote) >>
            quote                     >>
        (s)
    )
);

named!(little_endian<&str, Endianess>,
    do_parse!(
        char!('1') >>
        (Endianess::LittleEndian)
    )
);

named!(big_endian<&str, Endianess>,
    do_parse!(
        char!('0') >>
        (Endianess::BigEndian)
    )
);

named!(endianess<&str, Endianess>, alt!(little_endian | big_endian));

named!(pub message_id<&str, MessageId>,
    do_parse!(
        id:  u64_digit >>
        (MessageId(id))
    )
);

named!(signed<&str, ValueType>,
    do_parse!(
        char!('-') >>
        (ValueType::Signed)
    )
);

named!(unsigned<&str, ValueType>,
    do_parse!(
        char!('+') >>
        (ValueType::Unsigned)
    )
);

named!(value_type<&str, ValueType>, alt!(signed | unsigned));

named!(pub multiplexer<&str, SignalType>,
    do_parse!(
            char!('m') >>
        d: u64_digit >>
            ss >>
        (SignalType::MultiplexedSignal(d))
    )
);

named!(pub multiplexor<&str, SignalType>,
    do_parse!(
        char!('M') >>
        ss >>
        (SignalType::Multiplexor)
    )
);

named!(pub plain<&str, SignalType>,
    do_parse!(
        ss >>
        (SignalType::Plain)
    )
);

named!(pub signal_type<&str, SignalType>, alt!(multiplexer | multiplexor | plain));

named!(pub signal<&str, Signal>,
    do_parse!(
                             space >>
                              tag!("SG_") >>
                              ss                >>
        name:            c_ident        >>
        signal_type: signal_type >>
                              colon           >>
                              ss                >>
       offset:             u64_digit         >>
                              pipe             >>
       length:            u64_digit         >>
                              at                  >>
      endianess:    endianess    >>
       value_type:  value_type    >>
                             ss                 >>
                             brc_open      >>
       slope:            double_s             >>
                            comma          >>
       intercept:       double_s        >>
                             brc_close     >>
                             ss                 >>
                             brk_open      >>
       min:               double_s            >>
                             pipe               >>
       max:              double_s            >>
                             brk_close      >>
                             ss                  >>
       unit:               quoted           >>
                             ss                  >>
        receivers: c_ident_vec     >>
        (Signal {
            name: name.to_string(),
            signal_type: signal_type,
            offset: offset,
            length: length,
            endianess: endianess,
            value_type: value_type,
            slope: slope,
            intercept: intercept,
            min: min,
            max: max,
            unit:  unit.to_string(),
            receivers: receivers,
        })
    )
);

named!(pub message_definition<&str, DbcElement>,
  do_parse!(
    tag!("BO_") >>
    ss >>
    id:   message_id >>
    ss >>
    name: take_till_s!(is_colon) >>
    colon >>
    ss >>
    size:  u64_digit >>
    ss >>
    transmitter: c_ident >>
    line_ending >>
    signals: separated_nonempty_list!(line_ending, signal) >>
    (DbcElement::Message(id, false, name.to_string(), size, transmitter.to_string(), signals))
  )
);
