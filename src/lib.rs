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
        let signal_line = " SG_ NAME : 3|2@1- (1,0) [0|0] \"x\" UFA\r\n\r\n";
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

    #[test]
    fn signal_comment_test() {
        let def1 = "CM_ SG_ 193 KLU_R_X \"This is a signal comment test\";";
        let id1 = SignalCommentId(193);
        let comment1 = DbcElement::SignalComment(
            id1,
            "KLU_R_X".to_string(),
            "This is a signal comment test".to_string(),
            false,
        );
        let (_, comment1_def) = comment(def1).expect("Failed to parse signal comment definition");
        assert_eq!(comment1, comment1_def);
    }

    #[test]
    fn message_definition_comment_test() {
        let def1 = "CM_ BO_ 34544 XYZ \"Some Message comment\";";
        let id1 = MessageId(34544);
        let comment1 = DbcElement::MessageDefinitionComment(
            id1,
            "XYZ".to_string(),
            "Some Message comment".to_string(),
            false,
        );
        let (_, comment1_def) =
            comment(def1).expect("Failed to parse message definition comment definition");
        assert_eq!(comment1, comment1_def);
    }

    #[test]
    fn value_description_for_signal_test() {
        let def1 = "VAL_ 837 UF_HZ_OI 255 \"NOP\" ;";
        let id = MessageId(837);
        let name = "UF_HZ_OI".to_string();
        let descriptions = vec![ValueDescription {
            a: 255.0,
            b: "NOP".to_string(),
        }];
        let value_description_for_signal1 =
            DbcElement::ValueDescriptionsForSignal(id, name, descriptions);
        let (_, value_signal_def) =
            value_descriptions(def1).expect("Failed to parse value desc for signal");
        assert_eq!(value_description_for_signal1, value_signal_def);
    }

    #[test]
    fn value_description_for_env_var_test() {
        let def1 = "VAL_ MY_ENV_VAR 255 \"NOP\" ;";
        let name = "MY_ENV_VAR".to_string();
        let descriptions = vec![ValueDescription {
            a: 255.0,
            b: "NOP".to_string(),
        }];
        let value_env_var1 = DbcElement::ValueDescriptionsForEnvVar(name, descriptions);
        let (_, value_env_var) =
            value_descriptions(def1).expect("Failed to parse value desc for env var");
        assert_eq!(value_env_var1, value_env_var);
    }

    #[test]
    fn environment_variable_test() {
        let def1 = "EV_ IUV: 0 [-22|20] \"mm\" 3 7 DUMMY_NODE_VECTOR0 VECTOR_XXX;";
        let nodes1 = vec![AccessNode::AccessNodeVectorXXX];
        let env_var1 = DbcElement::EnvVariable(
            "IUV".to_string(),
            EnvType::EnvTypeFloat,
            -22,
            20,
            "mm".to_string(),
            3.0,
            7,
            AccessType::DUMMY_NODE_VECTOR0,
            nodes1,
        );
        let (_, env_var) =
            environment_variable(def1).expect("Failed to parse environment variable");
        assert_eq!(env_var1, env_var);
    }

    #[test]
    fn network_node_attribute_value_test() {
        let def = "BA_ \"AttrName\" BU_ NodeName 12;\n";
        let node = AttributeValuedForObjectType::NetworkNodeAttributeValue("NodeName".to_string(), AttributeValue::AttributeValueU64(12));
        let attr_val_exp= DbcElement::AttributeValueForObject("AttrName".to_string(), node);
        let (_, attr_val) = attribute_value_for_object(def).unwrap();
        assert_eq!(attr_val_exp, attr_val);
    }

    #[test]
    fn message_definition_attribute_value_test() {
        let def = "BA_ \"AttrName\" BO_ 298 13;\n";
        let msg_def = AttributeValuedForObjectType::MessageDefinitionAttributeValue(MessageId(298), AttributeValue::AttributeValueU64(13));
        let attr_val_exp= DbcElement::AttributeValueForObject("AttrName".to_string(), msg_def);
        let (_, attr_val) = attribute_value_for_object(def).unwrap();
        assert_eq!(attr_val_exp, attr_val);
    }

    #[test]
    fn signal_attribute_value_test() {
        let def = "BA_ \"AttrName\" SG_ 198 SGName 13;\n";
        let msg_def = AttributeValuedForObjectType::SignalAttributeValue(MessageId(198), "SGName".to_string(), AttributeValue::AttributeValueU64(13));
        let attr_val_exp= DbcElement::AttributeValueForObject("AttrName".to_string(), msg_def);
        let (_, attr_val) = attribute_value_for_object(def).unwrap();
        assert_eq!(attr_val_exp, attr_val);
    }

    #[test]
    fn env_var_attribute_value_test() {
        let def = "BA_ \"AttrName\" EV_ EvName \"CharStr\";\n";
        let msg_def = AttributeValuedForObjectType::EnvVariableAttributeValue("EvName".to_string(), AttributeValue::AttributeValueCharString("CharStr".to_string()));
        let attr_val_exp= DbcElement::AttributeValueForObject("AttrName".to_string(), msg_def);
        let (_, attr_val) = attribute_value_for_object(def).unwrap();
        assert_eq!(attr_val_exp, attr_val);
    }

    #[test]
    fn raw_attribute_value_test() {
        let def = "BA_ \"AttrName\" \"RAW\";\n";
        let msg_def = AttributeValuedForObjectType::RawAttributeValue(AttributeValue::AttributeValueCharString("RAW".to_string()));
        let attr_val_exp= DbcElement::AttributeValueForObject("AttrName".to_string(), msg_def);
        let (_, attr_val) = attribute_value_for_object(def).unwrap();
        assert_eq!(attr_val_exp, attr_val);
    }

    #[test]
    fn new_symbols_test() {
        let def =
            "NS_ :
                NS_DESC_
                CM_
                BA_DEF_

            ";
        let symbols_exp = vec!(Symbol("NS_DESC_".to_string()), Symbol("CM_".to_string()), Symbol("BA_DEF_".to_string()));
        let (_, symbols) = new_symbols(def).unwrap();
        assert_eq!(symbols_exp, symbols);
    }

    #[test]
    fn network_node_test() {
        let def = "BU_: ZU XYZ ABC OIU\n";
        let nodes = vec!("ZU".to_string(), "XYZ".to_string(), "ABC".to_string(), "OIU".to_string());
        let (_, node) = network_node(def).unwrap();
        let node_exp = DbcElement::NetworkNode(nodes);
        assert_eq!(node_exp, node);
    }

    #[test]
    fn version_test() {
        let def = "VERSION \"HNPBNNNYNNNNNNNNNNNNNNNNNNNNNNNNYNYYYYYYYY>4>%%%/4>'%**4YYY///\"";
        let version_exp = Version("HNPBNNNYNNNNNNNNNNNNNNNNNNNNNNNNYNYYYYYYYY>4>%%%/4>'%**4YYY///".to_string());
        let (_, version) = version(def).unwrap();
        assert_eq!(version_exp, version);
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
pub struct SignalCommentId(u64);

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
    AttributeValueU64(u64),
    AttributeValueI64(i64),
    AttributeValueF64(f64),
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

fn is_semi_colon(chr: char) -> bool {
    chr == ';'
}

fn is_space_s(chr: char) -> bool {
    chr == ' '
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

/// Comma aka ';'
named!(semi_colon<&str,  char>, char!(';'));

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

named!(i64_digit<&str, i64>,
    flat_map!(recognize!(tuple!(opt!(alt!(char!('+') | char!('-'))), digit)), parse_to!(i64))
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

named!(pub signal_comment_id<&str, SignalCommentId>,
    do_parse!(
        id:  u64_digit >>
        (SignalCommentId(id))
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

named!(pub version<&str, Version>,
    do_parse!(
        tag!("VERSION") >>
        ss >>
        v: quoted >>
        (Version(v.to_string()))
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

named!(pub signal_comment<&str, DbcElement>,
    do_parse!(
        tag!("SG_") >>
        ss >>
        id: signal_comment_id >>
        ss >>
        name: c_ident >>
        ss >>
        comment: quoted >>
        (DbcElement::SignalComment(id, name.to_string(), comment.to_string(), false))
    )
);

named!(pub message_definition_comment<&str, DbcElement>,
    do_parse!(
        tag!("BO_") >>
        ss >>
        id: message_id >>
        ss >>
        // TODO not only c ident ?
        name: take_till_s!(is_space_s) >>
        ss >>
        comment: quoted >>
        (DbcElement::MessageDefinitionComment(id, name.to_string(), comment.to_string(), false))
    )
);

named!(pub comment<&str, DbcElement>,
    do_parse!(
        tag!("CM_") >>
        ss >>
        c: alt!(signal_comment | message_definition_comment) >>
        semi_colon >>
        (c)
    )
);

named!(pub value_description<&str, ValueDescription>,
    do_parse!(
        a: double_s >>
        ss >>
        b: quoted >>
        (ValueDescription { a: a, b: b.to_string() })
    )
);

named!(pub value_description_for_signal<&str, DbcElement>,
    do_parse!(
        tag!("VAL_") >>
        ss >>
        id: message_id >>
        ss >>
        name: c_ident >>
        descriptions:  many_till!(preceded!(ss, value_description), preceded!(ss, semi_colon)) >>
        (DbcElement::ValueDescriptionsForSignal(id, name.to_string(), descriptions.0))
    )
);

named!(pub value_description_for_env_var<&str, DbcElement>,
    do_parse!(
        tag!("VAL_") >>
        ss >>
        name: c_ident >>
        descriptions:  many_till!(preceded!(ss, value_description), preceded!(ss, semi_colon)) >>
        (DbcElement::ValueDescriptionsForEnvVar(name.to_string(), descriptions.0))
    )
);

named!(pub value_descriptions<&str, DbcElement>,
    alt!(value_description_for_signal | value_description_for_env_var)
);

named!(env_float<&str,  EnvType>, value!(EnvType::EnvTypeFloat, char!('0')));
named!(env_int<&str,  EnvType>, value!(EnvType::EnvTypeu64, char!('1')));
named!(env_data<&str,  EnvType>, value!(EnvType::EnvTypeData, char!('2')));

/// 9 Environment Variable Definitions
named!(pub env_var_type<&str, EnvType>, alt!(env_float | env_int | env_data));

named!(dummy_node_vector_0<&str,  AccessType>, value!(AccessType::DUMMY_NODE_VECTOR0, char!('0')));
named!(dummy_node_vector_1<&str,  AccessType>, value!(AccessType::DUMMY_NODE_VECTOR1, char!('1')));
named!(dummy_node_vector_2<&str,  AccessType>, value!(AccessType::DUMMY_NODE_VECTOR2, char!('2')));
named!(dummy_node_vector_3<&str,  AccessType>, value!(AccessType::DUMMY_NODE_VECTOR3, char!('3')));

/// 9 Environment Variable Definitions
named!(pub access_type<&str, AccessType>,
    do_parse!(
        tag!("DUMMY_NODE_VECTOR") >>
        node: alt!(dummy_node_vector_0 | dummy_node_vector_1 | dummy_node_vector_2 | dummy_node_vector_3) >>
        (node)
    )
);

named!(access_node_vector_xxx<&str, AccessNode>,  value!(AccessNode::AccessNodeVectorXXX, tag!("VECTOR_XXX")));
named!(access_node_name<&str, AccessNode>,  map!(c_ident, |name| AccessNode::AccessNodeName(name.to_string())));

/// 9 Environment Variable Definitions
named!(pub access_node<&str, AccessNode>, alt!(access_node_vector_xxx | access_node_name));

/// 9 Environment Variable Definitions
named!(environment_variable<&str, DbcElement>,
    do_parse!(
        tag!("EV_") >>
        ss >>
        name: c_ident >>
        colon >>
        ss >>
        type_: env_var_type >>
        ss >>
        brk_open >>
        min: i64_digit >>
        pipe >>
        max: i64_digit >>
        brk_close >>
        ss >>
        unit: quoted >>
        ss >>
        initial_value: double_s >>
        ss >>
        id: i64_digit >>
        ss >>
        access_type: access_type >>
        ss >>
        access_nodes: separated_nonempty_list!(comma, access_node) >>
        semi_colon >>
       (DbcElement::EnvVariable(name.to_string(), type_, min, max, unit.to_string(), initial_value, id, access_type, access_nodes))
    )
);

named!(pub attribute_value_uint64<&str, AttributeValue>, 
    map!(u64_digit, AttributeValue::AttributeValueU64)
);

named!(pub attribute_value_int64<&str, AttributeValue>, 
    map!(i64_digit, AttributeValue::AttributeValueI64)
);

named!(pub attribute_value_f64<&str, AttributeValue>, 
    map!(double_s, AttributeValue::AttributeValueF64)
);

named!(pub attribute_value_charstr<&str, AttributeValue>, 
    map!(quoted, |x| AttributeValue::AttributeValueCharString(x.to_string()))
);

named!(pub attribute_value<&str, AttributeValue>,
    alt!(
        attribute_value_uint64 |
        attribute_value_int64 |
        attribute_value_f64 |
        attribute_value_charstr
    )
);

named!(pub network_node_attribute_value<&str, AttributeValuedForObjectType>,
    do_parse!(
        tag!("BU_") >>
        ss >>
        node_name: c_ident >>
        ss >>
        value: attribute_value >>
        (AttributeValuedForObjectType::NetworkNodeAttributeValue(node_name.to_string(), value))
    )
);

named!(pub message_definition_attribute_value<&str, AttributeValuedForObjectType>,
    do_parse!(
        tag!("BO_") >>
        ss >>
        message_id: message_id >>
        ss >>
        value: attribute_value >>
        (AttributeValuedForObjectType::MessageDefinitionAttributeValue(message_id, value))
    )
);

named!(pub signal_attribute_value<&str, AttributeValuedForObjectType>,
    do_parse!(
        tag!("SG_") >>
        ss >>
        message_id: message_id >>
        ss >>
        signal_name: c_ident >>
        ss >>
        value: attribute_value >>
        (AttributeValuedForObjectType::SignalAttributeValue(message_id, signal_name.to_string(), value))
    )
);

named!(pub env_variable_attribute_value<&str, AttributeValuedForObjectType>,
    do_parse!(
        tag!("EV_") >>
        ss >>
        env_var_name: c_ident >>
        ss >>
        value: attribute_value >>
        (AttributeValuedForObjectType::EnvVariableAttributeValue(env_var_name.to_string(), value))
    )
);

named!(pub raw_attribute_value<&str, AttributeValuedForObjectType>,
    map!(attribute_value, AttributeValuedForObjectType::RawAttributeValue)
);

named!(pub attribute_value_for_object<&str, DbcElement>,
    do_parse!(
        tag!("BA_") >>
        ss >>
        name: quoted >>
        ss >>
        value: alt!(
                    network_node_attribute_value |
                    message_definition_attribute_value |
                    signal_attribute_value |
                    env_variable_attribute_value |
                    raw_attribute_value
                ) >>
        semi_colon >>
        (DbcElement::AttributeValueForObject(name.to_string(), value))
    )
);

named!(pub symbol<&str, Symbol>,
    do_parse!(
        space >>
        symbol: c_ident >>
        (Symbol(symbol.to_string()))
    )
);

named!(pub new_symbols<&str, Vec<Symbol>>,
    do_parse!(
        tag!("NS_ :") >>
        line_ending >>
        symbols: separated_nonempty_list!(line_ending, symbol) >>
        (symbols)
    )
);

named!(pub network_node<&str, DbcElement>,
    do_parse!(
        tag!("BU_:") >>
        ss >>
        li: map!(separated_nonempty_list!(ss, c_ident), |li| li.iter().map(|s| s.to_string()).collect())>>
        (DbcElement::NetworkNode(li))
    )
);
