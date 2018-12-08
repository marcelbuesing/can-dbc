use nom::*;
use nom::types::CompleteByteSlice;
use std::str;
use nom_trace::*;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn c_ident_test() {
        let def1 = CompleteByteSlice(b"EALL_DUSasb18 ");
        let (_, cid1) = c_ident(def1).unwrap();
        assert_eq!("EALL_DUSasb18", cid1);

        let def2 = CompleteByteSlice(b"_EALL_DUSasb18 ");
        let (_, cid2) = c_ident(def2).unwrap();
        assert_eq!("_EALL_DUSasb18", cid2);

        // identifiers must not start with digits
        let def3 = CompleteByteSlice(b"3EALL_DUSasb18 ");
        let cid3_result = c_ident(def3);
        assert!(cid3_result.is_err());
    }

    #[test]
    fn c_ident_vec_test() {
        let cid = CompleteByteSlice(b"FZHL_DUSasb18 ");
        let (_, cid1) = c_ident_vec(cid).unwrap();

        assert_eq!(vec!("FZHL_DUSasb18".to_string()), cid1);

        let cid_vec = CompleteByteSlice(b"FZHL_DUSasb19,xkask_3298 ");
        let (_, cid2) = c_ident_vec(cid_vec).unwrap();

        assert_eq!(
            vec!("FZHL_DUSasb19".to_string(), "xkask_3298".to_string()),
            cid2
        );
    }

    #[test]
    fn signal_test() {
        let signal_line = CompleteByteSlice(b"SG_ NAME : 3|2@1- (1,0) [0|0] \"x\" UFA\r\n");
        let _signal = signal(signal_line).unwrap();
    }

    #[test]
    fn byte_order_test() {
        let (_, big_endian) = byte_order(CompleteByteSlice(b"0")).expect("Failed to parse big endian");
        assert_eq!(ByteOrder::BigEndian, big_endian);

        let (_, little_endian) = byte_order(CompleteByteSlice(b"1")).expect("Failed to parse little endian");
        assert_eq!(ByteOrder::LittleEndian, little_endian);
    }

    #[test]
    fn multiplexer_indicator_test() {
        let (_, multiplexer) = multiplexer_indicator(CompleteByteSlice(b" m34920 ")).expect("Failed to parse multiplexer");
        assert_eq!(MultiplexIndicator::MultiplexedSignal(34920), multiplexer);

        let (_, multiplexor) = multiplexer_indicator(CompleteByteSlice(b" M ")).expect("Failed to parse multiplexor");
        assert_eq!(MultiplexIndicator::Multiplexor, multiplexor);

        let (_, plain) = multiplexer_indicator(CompleteByteSlice(b" ")).expect("Failed to parse plain");
        assert_eq!(MultiplexIndicator::Plain, plain);
    }

    #[test]
    fn value_type_test() {
        let (_, vt) = value_type(CompleteByteSlice(b"- ")).expect("Failed to parse value type");
        assert_eq!(ValueType::Signed, vt);

        let (_, vt) = value_type(CompleteByteSlice(b"+ ")).expect("Failed to parse value type");
        assert_eq!(ValueType::Unsigned, vt);
    }

    #[test]
    fn message_definition_test() {
        let def = CompleteByteSlice(b"BO_ 1 MCA_A1: 6 MFA\r\nSG_ ABC_1 : 9|2@1+ (1,0) [0|0] \"x\" XYZ_OUS\r\nSG_ BasL2 : 3|2@0- (1,0) [0|0] \"x\" DFA_FUS\r\n x");
        signal(CompleteByteSlice(b"\r\n\r\nSG_ BasL2 : 3|2@0- (1,0) [0|0] \"x\" DFA_FUS\r\n")).expect("Faield");
        let (_, _message_def) = message(def).expect("Failed to parse message definition");
    }

    #[test]
    fn signal_comment_test() {
        let def1 = CompleteByteSlice(b"CM_ SG_ 193 KLU_R_X \"This is a signal comment test\";\n");
        let id1 = SignalCommentId(193);
        let comment1 = Comment::Signal(
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
        let def1 = CompleteByteSlice(b"CM_ BO_ 34544 XYZ \"Some Message comment\";\n");
        let id1 = MessageId(34544);
        let comment1 = Comment::MessageDefinition(
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
        let def1 = CompleteByteSlice(b"VAL_ 837 UF_HZ_OI 255 \"NOP\" ;\n");
        let id = MessageId(837);
        let name = "UF_HZ_OI".to_string();
        let descriptions = vec![ValDescription {
            a: 255.0,
            b: "NOP".to_string(),
        }];
        let value_description_for_signal1 =
            ValueDescription::Signal(id, name, descriptions);
        let (_, value_signal_def) =
            value_descriptions(def1).expect("Failed to parse value desc for signal");
        assert_eq!(value_description_for_signal1, value_signal_def);
    }

    #[test]
    fn value_description_for_env_var_test() {
        let def1 = CompleteByteSlice(b"VAL_ MY_ENV_VAR 255 \"NOP\" ;\n");
        let name = "MY_ENV_VAR".to_string();
        let descriptions = vec![ValDescription {
            a: 255.0,
            b: "NOP".to_string(),
        }];
        let value_env_var1 = ValueDescription::EnvironmentVariable(name, descriptions);
        let (_, value_env_var) =
            value_descriptions(def1).expect("Failed to parse value desc for env var");
        assert_eq!(value_env_var1, value_env_var);
    }

    #[test]
    fn environment_variable_test() {
        let def1 = CompleteByteSlice(b"EV_ IUV: 0 [-22|20] \"mm\" 3 7 DUMMY_NODE_VECTOR0 VECTOR_XXX;\n");
        let nodes1 = vec![AccessNode::AccessNodeVectorXXX];
        let env_var1 = EnvironmentVariable(
            "IUV".to_string(),
            EnvType::EnvTypeFloat,
            -22,
            20,
            "mm".to_string(),
            3.0,
            7,
            AccessType::DummyNodeVector0,
            nodes1,
        );
        let (_, env_var) =
            environment_variable(def1).expect("Failed to parse environment variable");
        assert_eq!(env_var1, env_var);
    }

    #[test]
    fn network_node_attribute_value_test() {
        let def = CompleteByteSlice(b"BA_ \"AttrName\" BU_ NodeName 12;\n");
        let node = AttributeValuedForObjectType::NetworkNodeAttributeValue("NodeName".to_string(), AttributeValue::AttributeValueF64(12.0));
        let attr_val_exp= AttributeValueForObject("AttrName".to_string(), node);
        let (_, attr_val) = attribute_value_for_object(def).unwrap();
        assert_eq!(attr_val_exp, attr_val);
    }

    #[test]
    fn message_definition_attribute_value_test() {
        let def = CompleteByteSlice(b"BA_ \"AttrName\" BO_ 298 13;\n");
        let msg_def = AttributeValuedForObjectType::MessageDefinitionAttributeValue(MessageId(298), Some(AttributeValue::AttributeValueF64(13.0)));
        let attr_val_exp= AttributeValueForObject("AttrName".to_string(), msg_def);
        let (_, attr_val) = attribute_value_for_object(def).unwrap();
        assert_eq!(attr_val_exp, attr_val);
    }

    #[test]
    fn signal_attribute_value_test() {
        let def = CompleteByteSlice(b"BA_ \"AttrName\" SG_ 198 SGName 13;\n");
        let msg_def = AttributeValuedForObjectType::SignalAttributeValue(MessageId(198), "SGName".to_string(), AttributeValue::AttributeValueF64(13.0));
        let attr_val_exp= AttributeValueForObject("AttrName".to_string(), msg_def);
        let (_, attr_val) = attribute_value_for_object(def).unwrap();
        assert_eq!(attr_val_exp, attr_val);
    }

    #[test]
    fn env_var_attribute_value_test() {
        let def = CompleteByteSlice(b"BA_ \"AttrName\" EV_ EvName \"CharStr\";\n");
        let msg_def = AttributeValuedForObjectType::EnvVariableAttributeValue("EvName".to_string(), AttributeValue::AttributeValueCharString("CharStr".to_string()));
        let attr_val_exp= AttributeValueForObject("AttrName".to_string(), msg_def);
        let (_, attr_val) = attribute_value_for_object(def).unwrap();
        assert_eq!(attr_val_exp, attr_val);
    }

    #[test]
    fn raw_attribute_value_test() {
        let def = CompleteByteSlice(b"BA_ \"AttrName\" \"RAW\";\n");
        let msg_def = AttributeValuedForObjectType::RawAttributeValue(AttributeValue::AttributeValueCharString("RAW".to_string()));
        let attr_val_exp= AttributeValueForObject("AttrName".to_string(), msg_def);
        let (_, attr_val) = attribute_value_for_object(def).unwrap();
        assert_eq!(attr_val_exp, attr_val);
    }

    #[test]
    fn new_symbols_test() {
        let def =
            CompleteByteSlice(b"NS_ :
                NS_DESC_
                CM_
                BA_DEF_

            ");
        let symbols_exp = vec!(Symbol("NS_DESC_".to_string()), Symbol("CM_".to_string()), Symbol("BA_DEF_".to_string()));
        let (_, symbols) = new_symbols(def).unwrap();
        assert_eq!(symbols_exp, symbols);
    }

    #[test]
    fn network_node_test() {
        let def = CompleteByteSlice(b"BU_: ZU XYZ ABC OIU\n");
        let nodes = vec!("ZU".to_string(), "XYZ".to_string(), "ABC".to_string(), "OIU".to_string());
        let (_, node) = node(def).unwrap();
        let node_exp = Node(nodes);
        assert_eq!(node_exp, node);
    }

     #[test]
    fn envvar_data_test() {
        let def = CompleteByteSlice(b"ENVVAR_DATA_ SomeEnvVarData: 399;\n");
        let (_, envvar_data) = environment_variable_data(def).unwrap();
        let envvar_data_exp = EnvironmentVariableData("SomeEnvVarData".to_string(), 399);
        assert_eq!(envvar_data_exp, envvar_data);
    }

    #[test]
    fn attribute_default_test() {
        let def = CompleteByteSlice(b"BA_DEF_DEF_  \"ZUV\" \"OAL\";\n");
        let (_, attr_default) = attribute_default(def).unwrap();
        let attr_default_exp = AttributeDefault("ZUV".to_string(), AttributeValue::AttributeValueCharString("OAL".to_string()));
        assert_eq!(attr_default_exp, attr_default);
    }

    #[test]
    fn attribute_value_f64_test() {
        let def = CompleteByteSlice(b"80.0");
        let (_, val) = attribute_value(def).unwrap();
        assert_eq!(AttributeValue::AttributeValueF64(80.0), val);
    }

    #[test]
    fn attribute_definition_test() {
        let def_bo = CompleteByteSlice(b"BA_DEF_ BO_ \"BaDef1BO\" INT 0 1000000;\n");
        let (_, bo_def) = attribute_definition(def_bo).unwrap();
        let bo_def_exp = AttributeDefinition::Message("\"BaDef1BO\" INT 0 1000000".to_string());
        assert_eq!(bo_def_exp, bo_def);

        let def_bu = CompleteByteSlice(b"BA_DEF_ BU_ \"BuDef1BO\" INT 0 1000000;\n");
        let (_, bu_def) = attribute_definition(def_bu).unwrap();
        let bu_def_exp = AttributeDefinition::Node("\"BuDef1BO\" INT 0 1000000".to_string());
        assert_eq!(bu_def_exp, bu_def);

        let def_signal = CompleteByteSlice(b"BA_DEF_ SG_ \"SgDef1BO\" INT 0 1000000;\n");
        let (_, signal_def) = attribute_definition(def_signal).unwrap();
        let signal_def_exp = AttributeDefinition::Signal("\"SgDef1BO\" INT 0 1000000".to_string());
        assert_eq!(signal_def_exp, signal_def);

        let def_env_var = CompleteByteSlice(b"BA_DEF_ EV_ \"EvDef1BO\" INT 0 1000000;\n");
        let (_, env_var_def) = attribute_definition(def_env_var).unwrap();
        let env_var_def_exp = AttributeDefinition::EnvironmentVariable("\"EvDef1BO\" INT 0 1000000".to_string());
        assert_eq!(env_var_def_exp, env_var_def);
    }

    #[test]
    fn version_test() {
        let def = CompleteByteSlice(b"VERSION \"HNPBNNNYNNNNNNNNNNNNNNNNNNNNNNNNYNYYYYYYYY>4>%%%/4>'%**4YYY///\"\n");
        let version_exp = Version("HNPBNNNYNNNNNNNNNNNNNNNNNNNNNNNNYNYYYYYYYY>4>%%%/4>'%**4YYY///".to_string());
        let (_, version) = version(def).unwrap();
        assert_eq!(version_exp, version);
    }

    #[test]
    fn dbc_definition_test() {
        let sample_dbc = 
        b"
VERSION \"0.1\"
NS_ :
    NS_DESC_
    CM_
    BA_DEF_
    BA_
    VAL_
    CAT_DEF_
    CAT_
    FILTER
    BA_DEF_DEF_
    EV_DATA_
    ENVVAR_DATA_
    SGTYPE_
    SGTYPE_VAL_
    BA_DEF_SGTYPE_
    BA_SGTYPE_
    SIG_TYPE_REF_
    VAL_TABLE_
    SIG_GROUP_
    SIG_VALTYPE_
    SIGTYPE_VALTYPE_
    BO_TX_BU_
    BA_DEF_REL_
    BA_REL_
    BA_DEF_DEF_REL_
    BU_SG_REL_
    BU_EV_REL_
    BU_BO_REL_
    SG_MUL_VAL_
BS_:
BU_: PC
BO_ 2000 WebData_2000: 4 Vector__XXX
    SG_ Signal_8 : 24|8@1+ (1,0) [0|255] \"\" Vector__XXX
    SG_ Signal_7 : 16|8@1+ (1,0) [0|255] \"\" Vector__XXX
    SG_ Signal_6 : 8|8@1+ (1,0) [0|255] \"\" Vector__XXX
    SG_ Signal_5 : 0|8@1+ (1,0) [0|255] \"\" Vector__XXX
BO_ 1840 WebData_1840: 4 PC
    SG_ Signal_4 : 24|8@1+ (1,0) [0|255] \"\" Vector__XXX
    SG_ Signal_3 : 16|8@1+ (1,0) [0|255] \"\" Vector__XXX
    SG_ Signal_2 : 8|8@1+ (1,0) [0|255] \"\" Vector__XXX
    SG_ Signal_1 : 0|8@1+ (1,0) [0|0] \"\" Vector__XXX

EV_ Environment1: 0 [0|220] \"\" 0 6 DUMMY_NODE_VECTOR0 DUMMY_NODE_VECTOR2;
EV_ Environment2: 0 [0|177] \"\" 0 7 DUMMY_NODE_VECTOR1 DUMMY_NODE_VECTOR2;
ENVVAR_DATA_ SomeEnvVarData: 399;

CM_ SG_ 4 TestSigLittleUnsigned1 \"asaklfjlsdfjlsdfgls
HH?=(%)/&KKDKFSDKFKDFKSDFKSDFNKCnvsdcvsvxkcv\";
CM_ SG_ 5 TestSigLittleUnsigned1 \"asaklfjlsdfjlsdfgls
=0943503450KFSDKFKDFKSDFKSDFNKCnvsdcvsvxkcv\";

BA_DEF_DEF_ \"BusType\" \"AS\";

BA_ \"Attr\" BO_ 4358435 283;
BA_ \"Attr\" BO_ 56949545 344;
";
        match DBC::from_slice(sample_dbc) {
            Ok(dbc_content) => println!("DBC Content{:#?}", dbc_content),
            Err(e) => {
                match e {
                    Error::NomError(nom::Err::Incomplete(needed)) => eprintln!("Error incomplete input, needed: {:?}", needed),
                    Error::NomError(nom::Err::Error(ctx)) => {
                        match ctx {
                            verbose_errors::Context::Code(i, kind) => eprintln!("Error Kind: {:?}, Code: {:?}", kind, str::from_utf8(i.as_bytes())),
                            verbose_errors::Context::List(l)=> eprintln!("Error List: {:?}", l),
                        }
                    }
                    Error::NomError(nom::Err::Failure(ctx)) => eprintln!("Failure {:?}", ctx),
                    Error::Incomplete(dbc, remaining) => eprintln!("Not all data in buffer was read {:#?}, remaining unparsed: {}", dbc, String::from_utf8(remaining).unwrap())
                }
                panic!("Failed to read DBC");
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Label(String);

/// Baudrate in kbit/s
#[derive(Debug, PartialEq)]
pub struct Baudrate(u64);

#[derive(Debug, PartialEq)]
pub struct Signal {
    name: String,
    multiplexer_indicator: MultiplexIndicator,
    pub start_bit: u64,
    pub signal_size: u64,
    byte_order: ByteOrder,
    value_type: ValueType,
    pub factor: f64,
    pub offset: f64,
    pub min: f64,
    pub max: f64,
    unit: String,
    receivers: Vec<String>,
}

impl Signal {
    pub fn name(&self) -> &str {
        &self.name
    } 

    pub fn multiplexer_indicator(&self) -> &MultiplexIndicator {
        &self.multiplexer_indicator
    }

    pub fn byte_order(&self) -> &ByteOrder {
        &self.byte_order
    }

    pub fn value_type(&self) -> &ValueType {
        &self.value_type
    }

    pub fn unit(&self) -> &str {
        &self.unit
    }

    pub fn receivers(&self) -> &Vec<String> {
        &self.receivers
    }
}

#[derive(Debug, PartialEq)]
pub struct SignalCommentId(u64);

#[derive(Debug, PartialEq)]
pub struct MessageId(u64);

#[derive(Debug, PartialEq)]
pub enum Transmitter {
    /// node transmitting the message
    NodeName(String),
    /// message has no sender
    VectorXXX
}

#[derive(Debug, PartialEq)]
pub struct MessageTransmitter {
    message_id: MessageId,
    transmitter: Transmitter,
}

#[derive(Debug, PartialEq)]
pub struct Version(String);

#[derive(Debug, PartialEq)]
pub struct Symbol(String);

#[derive(Debug, PartialEq)]
pub enum MultiplexIndicator {
    /// Multiplexor switch
    Multiplexor,
    /// Signal us being multiplexed by the multiplexer switch.
    MultiplexedSignal(u64),
    /// Normal signal
    Plain,
}

#[derive(Debug, PartialEq)]
pub enum ByteOrder {
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

#[derive(Debug, PartialEq)]
pub struct LabelDescription {
    id: u64,
    signal_name: String,
    labels: Vec<Label>,
    extended: bool,
}

#[derive(Debug, PartialEq)]
pub enum AccessType {
    DummyNodeVector0,
    DummyNodeVector1,
    DummyNodeVector2,
    DummyNodeVector3,
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
    MessageDefinitionAttributeValue(MessageId, Option<AttributeValue>),
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
pub struct ValDescription {
    a: f64,
    b: String,
}

#[derive(Debug, PartialEq)]
pub struct AttrDefault {
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
pub struct ValueTable {
    value_table_name: String,
    value_descriptions: Vec<ValDescription>
}

#[derive(Debug, PartialEq)]
pub enum Comment {
    NetworkNode(String),
    MessageDefinition(MessageId, String, String, bool),
    Signal(SignalCommentId, String, String, bool),
    EnvVar(String),
    // TODO figure out if PLAIN is correct
    Plain(String),
}

#[derive(Debug, PartialEq)]
pub struct Message {
    message_id: MessageId,
    message_name: String,
    message_size: u64,
    transmitter: Transmitter,
    signals: Vec<Signal>
}

impl Message {
    pub fn message_name(&self) -> &str {
        &self.message_name
    }

    pub fn transmitter(&self) -> &Transmitter {
        &self.transmitter
    }

    pub fn signals(&self) -> &Vec<Signal> {
        &self.signals
    }
}

#[derive(Debug, PartialEq)]
pub struct EnvironmentVariable(
        String,
        EnvType,
        i64,
        i64,
        String,
        f64,
        i64,
        AccessType,
        Vec<AccessNode>,
);

#[derive(Debug, PartialEq)]
pub struct EnvironmentVariableData(String, u64);

///
/// Network node
///
#[derive(Debug, PartialEq)]
pub struct Node(Vec<String>);

///
/// BA_DEF_DEF
///
#[derive(Debug, PartialEq)]
pub struct AttributeDefault(String, AttributeValue);

#[derive(Debug, PartialEq)]
pub struct AttributeValueForObject(String, AttributeValuedForObjectType);

#[derive(Debug, PartialEq)]
pub enum AttributeDefinition {
    // TODO add properties
    Message(String),
    // TODO add properties
    Node(String),
    // TODO add properties
    Signal(String),
    // TODO add properties
    EnvironmentVariable(String),
    // TODO figure out name
    Plain(String),
}

#[derive(Debug, PartialEq)]
pub enum ValueDescription {
    Signal(MessageId, String, Vec<ValDescription>),
    EnvironmentVariable(String, Vec<ValDescription>),
}

#[derive(Debug, PartialEq)]
pub struct SignalTypeRef {
    message_id: MessageId,
    signal_name: String,
    signal_type_name: String,
}

#[derive(Debug, PartialEq)]
pub struct SignalGroups {
    message_id: MessageId,
    signal_group_name: String,
    repetitions: u64,
    signal_names: Vec<String>,
}

#[derive(Debug, PartialEq)]
pub enum SignalExtendedValueType {
    SignedOrUnsignedInteger,
    IEEEfloat32Bit,
    IEEEdouble64bit,
}

#[derive(Debug, PartialEq)]
pub struct SignalExtendedValueTypeList {
    message_id: MessageId,
    signal_name: String,
    signal_extended_value_type: SignalExtendedValueType
}

#[derive(Debug, PartialEq)]
pub struct DBC {
    version: Version,
    new_symbols: Vec<Symbol>,
    bit_timing: Option<Vec<Baudrate>>,
    nodes: Vec<Node>,
    value_tables: Vec<ValueTable>,
    messages: Vec<Message>,
    message_transmitters: Vec<MessageTransmitter>,
    environment_variables: Vec<EnvironmentVariable>,
    environment_variable_data: Vec<EnvironmentVariableData>,
    multiplexer_indicator: Vec<MultiplexIndicator>,
    ///
    /// Object comments.
    ///
    comments: Vec<Comment>,
    attribute_definitions: Vec<AttributeDefinition>,
    // undefined
    // sigtype_attr_list: SigtypeAttrList,
    attribute_defaults: Vec<AttributeDefault>,
    attribute_values: Vec<AttributeValueForObject>,
    ///
    /// Encoding for signal raw values.
    ///
    value_descriptions: Vec<ValueDescription>,
    // obsolete + undefined
    // category_definitions: Vec<CategoryDefinition>,
    // obsolete + undefined
    //categories: Vec<Category>,
    // obsolete + undefined
    //filter: Vec<Filter>,
    signal_type_refs: Vec<SignalTypeRef>,
    ///
    /// signal groups define a group of signals within a message
    ///
    signal_groups: Option<SignalGroups>,
    signal_extended_value_type_list: Option<SignalExtendedValueTypeList>,
}

#[derive(Debug)]
pub enum Error<'a> {
    // Remaining String
    Incomplete(DBC, Vec<u8>),
    NomError(nom::Err<nom::types::CompleteByteSlice<'a>, u32>)
}

impl DBC {
    pub fn from_slice(buffer: &[u8]) -> Result<DBC, Error> {
        match dbc(CompleteByteSlice(buffer)) {
            Ok((remaining, dbc)) => {
                if !remaining.is_empty() {
                    return Err(Error::Incomplete(dbc, remaining.as_bytes().to_vec()));
                }
                Ok(dbc)
            },
            Err(e) => Err(Error::NomError(e))
        }
    }

    pub fn messages(&self) -> &Vec<Message> {
        &self.messages
    }
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

fn is_c_ident_head(chr: char) -> bool {
   chr.is_alphabetic() || chr == '_'
}

fn is_quote(chr: char) -> bool {
    chr == '"'
}

/// Single space
named!(ss<CompleteByteSlice, char>, char!(' '));

/// Colon
named!(colon<CompleteByteSlice, char>, char!(':'));

/// Comma aka ','
named!(comma<CompleteByteSlice, char>, char!(','));

/// Comma aka ';'
named!(semi_colon<CompleteByteSlice, char>, char!(';'));

/// Quote aka '"'
named!(quote<CompleteByteSlice, char>, char!('"'));

named!(pipe<CompleteByteSlice, char>, char!('|'));

named!(at<CompleteByteSlice, char>, char!('@'));

/// brace open aka '('
named!(brc_open<CompleteByteSlice, char>, char!('('));

/// brace close aka '('
named!(brc_close<CompleteByteSlice, char>, char!(')'));

/// bracket open aka '['
named!(brk_open<CompleteByteSlice, char>, char!('['));

/// bracket close aka ']'
named!(brk_close<CompleteByteSlice, char>, char!(']'));

/// A valid C_identifier. C_identifiers start with a  alphacharacter or an underscore
/// and may further consist of alpha­numeric, characters and underscore
named!(c_ident<CompleteByteSlice, String>,
    map_res!(
        recognize!(
            do_parse!(
                take_while1!(|x| is_c_ident_head(x as char))  >>
                take_while!(|x| is_c_string_char(x as char)) >>
                ()
            )
        ),
        |x: CompleteByteSlice| String::from_utf8(x.as_bytes().to_vec())
    )
);

named!(c_ident_vec<CompleteByteSlice, Vec<String>>, separated_nonempty_list!(comma, c_ident));

named!(char_string<CompleteByteSlice, String>, map!(take_till_s!(|c| is_space_s(c as char)), |s| String::from_utf8(s.as_bytes().to_vec()).unwrap()));

named!(u64_s<CompleteByteSlice, u64>, map_res!(
        digit,
        |s: CompleteByteSlice| std::str::FromStr::from_str(str::from_utf8(s.as_bytes()).unwrap())
    )
 );

named!(i64_digit<CompleteByteSlice, i64>,
    flat_map!(recognize!(tuple!(opt!(alt_complete!(char!('+') | char!('-'))), digit)), parse_to!(i64))
);

named!(quoted<CompleteByteSlice, String>,
    do_parse!(
            quote                                 >>
        s:  take_till_s!(|c |is_quote(c as char)) >>
            quote                                 >>
        (String::from_utf8_lossy(s.as_bytes()).to_string())
    )
);

named!(pub little_endian<CompleteByteSlice, ByteOrder>, map!(char!('1'), |_| ByteOrder::LittleEndian));

named!(pub big_endian<CompleteByteSlice, ByteOrder>, map!(char!('0'), |_| ByteOrder::BigEndian));

named!(pub byte_order<CompleteByteSlice, ByteOrder>, alt_complete!(little_endian | big_endian));

named!(pub message_id<CompleteByteSlice, MessageId>, map!(u64_s, MessageId));

named!(pub signal_comment_id<CompleteByteSlice, SignalCommentId>, map!(u64_s, SignalCommentId));

named!(pub signed<CompleteByteSlice, ValueType>, map!(char!('-'), |_| ValueType::Signed));

named!(pub unsigned<CompleteByteSlice, ValueType>, map!(char!('+'), |_| ValueType::Unsigned));

named!(pub value_type<CompleteByteSlice, ValueType>, alt_complete!(signed | unsigned));

named!(pub multiplexer<CompleteByteSlice, MultiplexIndicator>,
    do_parse!(
           ss         >>
           char!('m') >>
        d: u64_s      >>
           ss         >>
        (MultiplexIndicator::MultiplexedSignal(d))
    )
);

named!(pub multiplexor<CompleteByteSlice, MultiplexIndicator>,
    do_parse!(
        ss         >>
        char!('M') >>
        ss         >>
        (MultiplexIndicator::Multiplexor)
    )
);

named!(pub plain<CompleteByteSlice, MultiplexIndicator>,
    do_parse!(
        ss >>
        (MultiplexIndicator::Plain)
    )
);

named!(pub version<CompleteByteSlice, Version>,
    do_parse!(
           multispace0 >>
           tag!("VERSION")         >>
           ss                      >>
        v: quoted                  >>
        eol >>
        (Version(v))
    )
);

named!(pub bit_timing<CompleteByteSlice, Vec<Baudrate>>,
    do_parse!(
                   multispace0                                                                  >>
                   tag!("BS_:")                                                                 >>
        baudrates: opt!(preceded!(ss,  separated_nonempty_list!(comma, map!(u64_s, Baudrate)))) >>
        (baudrates.unwrap_or(Vec::new()))
    )
);

named!(pub multiplexer_indicator<CompleteByteSlice, MultiplexIndicator>,
    do_parse!(
        x: alt_complete!(multiplexer | multiplexor | plain) >>
        (x)
    )
);

named!(pub signal<CompleteByteSlice, Signal>,
    do_parse!(                multispace0           >>
                              tag!("SG_")           >>
                              ss                    >>
       name:                  c_ident               >>
       multiplexer_indicator: multiplexer_indicator >>
                              colon                 >>
                              ss                    >>
       start_bit:             u64_s                 >>
                              pipe                  >>
       signal_size:           u64_s                 >>
                              at                    >>
       byte_order:            byte_order            >>
       value_type:            value_type            >>
                              ss                    >>
                              brc_open              >>
       factor:                double                >>
                              comma                 >>
       offset:                double                >>
                              brc_close             >>
                              ss                    >>
                              brk_open              >>
       min:                   double                >>
                              pipe                  >>
       max:                   double                >>
                              brk_close             >>
                              ss                    >>
       unit:                  quoted                >>
                              ss                    >>
       receivers:             c_ident_vec           >>
       eol                                          >>
        (Signal {
            name: name,
            multiplexer_indicator: multiplexer_indicator,
            start_bit: start_bit,
            signal_size: signal_size,
            byte_order: byte_order,
            value_type: value_type,
            factor: factor,
            offset: offset,
            min: min,
            max: max,
            unit:  unit,
            receivers: receivers,
        })
    )
);

named!(pub message<CompleteByteSlice, Message>,
  do_parse!(
                  multispace0                                                                 >>
                  tag!("BO_")                                                                 >>
                  ss                                                                          >>
    message_id:   message_id                                                                  >>
                  ss                                                                          >>
    message_name: map!(take_till_s!(|c| is_colon(c as char)), |s| String::from_utf8(s.as_bytes().to_vec()).unwrap()) >>
                  colon                                                                       >>
                  ss                                                                          >>
    message_size: u64_s                                                                       >>
                  ss                                                                          >>
    transmitter:  transmitter                                                                 >>
    signals:      many1!(signal)                                                              >>
    (Message {
        message_id,
        message_name: message_name,
        message_size,
        transmitter,
        signals
    })
  )
);

named!(pub attribute_default<CompleteByteSlice, AttributeDefault>,
    do_parse!(
                        multispace0 >>
                         tag!("BA_DEF_DEF_") >>
                         ss                  >>
                         ss                  >>
        attribute_name:  quoted              >>
                         ss                  >>
        attribute_value: attribute_value     >>
                         semi_colon          >>
                         eol                 >>
        (AttributeDefault(attribute_name, attribute_value))
    )
);

named!(pub signal_comment<CompleteByteSlice, Comment>,
    do_parse!(
                 tag!("SG_")       >>
                 ss                >>
        id:      signal_comment_id >>
                 ss                >>
        name:    c_ident           >>
                 ss                >>
        comment: quoted            >>
        (Comment::Signal(id, name, comment, false))
    )
);

named!(pub message_definition_comment<CompleteByteSlice, Comment>,
    do_parse!(
                  tag!("BO_")                                                                   >>
                  ss                                                                            >>
        id:       message_id                                                                    >>
                  ss                                                                            >>
                  // TODO not only c ident ?
        node_name:     char_string >>
                  ss                                                                            >>
        comment: quoted                                                                         >>
        (Comment::MessageDefinition(id, node_name, comment, false))
    )
);

named!(pub comment_plain<CompleteByteSlice, Comment>,
    do_parse!(
        comment: quoted                                                                         >>
        (Comment::Plain(comment))
    )
);

named!(pub comment<CompleteByteSlice, Comment>,
    do_parse!(
           multispace0                           >>
           tag!("CM_")                                       >>
           ss                                                >>
        c: alt!(signal_comment | message_definition_comment | comment_plain) >>
           semi_colon                                        >>
           eol                                               >>
        (c)
    )
);

named!(pub value_description<CompleteByteSlice, ValDescription>,
    do_parse!(
        a: double >>
           ss     >>
        b: quoted >>
        (ValDescription { a: a, b: b })
    )
);

named!(pub value_description_for_signal<CompleteByteSlice, ValueDescription>,
    do_parse!(
              tag!("VAL_")                                                                     >>
              ss                                                                               >>
        id:   message_id                                                                       >>
              ss                                                                               >>
        name: c_ident                                                                          >>
        descriptions:  many_till!(preceded!(ss, value_description), preceded!(ss, semi_colon)) >>
        (ValueDescription::Signal(id, name, descriptions.0))
    )
);

named!(pub value_description_for_env_var<CompleteByteSlice, ValueDescription>,
    do_parse!(
                      tag!("VAL_")                                                            >>
                      ss                                                                      >>
        name:         c_ident                                                                 >>
        descriptions: many_till!(preceded!(ss, value_description), preceded!(ss, semi_colon)) >>
        (ValueDescription::EnvironmentVariable(name, descriptions.0))
    )
);

named!(pub value_descriptions<CompleteByteSlice, ValueDescription>,
    do_parse!(
        multispace0 >>
        x: alt_complete!(value_description_for_signal | value_description_for_env_var) >>
        eol >>
        (x)
    )
);

named!(env_float<CompleteByteSlice, EnvType>, value!(EnvType::EnvTypeFloat, char!('0')));
named!(env_int<CompleteByteSlice, EnvType>, value!(EnvType::EnvTypeu64, char!('1')));
named!(env_data<CompleteByteSlice, EnvType>, value!(EnvType::EnvTypeData, char!('2')));

/// 9 Environment Variable Definitions
named!(pub env_var_type<CompleteByteSlice, EnvType>, alt_complete!(env_float | env_int | env_data));

named!(dummy_node_vector_0<CompleteByteSlice, AccessType>, value!(AccessType::DummyNodeVector0, char!('0')));
named!(dummy_node_vector_1<CompleteByteSlice, AccessType>, value!(AccessType::DummyNodeVector1, char!('1')));
named!(dummy_node_vector_2<CompleteByteSlice, AccessType>, value!(AccessType::DummyNodeVector2, char!('2')));
named!(dummy_node_vector_3<CompleteByteSlice, AccessType>, value!(AccessType::DummyNodeVector3, char!('3')));

/// 9 Environment Variable Definitions
named!(pub access_type<CompleteByteSlice, AccessType>,
    do_parse!(
              tag!("DUMMY_NODE_VECTOR") >>
        node: alt_complete!(dummy_node_vector_0 | dummy_node_vector_1 | dummy_node_vector_2 | dummy_node_vector_3) >>
        (node)
    )
);

named!(access_node_vector_xxx<CompleteByteSlice, AccessNode>,  value!(AccessNode::AccessNodeVectorXXX, tag!("VECTOR_XXX")));
named!(access_node_name<CompleteByteSlice, AccessNode>,  map!(c_ident, |name| AccessNode::AccessNodeName(name)));

/// 9 Environment Variable Definitions
named!(pub access_node<CompleteByteSlice, AccessNode>, alt_complete!(access_node_vector_xxx | access_node_name));

/// 9 Environment Variable Definitions
named!(pub environment_variable<CompleteByteSlice, EnvironmentVariable>,
    do_parse!(
                       multispace0                      >>
                       tag!("EV_")                                  >>
                       ss                                           >>
        name:          c_ident                                      >>
                       colon                                        >>
                       ss                                           >>
        type_:         env_var_type                                 >>
                       ss                                           >>
                       brk_open                                     >>
        min:           i64_digit                                    >>
                       pipe                                         >>
        max:           i64_digit                                    >>
                       brk_close                                    >>
                       ss                                           >>
        unit:          quoted                                       >>
                       ss                                           >>
        initial_value: double                                       >>
                       ss                                           >>
        id:            i64_digit                                    >>
                       ss                                           >>
        access_type:   access_type                                  >>
                       ss                                           >>
        access_nodes:  separated_nonempty_list!(comma, access_node) >>
                       semi_colon                                   >>
                       eol                                          >>
       (EnvironmentVariable(name, type_, min, max, unit, initial_value, id, access_type, access_nodes))
    )
);

named!(pub environment_variable_data<CompleteByteSlice, EnvironmentVariableData>,
    do_parse!(
                      multispace0 >>
                      tag!("ENVVAR_DATA_")    >>
                      ss                      >>
        env_var_name: c_ident                 >>
                      colon                   >>
                      ss                      >>
        data_size:    u64_s                   >>
                      semi_colon              >>
                      eol                     >>
        (EnvironmentVariableData(env_var_name, data_size))
    )
);

named!(pub attribute_value_uint64<CompleteByteSlice, AttributeValue>,
    map!(u64_s, AttributeValue::AttributeValueU64)
);

named!(pub attribute_value_int64<CompleteByteSlice, AttributeValue>,
    map!(i64_digit, AttributeValue::AttributeValueI64)
);

named!(pub attribute_value_f64<CompleteByteSlice, AttributeValue>,
    map!(double, AttributeValue::AttributeValueF64)
);

named!(pub attribute_value_charstr<CompleteByteSlice, AttributeValue>,
    map!(quoted, |x| AttributeValue::AttributeValueCharString(x))
);

named!(pub attribute_value<CompleteByteSlice, AttributeValue>,
    alt!(
        //attribute_value_uint64 |
       // attribute_value_int64  |
        attribute_value_f64    |
        attribute_value_charstr
    )
);

named!(pub network_node_attribute_value<CompleteByteSlice, AttributeValuedForObjectType>,
    do_parse!(
                   tag!("BU_")     >>
                   ss              >>
        node_name: c_ident         >>
                   ss              >>
        value:     attribute_value >>
        (AttributeValuedForObjectType::NetworkNodeAttributeValue(node_name, value))
    )
);

named!(pub message_definition_attribute_value<CompleteByteSlice, AttributeValuedForObjectType>,
    do_parse!(
                    tag!("BO_")           >>
                    ss                    >>
        message_id: message_id            >>
                    ss                    >>
        value:      opt!(attribute_value) >>
        (AttributeValuedForObjectType::MessageDefinitionAttributeValue(message_id, value))
    )
);

named!(pub signal_attribute_value<CompleteByteSlice, AttributeValuedForObjectType>,
    do_parse!(
                     tag!("SG_")     >>
                     ss              >>
        message_id:  message_id      >>
                     ss              >>
        signal_name: c_ident         >>
                     ss              >>
        value:       attribute_value >>
        (AttributeValuedForObjectType::SignalAttributeValue(message_id, signal_name, value))
    )
);

named!(pub env_variable_attribute_value<CompleteByteSlice, AttributeValuedForObjectType>,
    do_parse!(
                      tag!("EV_")     >>
                      ss              >>
        env_var_name: c_ident         >>
                      ss              >>
        value:        attribute_value >>
        (AttributeValuedForObjectType::EnvVariableAttributeValue(env_var_name, value))
    )
);

named!(pub raw_attribute_value<CompleteByteSlice, AttributeValuedForObjectType>,
    map!(attribute_value, AttributeValuedForObjectType::RawAttributeValue)
);

named!(pub attribute_value_for_object<CompleteByteSlice, AttributeValueForObject>,
    do_parse!(
               multispace0 >>
               tag!("BA_") >>
               ss          >>
        name:  quoted      >>
               ss          >>
        value: alt!(
                    network_node_attribute_value       |
                    message_definition_attribute_value |
                    signal_attribute_value             |
                    env_variable_attribute_value       |
                    raw_attribute_value
                )           >>
                semi_colon  >>
                eol         >>
        (AttributeValueForObject(name, value))
    )
);

// TODO add properties
named!(pub attribute_definition_node<CompleteByteSlice, AttributeDefinition>,
    do_parse!(
           tag!("BU_") >>
           ss          >>
        x: map!(take_till_s!(|c |is_semi_colon(c as char)), |x| String::from_utf8(x.as_bytes().to_vec()).unwrap()) >>
        (AttributeDefinition::Node(x))
    )
);

// TODO add properties
named!(pub attribute_definition_signal<CompleteByteSlice, AttributeDefinition>,
    do_parse!(
           tag!("SG_") >>
           ss          >>
        x: map!(take_till_s!(|c |is_semi_colon(c as char)), |x| String::from_utf8(x.as_bytes().to_vec()).unwrap()) >>
        (AttributeDefinition::Signal(x))
    )
);

// TODO add properties
named!(pub attribute_definition_environment_variable<CompleteByteSlice, AttributeDefinition>,
    do_parse!(
           tag!("EV_") >>
           ss          >>
        x: map!(take_till_s!(|c |is_semi_colon(c as char)), |x| String::from_utf8(x.as_bytes().to_vec()).unwrap()) >>
        (AttributeDefinition::EnvironmentVariable(x))
    )
);

// TODO add properties
named!(pub attribute_definition_message<CompleteByteSlice, AttributeDefinition>,
    do_parse!(
           tag!("BO_") >>
           ss          >>
        x: map!(take_till_s!(|c |is_semi_colon(c as char)), |x| String::from_utf8(x.as_bytes().to_vec()).unwrap()) >>
        (AttributeDefinition::Message(x))
    )
);

// TODO add properties
named!(pub attribute_definition_plain<CompleteByteSlice, AttributeDefinition>,
    do_parse!(
           ss          >>
        x: map!(take_till_s!(|c |is_semi_colon(c as char)), |x| String::from_utf8(x.as_bytes().to_vec()).unwrap()) >>
        (AttributeDefinition::Plain(x))
    )
);

named!(pub attribute_definition<CompleteByteSlice, AttributeDefinition>,
    do_parse!(
        multispace0 >>
        tag!("BA_DEF_")         >>
        ss                      >>
        def: alt!(attribute_definition_node                 |
                  attribute_definition_signal               |
                  attribute_definition_environment_variable |
                  attribute_definition_message              |
                  attribute_definition_plain
                 ) >>
        semi_colon >>
        eol        >>
        (def)
    )
);

named!(pub symbol<CompleteByteSlice, Symbol>,
    do_parse!(
                space   >>
        symbol: c_ident >>
        eol >>
        (Symbol(symbol))
    )
);

named!(pub new_symbols<CompleteByteSlice, Vec<Symbol>>,
    do_parse!(
                 multispace0    >>
                 tag!("NS_ :")  >>
                 eol            >>
        symbols: many0!(symbol) >>
        (symbols)
    )
);

///
/// Network node
///
named!(pub node<CompleteByteSlice, Node>,
    do_parse!(
            multispace0 >>
            tag!("BU_:")            >>
            ss                      >>
        li: separated_nonempty_list!(ss, c_ident) >>
        eol >>
        (Node(li))
    )
);

named!(pub signal_type_ref<CompleteByteSlice, SignalTypeRef>,
    do_parse!(
                          multispace0 >>
                          tag!("SGTYPE_") >>
                          ss              >>
        message_id:       message_id      >>
                          ss              >>
        signal_name:      c_ident         >>
                          ss              >>
                          colon           >>
                          ss              >>
        signal_type_name: c_ident         >>
                          semi_colon      >>
                          eol >>
        (SignalTypeRef {
            message_id: message_id,
            signal_name: signal_name,
            signal_type_name: signal_type_name,
        })
    )
);

named!(pub value_table<CompleteByteSlice, ValueTable>,
    do_parse!(
                            multispace0 >>
                            tag!("VAL_TABLE_")      >>
                            ss                      >>
        value_table_name:   c_ident                 >>
                            ss                      >>
        value_descriptions: many_till!(preceded!(ss, value_description), preceded!(ss, semi_colon)) >>
        eol >>
        (ValueTable {
            value_table_name: value_table_name,
            value_descriptions: value_descriptions.0
        })
    )
);

named!(pub signed_or_unsigned_integer<CompleteByteSlice, SignalExtendedValueType>, value!(SignalExtendedValueType::SignedOrUnsignedInteger, tag!("0")));
named!(pub ieee_float_32bit<CompleteByteSlice, SignalExtendedValueType>, value!(SignalExtendedValueType::IEEEfloat32Bit, tag!("1")));
named!(pub ieee_double_64bit<CompleteByteSlice, SignalExtendedValueType>, value!(SignalExtendedValueType::IEEEdouble64bit, tag!("2")));

named!(pub signal_extended_value_type<CompleteByteSlice, SignalExtendedValueType>,
    alt_complete!(
        signed_or_unsigned_integer |
        ieee_float_32bit           |
        ieee_double_64bit
    )
);

named!(pub signal_extended_value_type_list<CompleteByteSlice, SignalExtendedValueTypeList>,
    do_parse!(
        multispace0 >>
        tag!("SIG_VALTYPE_")                                   >>
        ss                                                     >>
        message_id: message_id                                 >>
        ss                                                     >>
        signal_name: c_ident                                   >>
        ss                                                     >>
        signal_extended_value_type: signal_extended_value_type >>
        semi_colon                                             >>
        eol                                                    >>
        (SignalExtendedValueTypeList {
            message_id: message_id,
            signal_name: signal_name,
            signal_extended_value_type: signal_extended_value_type,
        })
    )
);

named!(pub transmitter_vector_xxx<CompleteByteSlice, Transmitter>, value!(Transmitter::VectorXXX, tag!("Vector__XXX")));

named!(pub transmitter_node_name<CompleteByteSlice, Transmitter>, map!(c_ident, |x| Transmitter::NodeName(x)));

named!(pub transmitter<CompleteByteSlice, Transmitter>, alt_complete!(transmitter_vector_xxx | transmitter_node_name));

named!(pub message_transmitter<CompleteByteSlice, MessageTransmitter>,
    do_parse!(
                    multispace0 >>
                     tag!("BO_TX_BU_")      >>
                     ss                     >>
        message_id:  message_id             >>
                     ss                     >>
                     colon                  >>
                     ss                     >>
        transmitter: transmitter            >>
                     semi_colon             >>
                     eol >>
        (MessageTransmitter {
            message_id: message_id,
            transmitter: transmitter,
        })
    )
);

named!(pub signal_groups<CompleteByteSlice, SignalGroups>,
    do_parse!(
        multispace0                >>
        tag!("SIG_GROUP_")         >>
        message_id: message_id     >>
        signal_group_name: c_ident >>
        repetitions: u64_s         >>
        ss                         >>
        colon                      >>
        ss                         >>
        signal_names: c_ident_vec  >>
        semi_colon                 >>
        eol                        >>
        (SignalGroups{
            message_id: message_id,
            signal_group_name: signal_group_name,
            repetitions: repetitions,
            signal_names: signal_names,
        })
    )
);

declare_trace!();

named!(pub dbc<CompleteByteSlice, DBC>,
    do_parse!(
        version:                         version                               >>
        new_symbols:                     new_symbols                           >>
        bit_timing:                      opt!(bit_timing)                      >>
        nodes:                           many0!(node)                          >>
        value_tables:                    many0!(value_table)                   >>
        messages:                        many0!(message)                       >>
        message_transmitters:            many0!(message_transmitter)           >>
        environment_variables:           many0!(environment_variable)          >>
        environment_variable_data:       many0!(environment_variable_data)     >>
        multiplexer_indicator:           many0!(multiplexer_indicator)         >>
        comments:                        many0!(comment)                       >>
        attribute_definitions:           many0!(attribute_definition)          >>
        attribute_defaults:              many0!(attribute_default)             >>
        attribute_values:                many0!(attribute_value_for_object)    >>
        value_descriptions:              many0!(value_descriptions)            >>
        signal_type_refs:                many0!(signal_type_ref)               >>
        signal_groups:                   opt!(signal_groups)                   >>
        signal_extended_value_type_list: opt!(signal_extended_value_type_list) >>
        (DBC {
            version: version,
            new_symbols: new_symbols,
            bit_timing: bit_timing,
            nodes: nodes,
            value_tables: value_tables,
            messages: messages,
            message_transmitters: message_transmitters,
            environment_variables: environment_variables,
            environment_variable_data: environment_variable_data,
            multiplexer_indicator: multiplexer_indicator,
            comments: comments,
            attribute_definitions: attribute_definitions,
            attribute_defaults: attribute_defaults,
            attribute_values: attribute_values,
            value_descriptions: value_descriptions,
            signal_type_refs: signal_type_refs,
            signal_groups: signal_groups,
            signal_extended_value_type_list: signal_extended_value_type_list,
        })
    )
);