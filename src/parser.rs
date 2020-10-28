//!
//! Module containing nom parser combinators
//!

use crate::*;
use std::str;

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
    fn char_string_test() {
        let def = CompleteByteSlice(b"\"ab\x00\x7f\"");
        let (_, char_string) = char_string(def).unwrap();
        let exp = "ab\x00\x7f";
        assert_eq!(exp, char_string);
    }

    #[test]
    fn signal_test() {
        let signal_line = CompleteByteSlice(b"SG_ NAME : 3|2@1- (1,0) [0|0] \"x\" UFA\r\n");
        let _signal = signal(signal_line).unwrap();
    }

    #[test]
    fn byte_order_test() {
        let (_, big_endian) =
            byte_order(CompleteByteSlice(b"0")).expect("Failed to parse big endian");
        assert_eq!(ByteOrder::BigEndian, big_endian);

        let (_, little_endian) =
            byte_order(CompleteByteSlice(b"1")).expect("Failed to parse little endian");
        assert_eq!(ByteOrder::LittleEndian, little_endian);
    }

    #[test]
    fn multiplexer_indicator_test() {
        let (_, multiplexer) = multiplexer_indicator(CompleteByteSlice(b" m34920 "))
            .expect("Failed to parse multiplexer");
        assert_eq!(MultiplexIndicator::MultiplexedSignal(34920), multiplexer);

        let (_, multiplexor) =
            multiplexer_indicator(CompleteByteSlice(b" M ")).expect("Failed to parse multiplexor");
        assert_eq!(MultiplexIndicator::Multiplexor, multiplexor);

        let (_, plain) =
            multiplexer_indicator(CompleteByteSlice(b" ")).expect("Failed to parse plain");
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
        signal(CompleteByteSlice(
            b"\r\n\r\nSG_ BasL2 : 3|2@0- (1,0) [0|0] \"x\" DFA_FUS\r\n",
        ))
        .expect("Faield");
        let (_, _message_def) = message(def).expect("Failed to parse message definition");
    }

    #[test]
    fn signal_comment_test() {
        let def1 = CompleteByteSlice(b"CM_ SG_ 193 KLU_R_X \"This is a signal comment test\";\n");
        let message_id = MessageId(193);
        let comment1 = Comment::Signal {
            message_id,
            signal_name: "KLU_R_X".to_string(),
            comment: "This is a signal comment test".to_string(),
        };
        let (_, comment1_def) = comment(def1).expect("Failed to parse signal comment definition");
        assert_eq!(comment1, comment1_def);
    }

    #[test]
    fn message_definition_comment_test() {
        let def1 = CompleteByteSlice(b"CM_ BO_ 34544 \"Some Message comment\";\n");
        let message_id = MessageId(34544);
        let comment1 = Comment::Message {
            message_id,
            comment: "Some Message comment".to_string(),
        };
        let (_, comment1_def) =
            comment(def1).expect("Failed to parse message definition comment definition");
        assert_eq!(comment1, comment1_def);
    }

    #[test]
    fn node_comment_test() {
        let def1 = CompleteByteSlice(b"CM_ BU_ network_node \"Some network node comment\";\n");
        let comment1 = Comment::Node {
            node_name: "network_node".to_string(),
            comment: "Some network node comment".to_string(),
        };
        let (_, comment1_def) = comment(def1).expect("Failed to parse node comment definition");
        assert_eq!(comment1, comment1_def);
    }

    #[test]
    fn env_var_comment_test() {
        let def1 = CompleteByteSlice(b"CM_ EV_ ENVXYZ \"Some env var name comment\";\n");
        let comment1 = Comment::EnvVar {
            env_var_name: "ENVXYZ".to_string(),
            comment: "Some env var name comment".to_string(),
        };
        let (_, comment1_def) = comment(def1).expect("Failed to parse env var comment definition");
        assert_eq!(comment1, comment1_def);
    }

    #[test]
    fn value_description_for_signal_test() {
        let def1 = CompleteByteSlice(b"VAL_ 837 UF_HZ_OI 255 \"NOP\";\n");
        let message_id = MessageId(837);
        let signal_name = "UF_HZ_OI".to_string();
        let val_descriptions = vec![ValDescription {
            a: 255.0,
            b: "NOP".to_string(),
        }];
        let value_description_for_signal1 = ValueDescription::Signal {
            message_id,
            signal_name,
            value_descriptions: val_descriptions,
        };
        let (_, value_signal_def) =
            value_descriptions(def1).expect("Failed to parse value desc for signal");
        assert_eq!(value_description_for_signal1, value_signal_def);
    }

    #[test]
    fn value_description_for_env_var_test() {
        let def1 = CompleteByteSlice(b"VAL_ MY_ENV_VAR 255 \"NOP\";\n");
        let env_var_name = "MY_ENV_VAR".to_string();
        let val_descriptions = vec![ValDescription {
            a: 255.0,
            b: "NOP".to_string(),
        }];
        let value_env_var1 = ValueDescription::EnvironmentVariable {
            env_var_name,
            value_descriptions: val_descriptions,
        };
        let (_, value_env_var) =
            value_descriptions(def1).expect("Failed to parse value desc for env var");
        assert_eq!(value_env_var1, value_env_var);
    }

    #[test]
    fn environment_variable_test() {
        let def1 =
            CompleteByteSlice(b"EV_ IUV: 0 [-22|20] \"mm\" 3 7 DUMMY_NODE_VECTOR0 VECTOR_XXX;\n");
        let env_var1 = EnvironmentVariable {
            env_var_name: "IUV".to_string(),
            env_var_type: EnvType::EnvTypeFloat,
            min: -22,
            max: 20,
            unit: "mm".to_string(),
            initial_value: 3.0,
            ev_id: 7,
            access_type: AccessType::DummyNodeVector0,
            access_nodes: vec![AccessNode::AccessNodeVectorXXX],
        };
        let (_, env_var) =
            environment_variable(def1).expect("Failed to parse environment variable");
        assert_eq!(env_var1, env_var);
    }

    #[test]
    fn network_node_attribute_value_test() {
        let def = CompleteByteSlice(b"BA_ \"AttrName\" BU_ NodeName 12;\n");
        let attribute_value = AttributeValuedForObjectType::NetworkNodeAttributeValue(
            "NodeName".to_string(),
            AttributeValue::AttributeValueF64(12.0),
        );
        let attr_val_exp = AttributeValueForObject {
            attribute_name: "AttrName".to_string(),
            attribute_value,
        };
        let (_, attr_val) = attribute_value_for_object(def).unwrap();
        assert_eq!(attr_val_exp, attr_val);
    }

    #[test]
    fn message_definition_attribute_value_test() {
        let def = CompleteByteSlice(b"BA_ \"AttrName\" BO_ 298 13;\n");
        let attribute_value = AttributeValuedForObjectType::MessageDefinitionAttributeValue(
            MessageId(298),
            Some(AttributeValue::AttributeValueF64(13.0)),
        );
        let attr_val_exp = AttributeValueForObject {
            attribute_name: "AttrName".to_string(),
            attribute_value,
        };
        let (_, attr_val) = attribute_value_for_object(def).unwrap();
        assert_eq!(attr_val_exp, attr_val);
    }

    #[test]
    fn signal_attribute_value_test() {
        let def = CompleteByteSlice(b"BA_ \"AttrName\" SG_ 198 SGName 13;\n");
        let attribute_value = AttributeValuedForObjectType::SignalAttributeValue(
            MessageId(198),
            "SGName".to_string(),
            AttributeValue::AttributeValueF64(13.0),
        );
        let attr_val_exp = AttributeValueForObject {
            attribute_name: "AttrName".to_string(),
            attribute_value,
        };
        let (_, attr_val) = attribute_value_for_object(def).unwrap();
        assert_eq!(attr_val_exp, attr_val);
    }

    #[test]
    fn env_var_attribute_value_test() {
        let def = CompleteByteSlice(b"BA_ \"AttrName\" EV_ EvName \"CharStr\";\n");
        let attribute_value = AttributeValuedForObjectType::EnvVariableAttributeValue(
            "EvName".to_string(),
            AttributeValue::AttributeValueCharString("CharStr".to_string()),
        );
        let attr_val_exp = AttributeValueForObject {
            attribute_name: "AttrName".to_string(),
            attribute_value,
        };
        let (_, attr_val) = attribute_value_for_object(def).unwrap();
        assert_eq!(attr_val_exp, attr_val);
    }

    #[test]
    fn raw_attribute_value_test() {
        let def = CompleteByteSlice(b"BA_ \"AttrName\" \"RAW\";\n");
        let attribute_value = AttributeValuedForObjectType::RawAttributeValue(
            AttributeValue::AttributeValueCharString("RAW".to_string()),
        );
        let attr_val_exp = AttributeValueForObject {
            attribute_name: "AttrName".to_string(),
            attribute_value,
        };
        let (_, attr_val) = attribute_value_for_object(def).unwrap();
        assert_eq!(attr_val_exp, attr_val);
    }

    #[test]
    fn new_symbols_test() {
        let def = CompleteByteSlice(
            b"NS_ :
                NS_DESC_
                CM_
                BA_DEF_

            ",
        );
        let symbols_exp = vec![
            Symbol("NS_DESC_".to_string()),
            Symbol("CM_".to_string()),
            Symbol("BA_DEF_".to_string()),
        ];
        let (_, symbols) = new_symbols(def).unwrap();
        assert_eq!(symbols_exp, symbols);
    }

    #[test]
    fn network_node_test() {
        let def = CompleteByteSlice(b"BU_: ZU XYZ ABC OIU\n");
        let nodes = vec![
            "ZU".to_string(),
            "XYZ".to_string(),
            "ABC".to_string(),
            "OIU".to_string(),
        ];
        let (_, node) = node(def).unwrap();
        let node_exp = Node(nodes);
        assert_eq!(node_exp, node);
    }

    #[test]
    fn empty_network_node_test() {
        let def = CompleteByteSlice(b"BU_: \n");
        let nodes = vec![
        ];
        let (_, node) = node(def).unwrap();
        let node_exp = Node(nodes);
        assert_eq!(node_exp, node);
    }

    #[test]
    fn envvar_data_test() {
        let def = CompleteByteSlice(b"ENVVAR_DATA_ SomeEnvVarData: 399;\n");
        let (_, envvar_data) = environment_variable_data(def).unwrap();
        let envvar_data_exp = EnvironmentVariableData {
            env_var_name: "SomeEnvVarData".to_string(),
            data_size: 399,
        };
        assert_eq!(envvar_data_exp, envvar_data);
    }

    #[test]
    fn signal_type_test() {
        let def = CompleteByteSlice(
            b"SGTYPE_ signal_type_name: 1024@1+ (5,2) [1|3] \"unit\" 2.0 val_table;\n",
        );

        let exp = SignalType {
            signal_type_name: "signal_type_name".to_string(),
            signal_size: 1024,
            byte_order: ByteOrder::LittleEndian,
            value_type: ValueType::Unsigned,
            factor: 5.0,
            offset: 2.0,
            min: 1.0,
            max: 3.0,
            unit: "unit".to_string(),
            default_value: 2.0,
            value_table: "val_table".to_string(),
        };

        let (_, signal_type) = signal_type(def).unwrap();
        assert_eq!(exp, signal_type);
    }

    #[test]
    fn signal_groups_test() {
        let def = CompleteByteSlice(b"SIG_GROUP_ 23 X_3290 1 : A_b XY_Z;\n");

        let exp = SignalGroups {
            message_id: MessageId(23),
            signal_group_name: "X_3290".to_string(),
            repetitions: 1,
            signal_names: vec!["A_b".to_string(), "XY_Z".to_string()],
        };

        let (_, signal_groups) = signal_groups(def).unwrap();
        assert_eq!(exp, signal_groups);
    }

    #[test]
    fn attribute_default_test() {
        let def = CompleteByteSlice(b"BA_DEF_DEF_  \"ZUV\" \"OAL\";\n");
        let (_, attr_default) = attribute_default(def).unwrap();
        let attr_default_exp = AttributeDefault {
            attribute_name: "ZUV".to_string(),
            attribute_value: AttributeValue::AttributeValueCharString("OAL".to_string()),
        };
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
        let env_var_def_exp =
            AttributeDefinition::EnvironmentVariable("\"EvDef1BO\" INT 0 1000000".to_string());
        assert_eq!(env_var_def_exp, env_var_def);
    }

    #[test]
    fn version_test() {
        let def = CompleteByteSlice(
            b"VERSION \"HNPBNNNYNNNNNNNNNNNNNNNNNNNNNNNNYNYYYYYYYY>4>%%%/4>'%**4YYY///\"\n",
        );
        let version_exp =
            Version("HNPBNNNYNNNNNNNNNNNNNNNNNNNNNNNNYNYYYYYYYY>4>%%%/4>'%**4YYY///".to_string());
        let (_, version) = version(def).unwrap();
        assert_eq!(version_exp, version);
    }

    #[test]
    fn message_transmitters_test() {
        let def = CompleteByteSlice(b"BO_TX_BU_ 12345 : XZY,ABC;\n");
        let exp = MessageTransmitter {
            message_id: MessageId(12345),
            transmitter: vec![
                Transmitter::NodeName("XZY".to_string()),
                Transmitter::NodeName("ABC".to_string()),
            ],
        };
        let (_, transmitter) = message_transmitter(def).unwrap();
        assert_eq!(exp, transmitter);
    }

    #[test]
    fn value_description_test() {
        let def = CompleteByteSlice(b"2 \"ABC\"\n");
        let exp = ValDescription {
            a: 2f64,
            b: "ABC".to_string(),
        };
        let (_, val_desc) = value_description(def).unwrap();
        assert_eq!(exp, val_desc);
    }

    #[test]
    fn val_table_test() {
        let def = CompleteByteSlice(b"VAL_TABLE_ Tst 2 \"ABC\" 1 \"Test A\" ;\n");
        let exp = ValueTable {
            value_table_name: "Tst".to_string(),
            value_descriptions: vec![
                ValDescription {
                    a: 2f64,
                    b: "ABC".to_string(),
                },
                ValDescription {
                    a: 1f64,
                    b: "Test A".to_string(),
                },
            ],
        };
        let (_, val_table) = value_table(def).unwrap();
        assert_eq!(exp, val_table);
    }

    #[test]
    fn sig_val_type_test() {
        let def = CompleteByteSlice(b"SIG_VALTYPE_ 2000 Signal_8 : 1;\n");
        let exp = SignalExtendedValueTypeList {
            message_id: MessageId(2000),
            signal_name: "Signal_8".to_string(),
            signal_extended_value_type: SignalExtendedValueType::IEEEfloat32Bit,
        };

        let (_, extended_value_type_list) = signal_extended_value_type_list(def).unwrap();
        assert_eq!(extended_value_type_list, exp);
    }
}

fn is_semi_colon(chr: char) -> bool {
    chr == ';'
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

// Multi space
named!(multispace1<CompleteByteSlice, Vec<char>>, many1!(char!(' ')));

// Abreviation for multispace1
named!(ms<CompleteByteSlice, Vec<char>>, many1!(char!(' ')));

// Colon
named!(colon<CompleteByteSlice, char>, char!(':'));

// Comma aka ','
named!(comma<CompleteByteSlice, char>, char!(','));

// Comma aka ';'
named!(semi_colon<CompleteByteSlice, char>, char!(';'));

// Quote aka '"'
named!(quote<CompleteByteSlice, char>, char!('"'));

named!(pipe<CompleteByteSlice, char>, char!('|'));

named!(at<CompleteByteSlice, char>, char!('@'));

// brace open aka '('
named!(brc_open<CompleteByteSlice, char>, char!('('));

// brace close aka '('
named!(brc_close<CompleteByteSlice, char>, char!(')'));

// bracket open aka '['
named!(brk_open<CompleteByteSlice, char>, char!('['));

// bracket close aka ']'
named!(brk_close<CompleteByteSlice, char>, char!(']'));

// A valid C_identifier. C_identifiers start with a  alphacharacter or an underscore
// and may further consist of alpha­numeric, characters and underscore
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

named!(c_ident_vec<CompleteByteSlice, Vec<String>>, separated_list!(comma, c_ident));

named!(u32_s<CompleteByteSlice, u32>, map_res!(
        digit,
        |s: CompleteByteSlice| std::str::FromStr::from_str(str::from_utf8(s.as_bytes()).unwrap())
    )
 );

named!(u64_s<CompleteByteSlice, u64>, map_res!(
        digit,
        |s: CompleteByteSlice| std::str::FromStr::from_str(str::from_utf8(s.as_bytes()).unwrap())
    )
 );

named!(i64_digit<CompleteByteSlice, i64>,
    flat_map!(recognize!(tuple!(opt!(alt_complete!(char!('+') | char!('-'))), digit)), parse_to!(i64))
);

named!(char_string<CompleteByteSlice, String>,
    do_parse!(
            quote                                 >>
        s:  take_till!(|c| is_quote(c as char)) >>
            quote                                 >>
        (String::from_utf8_lossy(s.as_bytes()).to_string())
    )
);

named!(pub little_endian<CompleteByteSlice, ByteOrder>, map!(char!('1'), |_| ByteOrder::LittleEndian));

named!(pub big_endian<CompleteByteSlice, ByteOrder>, map!(char!('0'), |_| ByteOrder::BigEndian));

named!(pub byte_order<CompleteByteSlice, ByteOrder>, alt_complete!(little_endian | big_endian));

named!(pub message_id<CompleteByteSlice, MessageId>, map!(u32_s, MessageId));

named!(pub signed<CompleteByteSlice, ValueType>, map!(char!('-'), |_| ValueType::Signed));

named!(pub unsigned<CompleteByteSlice, ValueType>, map!(char!('+'), |_| ValueType::Unsigned));

named!(pub value_type<CompleteByteSlice, ValueType>, alt_complete!(signed | unsigned));

named!(pub multiplexer<CompleteByteSlice, MultiplexIndicator>,
    do_parse!(
           ms         >>
           char!('m') >>
        d: u64_s      >>
           ms         >>
        (MultiplexIndicator::MultiplexedSignal(d))
    )
);

named!(pub multiplexor<CompleteByteSlice, MultiplexIndicator>,
    do_parse!(
        ms         >>
        char!('M') >>
        ms         >>
        (MultiplexIndicator::Multiplexor)
    )
);

named!(pub plain<CompleteByteSlice, MultiplexIndicator>,
    do_parse!(
        ms >>
        (MultiplexIndicator::Plain)
    )
);

named!(pub version<CompleteByteSlice, Version>,
    do_parse!(
           multispace0 >>
           tag!("VERSION")         >>
           ms                      >>
        v: char_string             >>
        eol >>
        (Version(v))
    )
);

named!(pub bit_timing<CompleteByteSlice, Vec<Baudrate>>,
    do_parse!(
                   multispace0                                                                  >>
                   tag!("BS_:")                                                                 >>
        baudrates: opt!(preceded!(ms,  separated_list!(comma, map!(u64_s, Baudrate)))) >>
        (baudrates.unwrap_or_default())
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
                              ms                    >>
       name:                  c_ident               >>
       multiplexer_indicator: multiplexer_indicator >>
                              colon                 >>
                              ms                    >>
       start_bit:             u64_s                 >>
                              pipe                  >>
       signal_size:           u64_s                 >>
                              at                    >>
       byte_order:            byte_order            >>
       value_type:            value_type            >>
                              ms                    >>
                              brc_open              >>
       factor:                double                >>
                              comma                 >>
       offset:                double                >>
                              brc_close             >>
                              ms                    >>
                              brk_open              >>
       min:                   double                >>
                              pipe                  >>
       max:                   double                >>
                              brk_close             >>
                              ms                    >>
       unit:                  char_string           >>
                              ms                    >>
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
                  multispace0    >>
                  tag!("BO_")    >>
                  ms             >>
    message_id:   message_id     >>
                  ms             >>
    message_name: c_ident        >>
                  colon          >>
                  ms             >>
    message_size: u64_s          >>
                  ms             >>
    transmitter:  transmitter    >>
    signals:      many0!(signal) >>
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
                        multispace0          >>
                         tag!("BA_DEF_DEF_") >>
                         ms                  >>
        attribute_name:  char_string         >>
                         ms                  >>
        attribute_value: attribute_value     >>
                         semi_colon          >>
                         eol                 >>
        (AttributeDefault { attribute_name, attribute_value })
    )
);

named!(pub node_comment<CompleteByteSlice, Comment>,
    do_parse!(
                   tag!("BU_") >>
                   ms          >>
        node_name: c_ident     >>
                   ms          >>
        comment:   char_string >>
        (Comment::Node { node_name, comment })
    )
);

named!(pub message_comment<CompleteByteSlice, Comment>,
    do_parse!(
                    tag!("BO_") >>
                    ms          >>
        message_id: message_id  >>
                    ms          >>
        comment:    char_string >>
        (Comment::Message { message_id, comment })
    )
);

named!(pub signal_comment<CompleteByteSlice, Comment>,
    do_parse!(
                     tag!("SG_") >>
                     ms          >>
        message_id:  message_id  >>
                     ms          >>
        signal_name: c_ident     >>
                     ms          >>
        comment:     char_string >>
        (Comment::Signal { message_id, signal_name, comment })
    )
);

named!(pub env_var_comment<CompleteByteSlice, Comment>,
    do_parse!(
                      tag!("EV_") >>
                      ms          >>
        env_var_name: c_ident     >>
                      ms          >>
        comment:      char_string >>
        (Comment::EnvVar { env_var_name, comment })
    )
);

named!(pub comment_plain<CompleteByteSlice, Comment>,
    do_parse!(
        comment: char_string >>
        (Comment::Plain { comment })
    )
);

named!(pub comment<CompleteByteSlice, Comment>,
    do_parse!(
           multispace0                        >>
           tag!("CM_")                        >>
           ms                                 >>
        c: alt!( node_comment
               | message_comment
               | env_var_comment
               | signal_comment
               | comment_plain
               )                              >>
           semi_colon                         >>
           eol                                >>
        (c)
    )
);

named!(pub value_description<CompleteByteSlice, ValDescription>,
    do_parse!(
        a: double      >>
           ms          >>
        b: char_string >>
        (ValDescription { a: a, b: b })
    )
);

named!(pub value_description_for_signal<CompleteByteSlice, ValueDescription>,
    do_parse!(
                     tag!("VAL_")  >>
                     ms            >>
        message_id:  message_id    >>
                     ms            >>
        signal_name: c_ident       >>
        value_descriptions:  many_till!(preceded!(ms, value_description), preceded!(opt!(ms), semi_colon)) >>
        (ValueDescription::Signal {
            message_id,
            signal_name,
            value_descriptions: value_descriptions.0
        })
    )
);

named!(pub value_description_for_env_var<CompleteByteSlice, ValueDescription>,
    do_parse!(
                      tag!("VAL_")                                                                        >>
                      ms                                                                                  >>
        env_var_name: c_ident                                                                             >>
        value_descriptions: many_till!(preceded!(ms, value_description), preceded!(opt!(ms), semi_colon)) >>
        (ValueDescription::EnvironmentVariable {
            env_var_name,
            value_descriptions: value_descriptions.0
        })
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

// 9 Environment Variable Definitions
named!(pub env_var_type<CompleteByteSlice, EnvType>, alt_complete!(env_float | env_int | env_data));

named!(dummy_node_vector_0<CompleteByteSlice, AccessType>, value!(AccessType::DummyNodeVector0, char!('0')));
named!(dummy_node_vector_1<CompleteByteSlice, AccessType>, value!(AccessType::DummyNodeVector1, char!('1')));
named!(dummy_node_vector_2<CompleteByteSlice, AccessType>, value!(AccessType::DummyNodeVector2, char!('2')));
named!(dummy_node_vector_3<CompleteByteSlice, AccessType>, value!(AccessType::DummyNodeVector3, char!('3')));

// 9 Environment Variable Definitions
named!(pub access_type<CompleteByteSlice, AccessType>,
    do_parse!(
              tag!("DUMMY_NODE_VECTOR") >>
        node: alt_complete!(dummy_node_vector_0 | dummy_node_vector_1 | dummy_node_vector_2 | dummy_node_vector_3) >>
        (node)
    )
);

named!(access_node_vector_xxx<CompleteByteSlice, AccessNode>,  value!(AccessNode::AccessNodeVectorXXX, tag!("VECTOR_XXX")));
named!(access_node_name<CompleteByteSlice, AccessNode>,  map!(c_ident, |name| AccessNode::AccessNodeName(name)));

// 9 Environment Variable Definitions
named!(pub access_node<CompleteByteSlice, AccessNode>, alt_complete!(access_node_vector_xxx | access_node_name));

// 9 Environment Variable Definitions
named!(pub environment_variable<CompleteByteSlice, EnvironmentVariable>,
    do_parse!(
                       multispace0                                  >>
                       tag!("EV_")                                  >>
                       ms                                           >>
        env_var_name:  c_ident                                      >>
                       colon                                        >>
                       ms                                           >>
        env_var_type:  env_var_type                                 >>
                       ms                                           >>
                       brk_open                                     >>
        min:           i64_digit                                    >>
                       pipe                                         >>
        max:           i64_digit                                    >>
                       brk_close                                    >>
                       ms                                           >>
        unit:          char_string                                  >>
                       ms                                           >>
        initial_value: double                                       >>
                       ms                                           >>
        ev_id:         i64_digit                                    >>
                       ms                                           >>
        access_type:   access_type                                  >>
                       ms                                           >>
        access_nodes:  separated_list!(comma, access_node) >>
                       semi_colon                                   >>
                       eol                                          >>
       (EnvironmentVariable {
           env_var_name,
           env_var_type,
           min,
           max,
           unit,
           initial_value,
           ev_id,
           access_type,
           access_nodes
        })
    )
);

named!(pub environment_variable_data<CompleteByteSlice, EnvironmentVariableData>,
    do_parse!(
                      multispace0          >>
                      tag!("ENVVAR_DATA_") >>
                      ms                   >>
        env_var_name: c_ident              >>
                      colon                >>
                      ms                   >>
        data_size:    u64_s                >>
                      semi_colon           >>
                      eol                  >>
        (EnvironmentVariableData { env_var_name, data_size })
    )
);

named!(pub signal_type<CompleteByteSlice, SignalType>,
    do_parse!(
        multispace0                   >>
        tag!("SGTYPE_")               >>
                          ms          >>
        signal_type_name: c_ident     >>
                          colon       >>
                          ms          >>
        signal_size:      u64_s       >>
                          at          >>
        byte_order:       byte_order  >>
        value_type:       value_type  >>
                          ms          >>
                          brc_open    >>
        factor:           double      >>
                          comma       >>
        offset:           double      >>
                          brc_close   >>
                          ms          >>
                          brk_open    >>
        min:              double      >>
                          pipe        >>
        max:              double      >>
                          brk_close   >>
                          ms          >>
        unit:             char_string >>
                          ms          >>
        default_value:    double      >>
                          ms          >>
        value_table:      c_ident     >>
                          semi_colon  >>
                          eol         >>
        (SignalType {
            signal_type_name,
            signal_size,
            byte_order,
            value_type,
            factor,
            offset,
            min,
            max,
            unit,
            default_value,
            value_table,
        })
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
    map!(char_string, |x| AttributeValue::AttributeValueCharString(x))
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
                   ms              >>
        node_name: c_ident         >>
                   ms              >>
        value:     attribute_value >>
        (AttributeValuedForObjectType::NetworkNodeAttributeValue(node_name, value))
    )
);

named!(pub message_definition_attribute_value<CompleteByteSlice, AttributeValuedForObjectType>,
    do_parse!(
                    tag!("BO_")           >>
                    ms                    >>
        message_id: message_id            >>
                    ms                    >>
        value:      opt!(attribute_value) >>
        (AttributeValuedForObjectType::MessageDefinitionAttributeValue(message_id, value))
    )
);

named!(pub signal_attribute_value<CompleteByteSlice, AttributeValuedForObjectType>,
    do_parse!(
                     tag!("SG_")     >>
                     ms              >>
        message_id:  message_id      >>
                     ms              >>
        signal_name: c_ident         >>
                     ms              >>
        value:       attribute_value >>
        (AttributeValuedForObjectType::SignalAttributeValue(message_id, signal_name, value))
    )
);

named!(pub env_variable_attribute_value<CompleteByteSlice, AttributeValuedForObjectType>,
    do_parse!(
                      tag!("EV_")     >>
                      ms              >>
        env_var_name: c_ident         >>
                      ms              >>
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
                         ms          >>
        attribute_name:  char_string >>
               ms          >>
        attribute_value: alt!(
                              network_node_attribute_value       |
                              message_definition_attribute_value |
                              signal_attribute_value             |
                              env_variable_attribute_value       |
                              raw_attribute_value

                          )           >>
                          semi_colon  >>
                          eol         >>
        (AttributeValueForObject{ attribute_name, attribute_value })
    )
);

// TODO add properties
named!(pub attribute_definition_node<CompleteByteSlice, AttributeDefinition>,
    do_parse!(
           tag!("BU_") >>
           ms          >>
        x: map!(take_till!(|c |is_semi_colon(c as char)), |x| String::from_utf8(x.as_bytes().to_vec()).unwrap()) >>
        (AttributeDefinition::Node(x))
    )
);

// TODO add properties
named!(pub attribute_definition_signal<CompleteByteSlice, AttributeDefinition>,
    do_parse!(
           tag!("SG_") >>
           ms          >>
        x: map!(take_till!(|c |is_semi_colon(c as char)), |x| String::from_utf8(x.as_bytes().to_vec()).unwrap()) >>
        (AttributeDefinition::Signal(x))
    )
);

// TODO add properties
named!(pub attribute_definition_environment_variable<CompleteByteSlice, AttributeDefinition>,
    do_parse!(
           tag!("EV_") >>
           ms          >>
        x: map!(take_till!(|c |is_semi_colon(c as char)), |x| String::from_utf8(x.as_bytes().to_vec()).unwrap()) >>
        (AttributeDefinition::EnvironmentVariable(x))
    )
);

// TODO add properties
named!(pub attribute_definition_message<CompleteByteSlice, AttributeDefinition>,
    do_parse!(
           tag!("BO_") >>
           ms          >>
        x: map!(take_till!(|c |is_semi_colon(c as char)), |x| String::from_utf8(x.as_bytes().to_vec()).unwrap()) >>
        (AttributeDefinition::Message(x))
    )
);

// TODO add properties
named!(pub attribute_definition_plain<CompleteByteSlice, AttributeDefinition>,
    do_parse!(
        x: map!(take_till!(|c |is_semi_colon(c as char)), |x| String::from_utf8(x.as_bytes().to_vec()).unwrap()) >>
        (AttributeDefinition::Plain(x))
    )
);

named!(pub attribute_definition<CompleteByteSlice, AttributeDefinition>,
    do_parse!(
        multispace0     >>
        tag!("BA_DEF_") >>
        ms              >>
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
                 space0         >>
                 eol            >>
        symbols: many0!(symbol) >>
        (symbols)
    )
);

//
// Network node
//
named!(pub node<CompleteByteSlice, Node>,
    do_parse!(
            multispace0                           >>
            tag!("BU_:")                          >>
        li: opt!(preceded!(ms, separated_list!(ms, c_ident))) >>
        space0                                    >>
        eol                                       >>
        (Node(li.unwrap_or_default()))
    )
);

named!(pub signal_type_ref<CompleteByteSlice, SignalTypeRef>,
    do_parse!(
                          multispace0     >>
                          tag!("SGTYPE_") >>
                          ms              >>
        message_id:       message_id      >>
                          ms              >>
        signal_name:      c_ident         >>
                          ms              >>
                          colon           >>
                          ms              >>
        signal_type_name: c_ident         >>
                          semi_colon      >>
                          eol             >>
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
                            ms                      >>
        value_table_name:   c_ident                 >>
        value_descriptions: many_till!(preceded!(ms, value_description), preceded!(ms, semi_colon)) >>
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
        ms                                                     >>
        message_id: message_id                                 >>
        ms                                                     >>
        signal_name: c_ident                                   >>
        ms                                                     >>
        opt!(colon)                                            >>
        ms                                                     >>
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

named!(pub message_transmitters<CompleteByteSlice, Vec<Transmitter>>, separated_list!(comma, transmitter));

named!(pub message_transmitter<CompleteByteSlice, MessageTransmitter>,
    do_parse!(
                    multispace0 >>
                     tag!("BO_TX_BU_")      >>
                     ms                     >>
        message_id:  message_id             >>
                     ms                     >>
                     colon                  >>
                     ms                     >>
        transmitter: message_transmitters   >>
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
        multispace0                                          >>
        tag!("SIG_GROUP_")                                   >>
        ms                                                   >>
        message_id: message_id                               >>
        ms                                                   >>
        signal_group_name: c_ident                           >>
        ms                                                   >>
        repetitions: u64_s                                   >>
        ms                                                   >>
        colon                                                >>
        ms                                                   >>
        signal_names: separated_list!(ms, c_ident)           >>
        semi_colon                                           >>
        eol                                                  >>
        (SignalGroups{
            message_id: message_id,
            signal_group_name: signal_group_name,
            repetitions: repetitions,
            signal_names: signal_names,
        })
    )
);

named!(pub dbc<CompleteByteSlice, DBC>,
    do_parse!(
        version:                         version                                 >>
        new_symbols:                     new_symbols                             >>
        bit_timing:                      opt!(bit_timing)                        >>
        nodes:                           many0!(node)                            >>
        value_tables:                    many0!(value_table)                     >>
        messages:                        many0!(message)                         >>
        message_transmitters:            many0!(message_transmitter)             >>
        environment_variables:           many0!(environment_variable)            >>
        environment_variable_data:       many0!(environment_variable_data)       >>
        signal_types:                    many0!(signal_type)                     >>
        comments:                        many0!(comment)                         >>
        attribute_definitions:           many0!(attribute_definition)            >>
        attribute_defaults:              many0!(attribute_default)               >>
        attribute_values:                many0!(attribute_value_for_object)      >>
        value_descriptions:              many0!(value_descriptions)              >>
        signal_type_refs:                many0!(signal_type_ref)                 >>
        signal_groups:                   many0!(signal_groups)                   >>
        signal_extended_value_type_list: many0!(signal_extended_value_type_list) >>
        multispace0                                                              >>
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
            signal_types: signal_types,
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
