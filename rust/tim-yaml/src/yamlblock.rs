use snafu::{ResultExt, Snafu};
use std::convert::TryFrom;
use yaml_rust::{ScanError, Yaml, YamlLoader};

#[derive(Debug)]
struct YamlBlock {
    pub yaml: Yaml,
}

#[derive(Parser)]
#[grammar = "timyaml.pest"]
pub struct YamlParser;

// TODO: return Cow type
fn correct_yaml(s: &str) -> String {
    s.into()
}

#[derive(Debug, Snafu)]
enum YamlError {
    #[snafu(display("YAML is invalid"))]
    InvalidYaml { source: ScanError },
    #[snafu(display("YAML is empty"))]
    EmptyYaml {},
}

impl TryFrom<&str> for YamlBlock {
    type Error = YamlError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut r = YamlLoader::load_from_str(&correct_yaml(value)).context(InvalidYaml {})?;
        let mut drain = r.drain(..);
        Ok(YamlBlock {
            yaml: drain.next().ok_or(YamlError::EmptyYaml {})?,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pest::consumes_to;
    use pest::parses_to;
    use pest::Parser;
    use std::convert::TryFrom;
    use yaml_rust::YamlLoader;

    fn check_same_yaml(std_yaml: &str, tim_yaml: &str) {
        assert_eq!(
            YamlLoader::load_from_str(std_yaml).unwrap()[0],
            YamlBlock::try_from(tim_yaml).unwrap().yaml
        );
    }

    fn print(input: &str) {
        let s1 = YamlParser::parse(Rule::yaml, input).unwrap();
        println!("{:#?}", s1);
        println!("{}", s1);
    }

    macro_rules! check {
        ($string:expr, [ $( $names:ident $calls:tt ),* $(,)* ] ) => {
            parses_to!(
                parser: YamlParser,
                input: $string,
                rule: Rule::yaml,
                tokens: [ $( $names $calls ),* ]
            )
        }
    }

    macro_rules! checkp {
        ($string:expr, [ $( $names:ident $calls:tt ),* $(,)* ] ) => {
            print($string);
        };
    }

    #[test]
    fn flow_obj() {
        check!(
            r#"a: {}"#,
            [
                obj(0, 5, [keyvalue(0, 5, [unquoted_str(0, 1), flow_obj(3, 5)])]),
                EOI(5, 5)
            ]
        );
        check!(
            r#"x: {a: b, 'c': d, 'ee': "f"}"#,
            [
                obj(
                    0,
                    28,
                    [keyvalue(
                        0,
                        28,
                        [
                            unquoted_str(0, 1),
                            flow_obj(
                                3,
                                28,
                                [
                                    flow_obj_element(
                                        4,
                                        8,
                                        [flow_unquoted_str(4, 5), flow_unquoted_str(7, 8)]
                                    ),
                                    flow_obj_element(
                                        10,
                                        16,
                                        [single_quoted_str(10, 13), flow_unquoted_str(15, 16)]
                                    ),
                                    flow_obj_element(
                                        18,
                                        27,
                                        [single_quoted_str(18, 22), double_quoted_str(24, 27)]
                                    )
                                ]
                            )
                        ]
                    )]
                ),
                EOI(28, 28)
            ]
        );
    }

    #[test]
    fn flow_list() {
        check!(
            r#"a: [
        x,
        "y",
        'z[',
        ]"#,
            [
                obj(
                    0,
                    52,
                    [keyvalue(
                        0,
                        52,
                        [
                            unquoted_str(0, 1),
                            flow_list(
                                3,
                                52,
                                [
                                    flow_unquoted_str(13, 14),
                                    double_quoted_str(24, 27),
                                    single_quoted_str(37, 41)
                                ]
                            )
                        ]
                    )]
                ),
                EOI(52, 52)
            ]
        );
    }

    #[test]
    fn list() {
        check!(
            r#"
a:
- 1
- 2
-
        "#,
            [
                obj(
                    1,
                    13,
                    [keyvalue(
                        1,
                        13,
                        [
                            unquoted_str(1, 2),
                            list(
                                4,
                                13,
                                [
                                    list_element(4, 7, [number(6, 7)]),
                                    list_element(8, 11, [number(10, 11)]),
                                    list_element(12, 13, [null(13, 13)])
                                ]
                            )
                        ]
                    )]
                ),
                EOI(22, 22)
            ]
        );
    }

    #[test]
    fn weird_str() {
        check!(
            r#"
a:
  xx
 yy
        "#,
            [
                obj(
                    1,
                    12,
                    [keyvalue(1, 12, [unquoted_str(1, 2), unquoted_str(4, 12)])]
                ),
                EOI(21, 21)
            ]
        );
    }

    #[test]
    fn comments() {
        check!(
            r#"# test
t: # start of t
#something
 q: z"#,
            [
                obj(
                    7,
                    39,
                    [keyvalue(
                        7,
                        39,
                        [
                            unquoted_str(7, 8),
                            obj(
                                34,
                                39,
                                [keyvalue(
                                    35,
                                    39,
                                    [unquoted_str(35, 36), unquoted_str(38, 39)]
                                )]
                            )
                        ]
                    )]
                ),
                EOI(39, 39)
            ]
        );
    }

    #[test]
    fn child_obj() {
        //        let f1 = YamlParser::parse(Rule::yaml, "2");
        //        println!("{:#?}", f1);

        check!(
            r#"
t:
 q: z
 x: w
f: g"#,
            [
                obj(
                    1,
                    20,
                    [
                        keyvalue(
                            1,
                            15,
                            [
                                unquoted_str(1, 2),
                                obj(
                                    4,
                                    15,
                                    [
                                        keyvalue(5, 9, [unquoted_str(5, 6), unquoted_str(8, 9)]),
                                        keyvalue(
                                            11,
                                            15,
                                            [unquoted_str(11, 12), unquoted_str(14, 15)]
                                        )
                                    ]
                                )
                            ]
                        ),
                        keyvalue(16, 20, [unquoted_str(16, 17), unquoted_str(19, 20)])
                    ]
                ),
                EOI(20, 20)
            ]
        );
    }

    #[test]
    fn indent_indicator() {
        check!(
            r#"
g:
 x: y
 z: |
   xxx
 d: |1
   yyy
 w:
    u: i
    g: h
"#,
            [
                obj(
                    1,
                    58,
                    [keyvalue(
                        1,
                        58,
                        [
                            unquoted_str(1, 2),
                            obj(
                                4,
                                58,
                                [
                                    keyvalue(5, 9, [unquoted_str(5, 6), unquoted_str(8, 9)]),
                                    keyvalue(
                                        11,
                                        22,
                                        [
                                            unquoted_str(11, 12),
                                            standard_yaml_multiline_str(
                                                14,
                                                22,
                                                [multiline_str_line(19, 22)]
                                            )
                                        ]
                                    ),
                                    keyvalue(
                                        24,
                                        36,
                                        [
                                            unquoted_str(24, 25),
                                            standard_yaml_multiline_str(
                                                27,
                                                36,
                                                [multiline_str_line(32, 36)]
                                            )
                                        ]
                                    ),
                                    keyvalue(
                                        38,
                                        58,
                                        [
                                            unquoted_str(38, 39),
                                            obj(
                                                41,
                                                58,
                                                [
                                                    keyvalue(
                                                        45,
                                                        49,
                                                        [
                                                            unquoted_str(45, 46),
                                                            unquoted_str(48, 49)
                                                        ]
                                                    ),
                                                    keyvalue(
                                                        54,
                                                        58,
                                                        [
                                                            unquoted_str(54, 55),
                                                            unquoted_str(57, 58)
                                                        ]
                                                    )
                                                ]
                                            )
                                        ]
                                    )
                                ]
                            )
                        ]
                    )]
                ),
                EOI(59, 59)
            ]
        );
    }

    #[test]
    fn tim_multiline_str() {
        check!(
            r#"a: b
c: d
e: |
 xx
 zz
q: |+
 xx
 zz
t:
 q: z
f: g
timstr: |!!
x: hi
ff: hello
!!
"#,
            [
                obj(
                    0,
                    81,
                    [
                        keyvalue(0, 4, [unquoted_str(0, 1), unquoted_str(3, 4)]),
                        keyvalue(5, 9, [unquoted_str(5, 6), unquoted_str(8, 9)]),
                        keyvalue(
                            10,
                            22,
                            [
                                unquoted_str(10, 11),
                                standard_yaml_multiline_str(
                                    13,
                                    22,
                                    [multiline_str_line(16, 18), multiline_str_line(20, 22)]
                                )
                            ]
                        ),
                        keyvalue(
                            23,
                            36,
                            [
                                unquoted_str(23, 24),
                                standard_yaml_multiline_str(
                                    26,
                                    36,
                                    [multiline_str_line(30, 32), multiline_str_line(34, 36)]
                                )
                            ]
                        ),
                        keyvalue(
                            37,
                            45,
                            [
                                unquoted_str(37, 38),
                                obj(
                                    40,
                                    45,
                                    [keyvalue(
                                        41,
                                        45,
                                        [unquoted_str(41, 42), unquoted_str(44, 45)]
                                    )]
                                )
                            ]
                        ),
                        keyvalue(46, 50, [unquoted_str(46, 47), unquoted_str(49, 50)]),
                        keyvalue(
                            51,
                            81,
                            [
                                unquoted_str(51, 57),
                                tim_multiline_str(
                                    59,
                                    81,
                                    [
                                        multiline_str_line(60, 62),
                                        tim_multiline_str_content(
                                            63,
                                            79,
                                            [
                                                multiline_str_line(63, 68),
                                                multiline_str_line(69, 78)
                                            ]
                                        )
                                    ]
                                )
                            ]
                        )
                    ]
                ),
                EOI(82, 82)
            ]
        );
    }

    #[test]
    fn standard_yaml_works_the_same() {
        for y in &[
            "css: !",
            "css:",
            "css: ",
            " css:",
            " css: ",
            "css: |+\n a\n b",
            "css: |+\n a\n b\n",
            "css: |-\n a\n b",
            "css: |-\n a\n b\n",
        ] {
            check_same_yaml(y, y);
        }
    }
}
