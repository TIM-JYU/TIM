use snafu::{ResultExt, Snafu};
use std::convert::TryFrom;
use yaml_rust::{Yaml, YamlLoader, ScanError};

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
    InvalidYaml {
        source: ScanError,
    },
    #[snafu(display("YAML is empty"))]
    EmptyYaml {
    },
}

impl TryFrom<&str> for YamlBlock {
    type Error = YamlError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut r =
            YamlLoader::load_from_str(&correct_yaml(value)).context(InvalidYaml {})?;
        let mut drain = r.drain(..);
        Ok(YamlBlock {
            yaml: drain.next().ok_or(YamlError::EmptyYaml {})?,
        })
    }
}

#[cfg(test)]
mod tests {
    use std::convert::TryFrom;
    use yaml_rust::YamlLoader;
    use pest::Parser;
    use pest::parses_to;
    use pest::consumes_to;
    use super::*;

    fn check_same_yaml(std_yaml: &str, tim_yaml: &str) {
        assert_eq!(
            YamlLoader::load_from_str(std_yaml).unwrap()[0],
            YamlBlock::try_from(tim_yaml).unwrap().yaml
        );
    }

    fn print(input: &str) {
        let s1 = YamlParser::parse(
            Rule::yaml, input).unwrap();
        println!("{:#?}", s1);
        println!("{}", s1);
    }

    #[test]
    fn flow_list() {
        print(r#"a: [
        x,
        "y",
        'z[',
        ]"#);
    }

    #[test]
    fn list() {
        print(r#"
a:
- 1
- 2
-
        "#);
    }

    #[test]
    fn weird_str() {
        print(r#"
a:
  xx
 yy
        "#);
    }

    #[test]
    fn comments() {
        parses_to!(
            parser: YamlParser,

            input: r#"# test
t: # start of t
#something
 q: z"#,
rule: Rule::yaml,
            tokens: [obj(7, 39,
                    [keyvalue(7, 39, [key(7, 8), obj(34, 39,
                                                [keyvalue(35, 39, [key(35, 36), unquoted_str(38, 39)])])])]), EOI(39, 39)]
        );
    }

    #[test]
    fn asdasd() {
//        let f1 = YamlParser::parse(Rule::yaml, "2");
//        println!("{:#?}", f1);

        parses_to!(
            parser: YamlParser,

            input: r#"
t:
 q: z
 x: w
f: g"#,
rule: Rule::yaml,
            tokens: [obj(1, 20,
            [keyvalue(1, 15,
                     [key(1, 2), obj(4, 15,
                                     [keyvalue(5, 9, [key(5, 6), unquoted_str(8, 9)]),
                                      keyvalue(11, 15, [key(11, 12), unquoted_str(14, 15)])])]),
             keyvalue(16, 20, [key(16, 17), unquoted_str(19, 20)])]), EOI(20, 20)]
        );

        parses_to!(
            parser: YamlParser,
            input: r#"
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
            rule: Rule::yaml,
            tokens: [obj(1, 58,
            [keyvalue(1, 58, [key(1, 2), obj(4, 58,
                                             [keyvalue(5, 9, [key(5, 6), unquoted_str(8, 9)]),
                                              keyvalue(11, 22, [key(11, 12), standard_yaml_multiline_str(14, 22, [multiline_str_line(19, 22)])]),
                                              keyvalue(24, 36, [key(24, 25), standard_yaml_multiline_str(27, 36, [multiline_str_line(32, 36)])]),
                                              keyvalue(38, 58, [key(38, 39), obj(41, 58,
                                                                                [keyvalue(45, 49, [key(45, 46), unquoted_str(48, 49)]),
                                                                                 keyvalue(54, 58, [key(54, 55), unquoted_str(57, 58)])])])])])]), EOI(59, 59)]
        );

        parses_to!(
            parser: YamlParser,
            input: r#"a: b
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
            rule: Rule::yaml,
            tokens: [obj(0, 81,
            [keyvalue(0, 4, [key(0, 1), unquoted_str(3, 4)]),
             keyvalue(5, 9, [key(5, 6), unquoted_str(8, 9)]),
             keyvalue(10, 22, [key(10, 11), standard_yaml_multiline_str(13, 22, [multiline_str_line(16, 18), multiline_str_line(20, 22)])]),
             keyvalue(23, 36, [key(23, 24), standard_yaml_multiline_str(26, 36, [multiline_str_line(30, 32), multiline_str_line(34, 36)])]),
             keyvalue(37, 45, [key(37, 38), obj(40, 45, [keyvalue(41, 45, [key(41, 42), unquoted_str(44, 45)])])]),
             keyvalue(46, 50, [key(46, 47), unquoted_str(49, 50)]),
             keyvalue(51, 81, [key(51, 57), tim_multiline_str(59, 81, [multiline_str_line(60, 62), tim_multiline_str_content(63, 79, [multiline_str_line(63, 68), multiline_str_line(69, 78)])])])]), EOI(82, 82)]
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
