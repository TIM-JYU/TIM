use crate::timerror::{TimError, TimErrorKind};
use failure::ResultExt;
use std::convert::TryFrom;
use yaml_rust::{Yaml, YamlLoader};

#[derive(Debug)]
struct YamlBlock {
    pub yaml: Yaml,
}

// TODO: return Cow type
fn correct_yaml(s: &str) -> String {
    s.into()
}

impl TryFrom<&str> for YamlBlock {
    type Error = TimError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut r =
            YamlLoader::load_from_str(&correct_yaml(value)).context(TimErrorKind::InvalidYaml)?;
        let mut drain = r.drain(..);
        Ok(YamlBlock {
            yaml: drain.next().ok_or(TimErrorKind::InvalidYaml)?,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::document::yamlblock::YamlBlock;
    use std::convert::TryFrom;
    use yaml_rust::YamlLoader;

    fn check_same_yaml(std_yaml: &str, tim_yaml: &str) {
        assert_eq!(
            YamlLoader::load_from_str(std_yaml).unwrap()[0],
            YamlBlock::try_from(tim_yaml).unwrap().yaml
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
