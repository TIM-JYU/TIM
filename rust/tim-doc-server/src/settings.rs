use config::Environment;
use config::{Config, ConfigError};
use serde_derive::Deserialize;

#[derive(Debug, Deserialize)]
pub struct Settings {
    pub psql_address: String,
    pub bind: String,
}

impl Settings {
    pub fn new() -> Result<Self, ConfigError> {
        let mut s = Config::new();
        s.merge(Environment::with_prefix("tim"))?;
        s.set_default(
            "psql_address",
            "postgresql://postgres@192.168.99.100:5432/tim",
        )?;
        s.set_default("bind", "127.0.0.1:80")?;
        s.try_into()
    }
}
