use serde::Deserialize;

#[derive(Deserialize, Debug)]
pub struct Config {
    pub name: String,
    pub include: Vec<String>,
}

pub fn get_config() -> Config {
    let file = std::fs::read_to_string("./crust.toml").unwrap();
    toml::from_str(file.as_str()).unwrap()
}
