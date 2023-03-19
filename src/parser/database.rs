//! Allows customization of the commands list at runtime in order to facilitate forwards-compatibility.

use std::collections::HashSet;

pub const NULAR_COMMANDS: &[&'static str] = &include!("./database/nular_commands.jsonc");
pub const UNARY_COMMANDS: &[&'static str] = &include!("./database/unary_commands.jsonc");
pub const BINARY_COMMANDS: &[&'static str] = &include!("./database/binary_commands.jsonc");

const BINARY_COMMANDS_SPECIAL: &[&'static str] = &[
  "or", "and", "else", "max", "min", "mod", "atan2"
];

/// Contains a list of most nular, unary, and binary commands.
///
/// Instances of [`Database`] should only contain commands that are alpha-numeric, and commands that do not have special precedence.
/// Non-alpha-numeric commands and commands with special precedence are handled manually by the parser.
pub struct Database {
  pub nular_commands: HashSet<String>,
  pub unary_commands: HashSet<String>,
  pub binary_commands: HashSet<String>
}

impl Database {
  /// An empty database with no entries.
  pub fn new() -> Self {
    Database {
      nular_commands: HashSet::new(),
      unary_commands: HashSet::new(),
      binary_commands: HashSet::new()
    }
  }

  pub fn add_nular_command(&mut self, command: impl AsRef<str>) {
    let command = command.as_ref().to_lowercase();
    if is_valid_command(&command) && command != "true" && command != "false" {
      self.nular_commands.insert(command);
    };
  }

  pub fn add_unary_command(&mut self, command: impl AsRef<str>) {
    let command = command.as_ref().to_lowercase();
    if is_valid_command(&command) {
      self.unary_commands.insert(command);
    };
  }

  pub fn add_binary_command(&mut self, command: impl AsRef<str>) {
    let command = command.as_ref().to_lowercase();
    if is_valid_command(&command) && !BINARY_COMMANDS_SPECIAL.contains(&command.as_str()) {
      self.binary_commands.insert(command);
    };
  }

  pub fn has_nular_command(&self, command: impl AsRef<str>) -> bool {
    self.nular_commands.contains(&command.as_ref().to_lowercase())
  }

  pub fn has_unary_command(&self, command: impl AsRef<str>) -> bool {
    self.unary_commands.contains(&command.as_ref().to_lowercase())
  }

  pub fn has_binary_command(&self, command: impl AsRef<str>) -> bool {
    self.binary_commands.contains(&command.as_ref().to_lowercase())
  }

  pub fn has_command(&self, command: impl AsRef<str>) -> bool {
    let command = command.as_ref().to_lowercase();
    self.nular_commands.contains(&command) ||
    self.unary_commands.contains(&command) ||
    self.binary_commands.contains(&command)
  }
}

impl Default for Database {
  fn default() -> Self {
    let mut nular_commands = to_set(NULAR_COMMANDS);
    nular_commands.remove("true");
    nular_commands.remove("false");

    let unary_commands = to_set(UNARY_COMMANDS);

    let mut binary_commands = to_set(BINARY_COMMANDS);
    for &command in BINARY_COMMANDS_SPECIAL {
      binary_commands.remove(command);
    };

    Database {
      nular_commands,
      unary_commands,
      binary_commands
    }
  }
}

pub fn is_special_command(command: &str) -> bool {
  command == "true" || command == "false" ||
  BINARY_COMMANDS_SPECIAL.contains(&command)
}

fn is_valid_command(command: &str) -> bool {
  command.chars().all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
}

fn to_set(commands: &[&str]) -> HashSet<String> {
  commands.into_iter().map(|command| command.to_lowercase()).collect()
}
