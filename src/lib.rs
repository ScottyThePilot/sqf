#[cfg(feature = "compiler")]
pub mod compiler;
#[cfg(feature = "parser")]
pub mod parser;

mod misc;

#[macro_use]
extern crate thiserror;
#[cfg(feature = "compiler")]
extern crate byteorder;
#[cfg(feature = "parser")]
extern crate chumsky;
extern crate float_ord;
#[cfg(feature = "compiler")]
extern crate lzo;

#[doc(no_inline)]
pub use float_ord::FloatOrd as Scalar;



#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Statements {
  pub content: Vec<Statement>,
  /// The source code string of this section of code.
  /// This isn't required to actually be anything significant, but will be displayed in-game if a script error occurs.
  pub source: String
}

impl Statements {
  /// Adds a source string to this code chunk.
  pub fn with_source(self, source: String) -> Self {
    Statements { content: self.content, source }
  }
}

impl From<Vec<Statement>> for Statements {
  fn from(content: Vec<Statement>) -> Self {
    Statements { content, source: String::new() }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Statement {
  AssignGlobal(String, Expression),
  AssignLocal(String, Expression),
  Expression(Expression)
}

impl From<Expression> for Statement {
  fn from(expression: Expression) -> Self {
    Statement::Expression(expression)
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expression {
  Code(Statements),
  String(String),
  Number(Scalar<f32>),
  Boolean(bool),
  Array(Vec<Self>),
  NularCommand(NularCommand),
  UnaryCommand(UnaryCommand, Box<Self>),
  BinaryCommand(BinaryCommand, Box<Self>, Box<Self>),
  Variable(String)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NularCommand {
  /// A named command.
  Named(String)
}

impl NularCommand {
  pub fn is_constant(&self) -> bool {
    // ArmaScriptCompiler contains code for serializing so-called "nular command constants"
    // which are stored in the constants table just like numbers, strings and booleans rather than
    // being called at runtime, however ArmaScriptCompiler doesn't ever seem to emit these

    // TODO: actually determine whether something is a nular constant or not
    false
  }

  pub fn to_str(&self) -> &str {
    match self {
      Self::Named(name) => name
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnaryCommand {
  /// A named command.
  /// Non-alphanumeric commands (such as `==` or `!`) or commands with special precedence should not go here.
  Named(String)
}

impl UnaryCommand {
  pub fn to_str(&self) -> &str {
    match self {
      Self::Named(name) => name
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinaryCommand {
  /// A named command.
  /// Non-alphanumeric commands (such as `==` or `!`) or commands with special precedence should not go here.
  Named(String)
}

impl BinaryCommand {
  pub fn to_str(&self) -> &str {
    match self {
      Self::Named(name) => name
    }
  }
}

#[derive(Debug, Error)]
pub enum Error {
  #[error(transparent)]
  CompileError(#[from] crate::compiler::CompileError),
  #[error(transparent)]
  SerializeError(#[from] crate::compiler::serializer::SerializeError)
}

pub(crate) mod sealed {
  pub trait Sealed {}
}
