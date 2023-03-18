#[cfg(feature = "compiler")]
extern crate byteorder;
#[cfg(feature = "compiler")]
extern crate lzo;
#[macro_use]
extern crate thiserror;

#[cfg(feature = "compiler")]
pub mod compiler;


#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
  Code(Statements),
  String(String),
  Number(f32),
  Boolean(bool),
  Array(Vec<Self>),
  NularCommandConstant(String),
  NularCommand(String),
  UnaryCommand(String, Box<Self>),
  BinaryCommand(String, Box<Self>, Box<Self>),
  Variable(String)
}

#[derive(Debug, Error)]
pub enum Error {
  #[error(transparent)]
  CompileError(#[from] crate::compiler::CompileError),
  #[error(transparent)]
  SerializeError(#[from] crate::compiler::serializer::SerializeError)
}
