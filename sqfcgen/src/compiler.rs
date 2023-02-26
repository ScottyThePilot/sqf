//! Since [`Compiled`]'s names and constants lists can be difficult to manage,
//! this module contains structs that allow for the creation of a sort of intermediate form
//! which can generate these lists automatically.
//!
//! The main entrypoint to this is the [`Statements`] struct, which can be converted to a
//! serializable [`Compiled`] via [`Statements::compile`].
//!
//! You can also construct statements/expressions with (relatively) little pain by using
//! the [`stmt`], [`stmts`] and [`expr`] macros.

use crate::serializer::{Compiled, Constant, Instruction, Instructions, InstructionContent};

/// Helper macro for creating instances of [`Statements`].
#[macro_export]
macro_rules! stmts {
  ($($statement:expr),* $(,)?) => {
    $crate::compiler::Statements::from(vec![$($statement),*])
  };
}

/// Helper macro for creating instances of [`Statement`].
#[macro_export]
macro_rules! stmt {
  (assign_global $name:literal $expression:expr) => (
    $crate::compiler::Statement::AssignGlobal($name.into(), $expression)
  );
  (assign_local $name:literal $expression:expr) => (
    $crate::compiler::Statement::AssignLocal($name.into(), $expression)
  );
  ($($t:tt)*) => (
    $crate::compiler::Statement::Expression($crate::expr!($($t)*))
  );
}

/// Helper macro for creating instances of [`Expression`].
#[macro_export]
macro_rules! expr {
  (code {$($statement:expr),* $(,)?}) => (
    $crate::compiler::Expression::Code($crate::stmts![$($statement,)*])
  );
  (string $string:expr) => (
    $crate::compiler::Expression::String($string.into())
  );
  (number $number:expr) => (
    $crate::compiler::Expression::Number($number)
  );
  (true) => (
    $crate::compiler::Expression::Boolean(true)
  );
  (false) => (
    $crate::compiler::Expression::Boolean(false)
  );
  (array [$($expression:expr),* $(,)?]) => (
    $crate::compiler::Expression::Array(vec![$($expression,)*])
  );
  (nular_constant $command:literal $(())?) => (
    $crate::compiler::Expression::NularCommandConstant($command.into())
  );
  (nular $command:literal $(())?) => (
    $crate::compiler::Expression::NularCommand($command.into())
  );
  (unary $command:literal ($arg:expr $(,)?)) => (
    $crate::compiler::Expression::UnaryCommand($command.into(), Box::new($arg))
  );
  (binary $command:literal ($arg1:expr, $arg2:expr $(,)?)) => (
    $crate::compiler::Expression::BinaryCommand($command.into(), Box::new($arg1), Box::new($arg2))
  );
  (variable $name:literal) => (
    $crate::compiler::Expression::Variable($name.into())
  );
}

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

  /// Converts this statements list into a [`Compiled`].
  /// A file name must be provided for debugging purposes.
  pub fn compile(&self, file_name: &str) -> CompileResult<Compiled> {
    let mut ctx = Context { constants_cache: Vec::new(), names_cache: Vec::new() };
    let entrypoint_code = self.compile_to_instructions(&mut ctx)?;
    let entrypoint_index = ctx.constants_cache.len() as u16;
    ctx.constants_cache.push(Constant::Code(entrypoint_code));
    Ok(Compiled {
      entry_point: entrypoint_index,
      constants_cache_compression: true,
      constants_cache: ctx.constants_cache,
      names_cache: ctx.names_cache,
      file_names: vec![file_name.to_owned()]
    })
  }

  pub fn compile_to_writer(&self, file_name: &str, mut writer: impl std::io::Write) -> Result<(), crate::Error> {
    Ok(self.compile(file_name)?.serialize(&mut writer)?)
  }

  pub(crate) fn compile_to_instructions(&self, ctx: &mut Context) -> CompileResult<Instructions> {
    let mut instructions = Vec::new();
    for statement in self.content.iter() {
      statement.compile_instructions(&mut instructions, ctx)?;
    };

    let source_string_index = ctx.add_constant(Constant::String(self.source.clone()))?;
    Ok(Instructions { contents: instructions, source_string_index })
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

impl Statement {
  pub(crate) fn compile_instructions(&self, instructions: &mut Vec<Instruction>, ctx: &mut Context) -> CompileResult {
    instructions.push(InstructionContent::EndStatement.full());
    match self {
      Self::AssignGlobal(name, expression) => {
        expression.compile_instructions(instructions, ctx)?;
        let name_index = ctx.add_name(name)?;
        instructions.push(InstructionContent::AssignTo(name_index).full());
      },
      Self::AssignLocal(name, expression) => {
        expression.compile_instructions(instructions, ctx)?;
        let name_index = ctx.add_name(name)?;
        instructions.push(InstructionContent::AssignToLocal(name_index).full());
      },
      Self::Expression(expression) => {
        expression.compile_instructions(instructions, ctx)?;
      }
    };

    Ok(())
  }
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

impl Expression {
  pub(crate) fn compile_instructions(&self, instructions: &mut Vec<Instruction>, ctx: &mut Context) -> CompileResult {
    match self.compile_constant(ctx)? {
      Some(constant) => {
        let constant_index = ctx.add_constant(constant)?;
        instructions.push(InstructionContent::Push(constant_index).full());
      },
      None => match self {
        Self::Array(array) => {
          let array_len = array.len().try_into().map_err(|_| CompileError::ListTooLong)?;
          for array_expr in array.into_iter() {
            array_expr.compile_instructions(instructions, ctx)?;
          };

          instructions.push(InstructionContent::MakeArray(array_len).full());
        },
        Self::NularCommand(name) => {
          let name_index = ctx.add_name(name)?;
          instructions.push(InstructionContent::CallNular(name_index).full());
        },
        Self::UnaryCommand(name, expr) => {
          expr.compile_instructions(instructions, ctx)?;
          let name_index = ctx.add_name(name)?;
          instructions.push(InstructionContent::CallUnary(name_index).full());
        },
        Self::BinaryCommand(name, expr1, expr2) => {
          expr1.compile_instructions(instructions, ctx)?;
          expr2.compile_instructions(instructions, ctx)?;
          let name_index = ctx.add_name(name)?;
          instructions.push(InstructionContent::CallBinary(name_index).full());
        },
        Self::Variable(name) => {
          let name_index = ctx.add_name(name)?;
          instructions.push(InstructionContent::GetVariable(name_index).full());
        },
        _ => unreachable!()
      }
    };

    Ok(())
  }

  pub(crate) fn compile_constant(&self, ctx: &mut Context) -> CompileResult<Option<Constant>> {
    Ok(match *self {
      Self::Code(ref statements) => {
        Some(Constant::Code(statements.compile_to_instructions(ctx)?))
      },
      Self::String(ref string) => {
        Some(Constant::String(string.clone()))
      },
      Self::Number(number) => {
        Some(Constant::Scalar(number))
      },
      Self::Boolean(boolean) => {
        Some(Constant::Boolean(boolean))
      },
      Self::Array(ref array) => {
        array.iter()
          .map(|value| value.clone().compile_constant(ctx))
          .collect::<CompileResult<Option<Vec<Constant>>>>()?
          .map(Constant::Array)
      },
      Self::NularCommandConstant(ref name) => {
        Some(Constant::NularCommand(name.to_lowercase()))
      },
      _ => None
    })
  }
}

#[derive(Debug, Error)]
pub enum CompileError {
  #[error("cannot convert list longer than 2^16 elements")]
  ListTooLong
}

type CompileResult<T = ()> = Result<T, CompileError>;

#[derive(Debug)]
pub(crate) struct Context {
  constants_cache: Vec<Constant>,
  names_cache: Vec<String>
}

impl Context {
  pub(crate) fn add_constant(&mut self, constant: Constant) -> CompileResult<u16> {
    add_or_get_index(&mut self.constants_cache, constant)
  }

  pub(crate) fn add_name(&mut self, name: impl AsRef<str>) -> CompileResult<u16> {
    add_or_get_index(&mut self.names_cache, name.as_ref().to_lowercase())
  }
}

fn add_or_get_index<T: PartialEq>(collection: &mut Vec<T>, value: T) -> CompileResult<u16> {
  collection.iter()
    .position(|item| item == &value)
    .unwrap_or_else(|| {
      let value_index = collection.len();
      collection.push(value);
      value_index
    })
    .try_into()
    .map_err(|_| CompileError::ListTooLong)
}
