//! Since [`Compiled`]'s names and constants lists can be difficult to manage,
//! this module contains structs that allow for the creation of a sort of intermediate form
//! which can generate these lists automatically.
//!
//! The main entrypoint to this is the [`Statements`][crate::Statements] struct, which can be
//! converted to a serializable [`Compiled`] via [`Statements::compile`][crate::Statements].

pub mod serializer;

use crate::{Expression, Statements, Statement};
use self::serializer::{Compiled, Constant, Instruction, Instructions};

impl Statements {
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

impl Statement {
  pub(crate) fn compile_instructions(&self, instructions: &mut Vec<Instruction>, ctx: &mut Context) -> CompileResult {
    instructions.push(Instruction::EndStatement);
    match *self {
      Self::AssignGlobal(ref name, ref expression, location) => {
        expression.compile_instructions(instructions, ctx)?;
        let name_index = ctx.add_name(name)?;
        instructions.push(Instruction::AssignTo(name_index, location.into()));
      },
      Self::AssignLocal(ref name, ref expression, location) => {
        expression.compile_instructions(instructions, ctx)?;
        let name_index = ctx.add_name(name)?;
        instructions.push(Instruction::AssignToLocal(name_index, location.into()));
      },
      Self::Expression(ref expression) => {
        expression.compile_instructions(instructions, ctx)?;
      }
    };

    Ok(())
  }
}

impl Expression {
  pub(crate) fn compile_instructions(&self, instructions: &mut Vec<Instruction>, ctx: &mut Context) -> CompileResult {
    match self.compile_constant(ctx)? {
      Some(constant) => {
        let constant_index = ctx.add_constant(constant)?;
        instructions.push(Instruction::Push(constant_index));
      },
      None => match *self {
        Self::Array(ref array, location) => {
          let array_len = array.len().try_into().map_err(|_| CompileError::ListTooLong)?;
          for array_expr in array.into_iter() {
            array_expr.compile_instructions(instructions, ctx)?;
          };

          instructions.push(Instruction::MakeArray(array_len, location.into()));
        },
        Self::NularCommand(ref command, location) => {
          let name_index = ctx.add_name(command.as_str())?;
          instructions.push(Instruction::CallNular(name_index, location.into()));
        },
        Self::UnaryCommand(ref command, ref expr, location) => {
          expr.compile_instructions(instructions, ctx)?;
          let name_index = ctx.add_name(command.as_str())?;
          instructions.push(Instruction::CallUnary(name_index, location.into()));
        },
        Self::BinaryCommand(ref command, ref expr1, ref expr2, location) => {
          expr1.compile_instructions(instructions, ctx)?;
          expr2.compile_instructions(instructions, ctx)?;
          let name_index = ctx.add_name(command.as_str())?;
          instructions.push(Instruction::CallBinary(name_index, location.into()));
        },
        Self::Variable(ref name, location) => {
          let name_index = ctx.add_name(name)?;
          instructions.push(Instruction::GetVariable(name_index, location.into()));
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
      Self::Number(crate::Scalar(number)) => {
        Some(Constant::Scalar(number))
      },
      Self::Boolean(boolean) => {
        Some(Constant::Boolean(boolean))
      },
      Self::Array(ref array, ..) => {
        array.iter()
          .map(|value| value.clone().compile_constant(ctx))
          .collect::<CompileResult<Option<Vec<Constant>>>>()?
          .map(Constant::Array)
      },
      Self::NularCommand(ref command, ..) if command.is_constant() => {
        let command = try_normalize_name(&command.name)?;
        debug_assert_ne!(command, "true", "do not provide `true` as a nular constant command");
        debug_assert_ne!(command, "false", "do not provide `false` as a nular constant command");
        Some(Constant::NularCommand(command))
      },
      _ => None
    })
  }
}

#[derive(Debug, Error)]
pub enum CompileError {
  #[error("cannot convert list longer than 2^16 elements")]
  ListTooLong,
  #[error("invalid name {0}")]
  InvalidName(String)
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

  pub(crate) fn add_name(&mut self, name: &str) -> CompileResult<u16> {
    add_or_get_index(&mut self.names_cache, try_normalize_name(name)?)
  }
}

fn try_normalize_name(name: &str) -> CompileResult<String> {
  let name_lower = name.to_ascii_lowercase();
  if crate::parser::database::is_valid_command(&name_lower) {
    Ok(name_lower)
  } else {
    Err(CompileError::InvalidName(name.to_owned()))
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
