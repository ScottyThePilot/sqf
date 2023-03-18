use super::{Compiled, Constant, Instructions, InstructionContent};

use std::fmt;

#[inline]
fn indent(f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
  (0..indent).try_for_each(|_| f.write_str("  "))
}

#[derive(Debug, Clone, Copy)]
pub struct DisplayInstructions<'a> {
  pub(super) compiled: &'a Compiled,
  pub(super) instructions: &'a Instructions,
  pub(super) indent: usize
}

impl<'a> fmt::Display for DisplayInstructions<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    for instruction in &self.instructions.contents {
      indent(f, self.indent)?;
      f.write_str(instruction.content.name())?;

      match instruction.content {
        InstructionContent::EndStatement => (),
        InstructionContent::Push(constant) => {
          f.write_str(" ")?;
          let constant = &self.compiled.constants_cache[constant as usize];
          fmt::Display::fmt(&DisplayConstant {
            compiled: self.compiled, constant, indent: self.indent
          }, f)?;
        },
        InstructionContent::CallUnary(name) |
        InstructionContent::CallBinary(name) |
        InstructionContent::CallNular(name) |
        InstructionContent::AssignTo(name) |
        InstructionContent::AssignToLocal(name) |
        InstructionContent::GetVariable(name) => {
          let name = &self.compiled.names_cache[name as usize];
          let offset = instruction.source_info.offset;
          write!(f, " {name} ({offset})")?;
        },
        InstructionContent::MakeArray(array_len) => {
          let offset = instruction.source_info.offset;
          write!(f, " {array_len} ({offset})")?;
        }
      };

      writeln!(f)?;
    };

    Ok(())
  }
}

#[derive(Debug, Clone, Copy)]
pub struct DisplayConstant<'a> {
  pub(super) compiled: &'a Compiled,
  pub(super) constant: &'a Constant,
  pub(super) indent: usize
}

impl<'a> fmt::Display for DisplayConstant<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self.constant {
      Constant::NularCommand(command) => write!(f, "Nular {command}")?,
      Constant::Code(instructions) => {
        f.write_str("{\n")?;
        fmt::Display::fmt(&DisplayInstructions {
          compiled: self.compiled, instructions, indent: self.indent + 1
        }, f)?;
        indent(f, self.indent)?;
        f.write_str("}")?;
      },
      Constant::String(string) => write!(f, "{string:?}")?,
      Constant::Scalar(scalar) => write!(f, "{scalar:?}")?,
      Constant::Boolean(boolean) => write!(f, "{boolean}")?,
      Constant::Array(array) => {
        f.write_str("[")?;
        for (i, constant) in array.iter().enumerate() {
          if i != 0 { f.write_str(", ")? };
          fmt::Display::fmt(&DisplayConstant {
            compiled: self.compiled, constant, indent: self.indent
          }, f)?;
        };
        f.write_str("]")?;
      }
    };

    Ok(())
  }
}
