use super::{Compiled, Constant, Instruction, InstructionContent};

use std::fmt::Write;

impl Compiled {
  /// Pretty-prints the contained instructions.
  pub fn print(&self) -> String {
    fn instruction_name(instruction: InstructionContent) -> &'static str {
      match instruction {
        InstructionContent::EndStatement => "EndStatement",
        InstructionContent::Push(..) => "Push",
        InstructionContent::CallUnary(..) => "CallUnary",
        InstructionContent::CallBinary(..) => "CallBinary",
        InstructionContent::CallNular(..) => "CallNular",
        InstructionContent::AssignTo(..) => "AssignTo",
        InstructionContent::AssignToLocal(..) => "AssignToLocal",
        InstructionContent::GetVariable(..) => "GetVariable",
        InstructionContent::MakeArray(..) => "MakeArray"
      }
    }

    fn print_indent(buffer: &mut String, indent: usize) {
      for _ in 0..indent { buffer.push_str("  ") };
    }

    fn print_code(buffer: &mut String, compiled: &Compiled, code: &[Instruction], indent: usize) {
      for instruction in code {
        print_indent(buffer, indent);

        buffer.push_str(instruction_name(instruction.content));

        match instruction.content {
          InstructionContent::EndStatement => (),
          InstructionContent::Push(constant) => {
            buffer.push(' ');
            let constant = &compiled.constants_cache[constant as usize];
            print_constant(buffer, compiled, constant, indent);
          },
          InstructionContent::CallUnary(name) |
          InstructionContent::CallBinary(name) |
          InstructionContent::CallNular(name) |
          InstructionContent::AssignTo(name) |
          InstructionContent::AssignToLocal(name) |
          InstructionContent::GetVariable(name) => {
            let name = &compiled.names_cache[name as usize];
            let offset = instruction.source_info.offset;
            write!(buffer, " {name} ({offset})").unwrap();
          },
          InstructionContent::MakeArray(array_len) => {
            let offset = instruction.source_info.offset;
            write!(buffer, " {array_len} ({offset})").unwrap();
          }
        };

        buffer.push('\n');
      };
    }

    fn print_constant(buffer: &mut String, compiled: &Compiled, constant: &Constant, indent: usize) {
      match constant {
        Constant::NularCommand(command) => write!(buffer, "Nular {command}").unwrap(),
        Constant::Code(code) => {
          buffer.push_str("{\n");
          print_code(buffer, compiled, &code.contents, indent + 1);
          print_indent(buffer, indent);
          buffer.push_str("}");
        },
        Constant::String(string) => write!(buffer, "{string:?}").unwrap(),
        Constant::Scalar(scalar) => write!(buffer, "{scalar:?}").unwrap(),
        Constant::Boolean(boolean) => write!(buffer, "{boolean}").unwrap(),
        Constant::Array(array) => {
          buffer.push('[');
          for (i, array_constant) in array.iter().enumerate() {
            if i != 0 { buffer.push_str(", ") };
            print_constant(buffer, compiled, array_constant, indent);
          };
          buffer.push_str("]");
        }
      }
    }

    let mut buffer = String::new();
    let entry_point = self.get_entry_point().unwrap();
    print_code(&mut buffer, self, &entry_point.contents, 0);
    if buffer.ends_with('\n') { buffer.pop(); };
    buffer
  }
}
