use sqfcgen::{stmt, stmts, expr};
use sqfcgen::serializer::{Compiled, Constant};

/// This mess is the AST form or intermediate form of the following SQF code:
/// ```sqf
/// params ["_args", "_targets", ["_jip", false]];
/// [_args, { hintSilent parseText _this }] remoteExecCall ["call", _targets, _jip];
/// ```
///
/// This example serializes it into a fully functioning SQFC script.
fn main() {
  let code = stmts![
    stmt!(unary "params" (
      expr!(array [
        expr!(string "_args"),
        expr!(string "_targets"),
        expr!(array [
          expr!(string "_jip"),
          expr!(false)
        ])
      ])
    )),
    stmt!(binary "remoteExecCall" (
      expr!(array [
        expr!(variable "_args"),
        expr!(code {
          stmt!(unary "hintSilent" (
            expr!(unary "parseText" (
              expr!(variable "_this")
            ))
          ))
        })
      ]),
      expr!(array [
        expr!(string "call"),
        expr!(variable "_targets"),
        expr!(variable "_jip")
      ])
    ))
  ];

  let mut compiled = code.compile("hintRemote.sqfc").unwrap();
  let mut serialize_buffer: Vec<u8> = Vec::new();
  add_debug_offsets_to_compiled(&mut compiled);
  compiled.serialize(&mut serialize_buffer).unwrap();
  std::fs::write("hintRemote.sqfc", serialize_buffer).unwrap();
  println!("{}", compiled.print());
}

fn add_debug_offsets_to_compiled(ctx: &mut Compiled) {
  let mut counter: usize = 0;
  for constant in ctx.constants_cache.iter_mut() {
    if let Constant::Code(code) = constant {
      for instruction in code.contents.iter_mut() {
        instruction.source_info.offset = counter as u32;
        instruction.source_info.file_line = counter as u16;
        counter += 1;
      };
    };
  };
}
