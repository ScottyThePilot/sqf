use sqf::parser::database::Database;

use std::env::args_os;
use std::fs::{self, File};
use std::io::{BufWriter, Write};
use std::path::PathBuf;

fn main() {
  let database = Database::default();
  for path in args_os().skip(1) {
    let path = PathBuf::from(path);
    let file_name = path.file_name().expect("invalid path").to_string_lossy();
    let file = fs::read_to_string(&path).expect("failed to read file");
    let statements = sqf::parser::run(&database, &file).expect("failed to parse file");

    let compiled = statements.compile(file_name.as_ref()).expect("failed to compile file");
    let serialized_out_file = File::create(path.with_extension("sqfc")).expect("failed to create output file");
    let mut serialized_out = BufWriter::new(serialized_out_file);
    compiled.serialize(&mut serialized_out).expect("failed to serialize file");
    serialized_out.flush().expect("failed to write to output file");
  };
}
