use sqf::parser::database::Database;

use std::env::args_os;
use std::fs::{self, File};
use std::io::BufWriter;
use std::path::PathBuf;

fn main() {
  let database = Database::default();
  for path in args_os().skip(1) {
    let path = PathBuf::from(path);
    let file_name = path.file_name().expect("invalid path").to_string_lossy();
    let file = fs::read_to_string(&path).expect("failed to read file");
    let statements = sqf::parser::run(&database, &file).expect("failed to parse file");

    let out = BufWriter::new(File::create(path.with_extension("sqfc")).expect("failed to create output file"));
    statements.compile_to_writer(file_name.as_ref(), out).expect("failed to compile file");
  };
}
