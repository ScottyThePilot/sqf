use sqf::parser::run;
use sqf::parser::database::Database;

use std::fs;
use std::io::Cursor;

#[test]
fn main() {
  let database = Database::default();
  for entry in fs::read_dir("./tests/samples").unwrap() {
    let entry = entry.unwrap();
    if entry.file_type().unwrap().is_file() {
      let path = entry.path();
      let file_name = path.file_name().unwrap().to_string_lossy();

      println!("testing {file_name}");
      let file = fs::read_to_string(&path).expect("failed to read file");
      let statements = run(&database, file).expect("failed to parse file");

      let compiled = statements.compile(&file_name).expect("failed to compile file");
      let mut serialized_out = Cursor::new(Vec::new());
      compiled.serialize(&mut serialized_out).expect("failed to serialize file");
    };
  };
}
