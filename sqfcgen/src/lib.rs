extern crate byteorder;
extern crate lzo1x_1 as lzo;
#[macro_use]
extern crate thiserror;

pub mod compiler;
pub mod serializer;

#[derive(Debug, Error)]
pub enum Error {
  #[error(transparent)]
  CompileError(#[from] crate::compiler::CompileError),
  #[error(transparent)]
  SerializeError(#[from] crate::serializer::SerializeError)
}
