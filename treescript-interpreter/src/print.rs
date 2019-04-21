use crate::value::{Record, Value};
use std::io;
use std::io::Write;

pub struct Printer<'a, W: Write> {
  pub output: &'a mut W,
}

impl<'a, W: Write> Printer<'a, W> {
  fn print_sub_value(&mut self, x: Value) -> io::Result<()> {
    match x {
      Value::Splice(idx) => return write!(self.output, "splice {} ", idx),
      Value::Prim(prim) => return write!(self.output, "{} {} ", prim.type_str(), prim),
      Value::Record(Record { head, props }) => {
        if let Err(err) = write!(self.output, "{} {} ", head, props.len()) {
          return Err(err);
        }
        for prop in props {
          if let Err(err) = self.print_sub_value(prop) {
            return Err(err);
          }
        }
        return Ok(());
      }
    }
  }

  fn print_newline(&mut self) -> io::Result<()> {
    return write!(self.output, "\n");
  }

  pub fn print_value(&mut self, x: Value) -> io::Result<()> {
    return self.print_sub_value(x).and_then(|()| self.print_newline());
  }
}
