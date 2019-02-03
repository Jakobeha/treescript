use crate::value::{Prim, Value};
use std::io;
use std::io::Write;

pub struct Printer<W: Write> {
  pub output: W,
}

impl<W: Write> Printer<W> {
  pub fn print_string(&mut self, x: String) -> io::Result<()> {
    return write!(
      &mut self.output,
      "string \"{}\" ",
      x.chars()
        .flat_map(|c| c.escape_default())
        .collect::<String>()
    );
  }

  pub fn print_value(&mut self, x: Value) -> io::Result<()> {
    match x {
      Value::Splice(idx) => return write!(&mut self.output, "splice {} ", idx),
      Value::Prim(Prim::Integer(val)) => return write!(&mut self.output, "integer {} ", val),
      Value::Prim(Prim::Float(val)) => return write!(&mut self.output, "float {} ", val),
      Value::Prim(Prim::String(val)) => return self.print_string(val),
      Value::Record { head, props } => {
        if let Err(err) = write!(&mut self.output, "{} ", head) {
          return Err(err);
        }
        for prop in props {
          if let Err(err) = self.print_value(prop) {
            return Err(err);
          }
        }
        return Ok(());
      }
    }
  }

  pub fn print_newline(&mut self) -> io::Result<()> {
    return write!(&mut self.output, "\n");
  }
}
