use crate::value::{Prim, Value};
use std::io;
use std::io::Write;

pub struct Printer<'a, W: Write> {
  pub output: &'a mut W,
}

impl<'a, W: Write> Printer<'a, W> {
  fn print_string(&mut self, x: String) -> io::Result<()> {
    return write!(
      self.output,
      "string \"{}\" ",
      x.chars()
        .flat_map(|c| c.escape_default())
        .collect::<String>()
    );
  }

  fn print_sub_value(&mut self, x: Value) -> io::Result<()> {
    match x {
      Value::Splice(idx) => return write!(self.output, "splice {} ", idx),
      Value::Prim(Prim::Integer(val)) => return write!(self.output, "integer {} ", val),
      Value::Prim(Prim::Float(val)) => return write!(self.output, "float {} ", val),
      Value::Prim(Prim::String(val)) => return self.print_string(val),
      Value::Record { head, props } => {
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
