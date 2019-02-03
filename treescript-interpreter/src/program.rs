extern crate serde;
use crate::parse::Parser;
use crate::print::Printer;
use crate::reduce::Group;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::io::{BufRead, BufReader, Read, Write};

#[derive(Debug, Deserialize, Serialize)]
pub struct Program {
  pub num_props_by_head: HashMap<String, usize>,
  pub libraries: Vec<String>,
  pub main_group: Group,
}

impl<R: Read> From<R> for Program {
  fn from(read: R) -> Program {
    let mut reader = BufReader::new(read);
    reader.read_line(&mut String::new()).unwrap(); //Gets rid of shebang
    return rmp_serde::from_read(reader).unwrap();
  }
}

impl Program {
  pub fn run<R: Read, W: Write>(&self, input: &mut R, output: &mut W) {
    let mut parser = Parser {
      input: input,
      num_props_by_head: self.num_props_by_head.clone(),
    };
    let mut printer = Printer { output: output };

    //TODO Setup and teardown libraries

    loop {
      let next_word = parser.scan_word();
      if next_word.is_empty() {
        break;
      }
      let mut next = parser.scan_value(next_word);
      self.main_group.reduce(&mut next);
      printer.print_value(next).unwrap();
      printer.print_newline().unwrap();
    }
  }
}
