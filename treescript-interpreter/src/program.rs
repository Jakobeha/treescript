extern crate enum_map;
extern crate serde;
use crate::parse::Parser;
use crate::print::Printer;
use crate::reduce::{GroupDef, GroupDefSerial, ReduceType, Statement};
use enum_map::enum_map;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::io::{BufRead, BufReader, Read, Write};
use std::rc::{Rc, Weak};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProgramSerial {
  pub num_props_by_head: HashMap<String, usize>,
  pub libraries: Vec<String>,
  pub main_statements: Vec<Statement>,
  pub sub_groups: Vec<GroupDefSerial>,
}

#[derive(Clone, Debug)]
pub struct Program {
  pub num_props_by_head: HashMap<String, usize>,
  pub libraries: Vec<String>,
  pub main_group: GroupDef,
  pub sub_groups: Rc<Vec<GroupDef>>,
}

impl From<ProgramSerial> for Program {
  fn from(serial: ProgramSerial) -> Program {
    let main_statements = serial.main_statements;
    let mut program = Program {
      num_props_by_head: serial.num_props_by_head,
      libraries: serial.libraries,
      main_group: GroupDef {
        props: vec![],
        loops: false,
        statements: enum_map![
          ReduceType::Regular => main_statements.clone(),
          ReduceType::EvalCtx => vec![],
          ReduceType::AltConsume => vec![],
        ],
        env: Weak::new(),
      },
      sub_groups: Rc::new(
        serial
          .sub_groups
          .into_iter()
          .map(|sub_group| GroupDef::from_no_env(sub_group))
          .collect(),
      ),
    };

    unsafe {
      let sub_groups = Rc::into_raw(program.sub_groups);
      let sub_groups = sub_groups as *mut Vec<GroupDef>;
      let sub_groups_iter = (&mut *sub_groups).iter_mut();
      program.sub_groups = Rc::from_raw(sub_groups);
      program.main_group.env = Rc::downgrade(&program.sub_groups);
      for sub_group in sub_groups_iter {
        sub_group.env = Rc::downgrade(&program.sub_groups);
      }
    }
    return program;
  }
}

impl<R: Read> From<R> for Program {
  fn from(read: R) -> Program {
    let mut reader = BufReader::new(read);
    reader.read_line(&mut String::new()).unwrap(); //Gets rid of shebang
    let serial: ProgramSerial = rmp_serde::from_read(reader).unwrap();
    return Program::from(serial);
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
