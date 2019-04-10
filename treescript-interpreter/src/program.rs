extern crate serde;
use crate::parse::Parser;
use crate::print::Printer;
use crate::reduce::{Consume, GroupDef, GroupDefSerial, ReduceResult};
use crate::session::{LibrarySpec, Session};
use crate::value::Value;
use serde::{Deserialize, Serialize};
use std::io::{BufRead, BufReader, Read, Write};
use std::path::PathBuf;
use std::rc::Rc;

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProgramSerial {
  pub libraries: Vec<LibrarySpec>,
  pub groups: Vec<GroupDefSerial>,
}

#[derive(Clone, Debug)]
pub struct Program {
  pub libraries: Vec<LibrarySpec>,
  pub groups: Rc<Vec<GroupDef>>,
}

impl From<ProgramSerial> for Program {
  fn from(serial: ProgramSerial) -> Program {
    let mut program = Program {
      libraries: serial.libraries,
      groups: Rc::new(
        serial
          .groups
          .into_iter()
          .map(|sub_group| GroupDef::from_no_env(sub_group))
          .collect(),
      ),
    };

    unsafe {
      let groups = Rc::into_raw(program.groups);
      let groups = groups as *mut Vec<GroupDef>;
      let groups_iter = (&mut *groups).iter_mut();
      program.groups = Rc::from_raw(groups);
      for group in groups_iter {
        group.env = Rc::downgrade(&program.groups);
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
  pub fn new_session(&self) -> Session {
    return Session::new(self.libraries.clone());
  }

  pub fn inferred_lang(&self) -> Option<String> {
    return self
      .groups
      .iter()
      .flat_map(|group| group.reducers.iter())
      .flat_map(|reducer| reducer.main.input.iter())
      .filter_map(|consume| match consume {
        Consume::Record(head) => Some(head),
        _ => None,
      })
      .filter_map(|head| Value::record_head_to_fun(head))
      .map(|(lib, _)| lib)
      .next();
  }

  fn main_group(&self) -> &GroupDef {
    return self
      .groups
      .iter()
      .next()
      .expect("program ill-formed - needs at least 1 group");
  }

  pub fn run<R: Read, W: Write>(
    &self,
    session: &mut Session,
    in_path: &Option<PathBuf>,
    input: &mut R,
    output: &mut W,
  ) {
    let mut parser = Parser { input: input };
    let mut printer = Printer { output: output };

    session.start(in_path);
    while let Option::Some(next) = parser.scan_value() {
      match self.main_group().transform(session, &next) {
        ReduceResult::Fail => panic!("couldn't reduce {}", next),
        ReduceResult::Success(next) => printer.print_value(next).unwrap(),
      };
    }
    session.stop();
  }
}
