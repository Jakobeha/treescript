extern crate enum_map;
extern crate serde;
use crate::parse::Parser;
use crate::print::Printer;
use crate::reduce::{Consume, GroupDef, GroupDefSerial, ReduceType, Statement};
use crate::session::{LibrarySpec, Session};
use crate::value::Value;
use enum_map::enum_map;
use serde::{Deserialize, Serialize};
use std::io::{BufRead, BufReader, Read, Write};
use std::iter;
use std::path::PathBuf;
use std::rc::{Rc, Weak};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProgramSerial {
  pub libraries: Vec<LibrarySpec>,
  pub main_statements: Vec<Statement>,
  pub sub_groups: Vec<GroupDefSerial>,
}

#[derive(Clone, Debug)]
pub struct Program {
  pub libraries: Vec<LibrarySpec>,
  pub main_group: GroupDef,
  pub sub_groups: Rc<Vec<GroupDef>>,
}

impl From<ProgramSerial> for Program {
  fn from(serial: ProgramSerial) -> Program {
    let main_statements = serial.main_statements;
    let mut program = Program {
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
  pub fn new_session(&self) -> Session {
    return Session::new(self.libraries.clone());
  }

  fn groups(&self) -> impl Iterator<Item = &GroupDef> {
    return iter::once(&self.main_group).chain(self.sub_groups.iter());
  }

  pub fn inferred_lang(&self) -> Option<String> {
    return self
      .groups()
      .flat_map(|group| group.statements[ReduceType::Regular].iter())
      .flat_map(|statement| match statement {
        Statement::Reducer(reducer) => vec![&reducer.input, &reducer.output],
        Statement::Group(_) => vec![],
      })
      .flat_map(|clause| clause.consumes.iter())
      .filter_map(|consume| match consume {
        Consume::Record(head) => Some(head),
        _ => None,
      })
      .filter_map(|head| Value::record_head_to_fun(head))
      .map(|(lib, _)| lib)
      .next();
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
    while let Option::Some(mut next) = parser.scan_value() {
      self.main_group.reduce(session, &mut next);
      printer.print_value(next).unwrap();
    }
    session.stop();
  }
}
