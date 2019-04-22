extern crate serde;
use crate::parse::Parser;
use crate::print::Printer;
use crate::reduce::{GroupDef, GroupDefSerial, ReduceResult};
use crate::session::{LibrarySpec, Session};
use crate::value::{Record, Symbol, Value};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::io::{BufRead, BufReader, Read, Write};
use std::iter::FromIterator;
use std::path::PathBuf;
use std::rc::Rc;

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct DeclSet {
  pub records: Vec<(String, usize)>,
  pub groups: Vec<(String, (usize, usize))>,
  pub functions: Vec<(String, usize)>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ImportDecl {
  pub path: String,
  pub qual: String,
  pub exports: DeclSet,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct RecordDecl {
  pub head: String,
  pub props: Vec<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProgramSerial {
  pub path: String,
  pub import_decls: Vec<ImportDecl>,
  pub record_decls: Vec<RecordDecl>,
  pub exports: DeclSet,
  pub groups: Vec<(Symbol, GroupDefSerial)>,
  pub libraries: Vec<(String, LibrarySpec)>,
}

#[derive(Clone, Debug)]
pub struct Program {
  path: String,
  pub groups: Rc<HashMap<Symbol, GroupDef>>,
  pub libraries: HashMap<String, LibrarySpec>,
}

impl From<ProgramSerial> for Program {
  fn from(serial: ProgramSerial) -> Program {
    let mut program = Program {
      path: serial.path,
      groups: Rc::new(
        serial
          .groups
          .into_iter()
          .map(|(head, sub_group)| (head, GroupDef::from_no_env(sub_group)))
          .collect(),
      ),
      libraries: HashMap::from_iter(serial.libraries),
    };

    unsafe {
      let groups = Rc::into_raw(program.groups);
      let groups = groups as *mut HashMap<Symbol, GroupDef>;
      let groups_iter = (&mut *groups).values_mut();
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

  pub fn inferred_lang(&self) -> Option<&String> {
    return self
      .groups
      .values()
      .flat_map(|group| group.reducers.iter())
      .map(|reducer| &reducer.main.input)
      .filter_map(|value| match value {
        Value::Record(Record { head, props: _ }) => Some(head),
        _ => None,
      })
      .map(|head| &head.module)
      .next();
  }

  fn main_group(&self) -> &GroupDef {
    return self
      .groups
      .get(&Symbol::main_group(self.path.clone()))
      .expect("program ill-formed - needs \"Main\" group");
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
