use crate::parse::Parser;
use crate::print::Printer;
use crate::session::{LibrarySpec, Session};
use crate::value::Value;
use std::env;
use std::fmt::Debug;
use std::io;
use std::io::{Read, Write};
use std::path::PathBuf;

pub struct Config<'a> {
  pub session: &'a mut Session,
  pub path: Option<PathBuf>,
}

impl<'a> Config<'a> {
  pub fn read_ast(&self) -> Option<Vec<Value>> {
    return self.path.as_ref().map(|path| {
      Parser {
        input: &mut self.session.read_ast(path.as_path()),
      }
      .collect()
    });
  }
}

pub trait LibProcessError: Debug + From<io::Error> {
  fn unknown_function(name: String) -> Self;
  fn invalid_num_args(fun: String, expected: usize, actual: usize) -> Self;
  fn invalid_arg_types(actual: Vec<Value>, expected: String) -> Self;
  fn not_function(value: Value) -> Self;
  fn unsupported(reason: String) -> Self;
}

pub trait LibProcess {
  type Error: LibProcessError;

  fn dependencies() -> Vec<LibrarySpec>;
  fn configure(&mut self, config: Config);
  fn call_fun(&mut self, fun: String, args: Vec<Value>) -> Result<Value, Self::Error>;

  fn run<R: Read, W: Write>(&mut self, input: &mut R, output: &mut W) -> Result<(), Self::Error> {
    let mut parser = Parser { input: input };
    let mut printer = Printer { output: output };
    while let Some(fun_val) = parser.scan_value() {
      if let Value::Record {
        head: fun,
        props: args,
      } = fun_val
      {
        let res = self.call_fun(fun, args)?;
        printer
          .print_value(res)
          .map_err(|err| Self::Error::from(err))?;
      } else {
        return Err(Self::Error::not_function(fun_val));
      }
    }
    return Ok(());
  }

  fn run_main(&mut self) {
    let args: Vec<String> = env::args().collect();
    let mut session = Session::new(Self::dependencies());
    let path: Option<PathBuf>;
    match args.len() {
      1 => path = None,
      2 => path = Some(PathBuf::from(args[1].clone())),
      _ => panic!(
        "invalid number of arguments - expected 0 or 1, got {}",
        args.len() - 1
      ),
    };

    session.start(&path);
    self.configure(Config {
      session: &mut session,
      path: path,
    });
    self
      .run(&mut io::stdin(), &mut io::stdout())
      .expect("server error");
    session.stop();
  }
}

#[derive(Debug)]
pub enum BasicLibProcessError {
  UnknownFunction(String),
  InvalidNumArgs {
    fun: String,
    expected: usize,
    actual: usize,
  },
  InvalidArgTypes {
    actual: Vec<Value>,
    expected: String,
  },
  NotFunction(Value),
  Unsupported(String),
  IOError(io::Error),
}

impl From<io::Error> for BasicLibProcessError {
  fn from(err: io::Error) -> BasicLibProcessError {
    return BasicLibProcessError::IOError(err);
  }
}

impl LibProcessError for BasicLibProcessError {
  fn unknown_function(name: String) -> BasicLibProcessError {
    return BasicLibProcessError::UnknownFunction(name);
  }

  fn invalid_num_args(fun: String, expected: usize, actual: usize) -> BasicLibProcessError {
    return BasicLibProcessError::InvalidNumArgs {
      fun: fun,
      expected: expected,
      actual: actual,
    };
  }

  fn invalid_arg_types(actual: Vec<Value>, expected: String) -> Self {
    return BasicLibProcessError::InvalidArgTypes {
      actual: actual,
      expected: expected,
    };
  }

  fn not_function(value: Value) -> BasicLibProcessError {
    return BasicLibProcessError::NotFunction(value);
  }

  fn unsupported(reason: String) -> BasicLibProcessError {
    return BasicLibProcessError::Unsupported(reason);
  }
}
