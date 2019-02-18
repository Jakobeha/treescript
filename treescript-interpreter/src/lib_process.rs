extern crate serde_json;
use crate::parse::Parser;
use crate::print::Printer;
use crate::value::Value;
use std::collections::HashMap;
use std::env;
use std::fmt::Debug;
use std::io;
use std::io::{Read, Write};

pub trait LibProcessError: Debug + From<io::Error> {
  fn unknown_function(name: String) -> Self;
  fn not_enough_args(fun: String, expected: usize) -> Self;
  fn invalid_arg_types(actual: Vec<Value>, expected: String) -> Self;
  fn not_function(value: Value) -> Self;
}

pub trait LibProcess {
  type Error: LibProcessError;

  fn call_fun<I: Iterator<Item = Value>>(
    &mut self,
    name: String,
    args: &mut I,
  ) -> Result<Value, Self::Error>;

  fn run<R: Read, W: Write>(
    &mut self,
    num_props_by_head: HashMap<String, usize>,
    input: &mut R,
    output: &mut W,
  ) -> Result<(), Self::Error> {
    let mut parser = Parser {
      input: input,
      num_props_by_head: num_props_by_head,
    };
    let mut printer = Printer { output: output };
    loop {
      let fun = parser.scan_word();
      if fun.is_empty() {
        break;
      }

      match self.call_fun(fun, &mut parser) {
        Ok(res) => {
          if let Err(err) = printer.print_value(res) {
            return Err(Self::Error::from(err));
          }
        }
        Err(err) => return Err(err),
      };
    }
    return Ok(());
  }

  fn run_main(&mut self) {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
      panic!(
        "invalid number of arguments - expected 1, got {}",
        args.len()
      );
    }
    let num_props_by_head = serde_json::from_str(&args[1].as_str())
      .expect("failed to parse num_props_by_head (first argument)");

    self
      .run(num_props_by_head, &mut io::stdin(), &mut io::stdout())
      .expect("server error");
  }
}

#[derive(Debug)]
pub enum BasicLibProcessError {
  UnknownFunction(String),
  NotEnoughArgs {
    fun: String,
    expected: usize,
  },
  InvalidArgTypes {
    actual: Vec<Value>,
    expected: String,
  },
  NotFunction(Value),
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

  fn not_enough_args(fun: String, expected: usize) -> BasicLibProcessError {
    return BasicLibProcessError::NotEnoughArgs {
      fun: fun,
      expected: expected,
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
}
