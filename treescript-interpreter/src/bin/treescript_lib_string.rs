extern crate inflector;
use inflector::cases::camelcase;
use treescript_interpreter::lib_process::{BasicLibProcessError, Config, LibProcess};
use treescript_interpreter::session::LibrarySpec;
use treescript_interpreter::value::{Prim, Value};

struct Lib;

impl Lib {
  fn unary_str_transform<F: FnOnce(String) -> String>(
    &self,
    f: F,
    x: Value,
  ) -> Result<Value, BasicLibProcessError> {
    match x {
      Value::Prim(Prim::String(x)) => {
        return Ok(Value::Prim(Prim::String(f(x))));
      }
      x => {
        return Err(BasicLibProcessError::InvalidArgTypes {
          actual: vec![x],
          expected: String::from("string"),
        });
      }
    };
  }

  fn binary_str_combine<F: FnOnce(String, String) -> String>(
    &self,
    f: F,
    x: Value,
    y: Value,
  ) -> Result<Value, BasicLibProcessError> {
    match (x, y) {
      (Value::Prim(Prim::String(x)), Value::Prim(Prim::String(y))) => {
        return Ok(Value::Prim(Prim::String(f(x, y))));
      }
      (x, y) => {
        return Err(BasicLibProcessError::InvalidArgTypes {
          actual: vec![x, y],
          expected: String::from("strings"),
        });
      }
    };
  }
}

impl LibProcess for Lib {
  type Error = BasicLibProcessError;

  fn dependencies() -> Vec<LibrarySpec> {
    return Vec::new();
  }

  fn configure(&mut self, _config: Config) {}

  fn call_fun(&mut self, name: String, args: Vec<Value>) -> Result<Value, Self::Error> {
    match name.as_str() {
      "Append" => {
        if args.len() != 2 {
          return Err(BasicLibProcessError::InvalidNumArgs {
            fun: String::from("Append"),
            expected: 2,
            actual: args.len(),
          });
        }
        let mut args_iter = args.into_iter();
        let arg0 = args_iter.next().unwrap();
        let arg1 = args_iter.next().unwrap();
        return self.binary_str_combine(|x, y| x + &y, arg0, arg1);
      }
      "ToCamel" => {
        if args.len() != 1 {
          return Err(BasicLibProcessError::InvalidNumArgs {
            fun: String::from("ToCamel"),
            expected: 1,
            actual: args.len(),
          });
        }
        let mut args_iter = args.into_iter();
        let arg0 = args_iter.next().unwrap();
        return self.unary_str_transform(|x| camelcase::to_camel_case(x.as_str()), arg0);
      }
      _ => return Err(BasicLibProcessError::UnknownFunction(name)),
    };
  }
}

fn main() {
  Lib.run_main();
}
