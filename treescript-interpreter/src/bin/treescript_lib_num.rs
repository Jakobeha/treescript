use std::collections::HashMap;
use treescript_interpreter::lib_process::{BasicLibProcessError, Config, LibProcess};
use treescript_interpreter::session::LibrarySpec;
use treescript_interpreter::value::{Float, Prim, Value};

struct Lib;

impl Lib {
  fn binary_num_combine<FI: FnOnce(i32, i32) -> i32, FF: FnOnce(f32, f32) -> f32>(
    &self,
    f_int: FI,
    f_float: FF,
    x: Value,
    y: Value,
  ) -> Result<Value, BasicLibProcessError> {
    match (x, y) {
      (Value::Prim(Prim::Integer(x)), Value::Prim(Prim::Integer(y))) => {
        return Ok(Value::Prim(Prim::Integer(f_int(x, y))));
      }
      (Value::Prim(Prim::Integer(x)), Value::Prim(Prim::Float(Float(y)))) => {
        return Ok(Value::Prim(Prim::Float(Float(f_float(x as f32, y)))));
      }
      (Value::Prim(Prim::Float(Float(x))), Value::Prim(Prim::Integer(y))) => {
        return Ok(Value::Prim(Prim::Float(Float(f_float(x, y as f32)))));
      }
      (Value::Prim(Prim::Float(Float(x))), Value::Prim(Prim::Float(Float(y)))) => {
        return Ok(Value::Prim(Prim::Float(Float(f_float(x, y)))));
      }
      (x, y) => {
        return Err(BasicLibProcessError::InvalidArgTypes {
          actual: vec![x, y],
          expected: String::from("numbers"),
        });
      }
    };
  }

  fn binary_num_compare<FI: FnOnce(i32, i32) -> bool, FF: FnOnce(f32, f32) -> bool>(
    &self,
    f_int: FI,
    f_float: FF,
    x: Value,
    y: Value,
  ) -> Result<Value, BasicLibProcessError> {
    match (x, y) {
      (Value::Prim(Prim::Integer(x)), Value::Prim(Prim::Integer(y))) => {
        return Ok(Value::bool(f_int(x, y)));
      }
      (Value::Prim(Prim::Integer(x)), Value::Prim(Prim::Float(Float(y)))) => {
        return Ok(Value::bool(f_float(x as f32, y)));
      }
      (Value::Prim(Prim::Float(Float(x))), Value::Prim(Prim::Integer(y))) => {
        return Ok(Value::bool(f_float(x, y as f32)));
      }
      (Value::Prim(Prim::Float(Float(x))), Value::Prim(Prim::Float(Float(y)))) => {
        return Ok(Value::bool(f_float(x, y)));
      }
      (x, y) => {
        return Err(BasicLibProcessError::InvalidArgTypes {
          actual: vec![x, y],
          expected: String::from("numbers"),
        });
      }
    };
  }
}

impl LibProcess for Lib {
  type Error = BasicLibProcessError;

  fn dependencies() -> HashMap<String, LibrarySpec> {
    return HashMap::new();
  }

  fn configure(&mut self, _config: Config) {}

  fn call_fun(&mut self, name: String, args: Vec<Value>) -> Result<Value, Self::Error> {
    match name.as_str() {
      "Add" => {
        if args.len() != 2 {
          return Err(BasicLibProcessError::InvalidNumArgs {
            fun: String::from("Add"),
            expected: 2,
            actual: args.len(),
          });
        }
        let mut args_iter = args.into_iter();
        let arg0 = args_iter.next().unwrap();
        let arg1 = args_iter.next().unwrap();
        return self.binary_num_combine(|x, y| x + y, |x, y| x + y, arg0, arg1);
      }
      "Subtract" => {
        if args.len() != 2 {
          return Err(BasicLibProcessError::InvalidNumArgs {
            fun: String::from("Subtract"),
            expected: 2,
            actual: args.len(),
          });
        }
        let mut args_iter = args.into_iter();
        let arg0 = args_iter.next().unwrap();
        let arg1 = args_iter.next().unwrap();
        return self.binary_num_combine(|x, y| x - y, |x, y| x - y, arg0, arg1);
      }
      "Multiply" => {
        if args.len() != 2 {
          return Err(BasicLibProcessError::InvalidNumArgs {
            fun: String::from("Multiply"),
            expected: 2,
            actual: args.len(),
          });
        }
        let mut args_iter = args.into_iter();
        let arg0 = args_iter.next().unwrap();
        let arg1 = args_iter.next().unwrap();
        return self.binary_num_combine(|x, y| x * y, |x, y| x * y, arg0, arg1);
      }
      "Divide" => {
        if args.len() != 2 {
          return Err(BasicLibProcessError::InvalidNumArgs {
            fun: String::from("Divide"),
            expected: 2,
            actual: args.len(),
          });
        }
        let mut args_iter = args.into_iter();
        let arg0 = args_iter.next().unwrap();
        let arg1 = args_iter.next().unwrap();
        return self.binary_num_combine(|x, y| x / y, |x, y| x / y, arg0, arg1);
      }
      "GreaterThan" => {
        if args.len() != 2 {
          return Err(BasicLibProcessError::InvalidNumArgs {
            fun: String::from("GreaterThan"),
            expected: 2,
            actual: args.len(),
          });
        }
        let mut args_iter = args.into_iter();
        let arg0 = args_iter.next().unwrap();
        let arg1 = args_iter.next().unwrap();
        return self.binary_num_compare(|x, y| x > y, |x, y| x > y, arg0, arg1);
      }
      "LessThan" => {
        if args.len() != 2 {
          return Err(BasicLibProcessError::InvalidNumArgs {
            fun: String::from("LessThan"),
            expected: 2,
            actual: args.len(),
          });
        }
        let mut args_iter = args.into_iter();
        let arg0 = args_iter.next().unwrap();
        let arg1 = args_iter.next().unwrap();
        return self.binary_num_compare(|x, y| x < y, |x, y| x < y, arg0, arg1);
      }
      _ => return Err(BasicLibProcessError::UnknownFunction(name)),
    };
  }
}

fn main() {
  Lib.run_main();
}
