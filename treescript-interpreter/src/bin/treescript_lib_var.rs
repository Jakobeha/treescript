use std::collections::HashMap;
use treescript_interpreter::lib_process::{BasicLibProcessError, Config, LibProcess};
use treescript_interpreter::session::LibrarySpec;
use treescript_interpreter::value::Value;

struct Lib {
  vars: HashMap<Value, Value>,
}

impl Lib {
  fn new() -> Lib {
    return Lib {
      vars: HashMap::new(),
    };
  }

  fn get_value(&self, key: Value) -> Value {
    return self.vars.get(&key).cloned().unwrap_or(Value::unit());
  }

  fn set_value(&mut self, key: Value, value: Value) {
    self.vars.insert(key, value);
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
      "Get" => {
        if args.len() != 2 {
          return Err(BasicLibProcessError::InvalidNumArgs {
            fun: String::from("Get"),
            expected: 1,
            actual: args.len(),
          });
        }
        let mut args_iter = args.into_iter();
        let arg0 = args_iter.next().unwrap();
        return Ok(self.get_value(arg0));
      }
      "Set" => {
        if args.len() != 2 {
          return Err(BasicLibProcessError::InvalidNumArgs {
            fun: String::from("Set"),
            expected: 2,
            actual: args.len(),
          });
        }
        let mut args_iter = args.into_iter();
        let arg0 = args_iter.next().unwrap();
        let arg1 = args_iter.next().unwrap();
        self.set_value(arg0, arg1);
        return Ok(Value::unit());
      }
      _ => return Err(BasicLibProcessError::UnknownFunction(name)),
    };
  }
}

fn main() {
  Lib::new().run_main();
}
