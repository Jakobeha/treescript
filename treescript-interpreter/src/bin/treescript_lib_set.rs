use std::collections::{HashMap, HashSet};
use treescript_interpreter::lib_process::{BasicLibProcessError, Config, LibProcess};
use treescript_interpreter::session::LibrarySpec;
use treescript_interpreter::value::Value;

struct Lib {
  sets: HashMap<Value, HashSet<Value>>,
}

impl Lib {
  fn new() -> Lib {
    return Lib {
      sets: HashMap::new(),
    };
  }

  fn set_has_value(&self, key: Value, value: Value) -> Value {
    return Value::bool(
      self
        .sets
        .get(&key)
        .map_or(false, |set| set.contains(&value)),
    );
  }

  fn add_value_to_set(&mut self, key: Value, value: Value) -> Value {
    let set = self.sets.entry(key).or_insert(HashSet::new());
    return Value::bool(set.insert(value));
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
      "Has" => {
        if args.len() != 2 {
          return Err(BasicLibProcessError::InvalidNumArgs {
            fun: String::from("Has"),
            expected: 2,
            actual: args.len(),
          });
        }
        let mut args_iter = args.into_iter();
        let arg0 = args_iter.next().unwrap();
        let arg1 = args_iter.next().unwrap();
        return Ok(self.set_has_value(arg0, arg1));
      }
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
        return Ok(self.add_value_to_set(arg0, arg1));
      }
      _ => return Err(BasicLibProcessError::UnknownFunction(name)),
    };
  }
}

fn main() {
  Lib::new().run_main();
}
