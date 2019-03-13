use treescript_interpreter::lib_process::{BasicLibProcessError, Config, LibProcess};
use treescript_interpreter::session::LibrarySpec;
use treescript_interpreter::value::Value;

struct Lib;

impl Lib {
  fn map_inner(
    &self,
    f: Value,
    items: &Value,
    rest: &Value,
  ) -> Result<Value, BasicLibProcessError> {
    if let Value::Record { head, props } = rest {
      match head.as_str() {
        "Nil" => return Ok(Value::nil()),
        "Cons" => {
          let mut y = f.clone();
          y.subst(&Value::hole(1), &props[0]);
          let ys = self.map_inner(f, items, &props[1])?;
          return Ok(Value::cons(y, ys));
        }
        _ => (),
      }
    }

    return Err(BasicLibProcessError::InvalidArgTypes {
      actual: vec![items.clone()],
      expected: String::from("list"),
    });
  }

  fn map(&self, f: Value, items: Value) -> Result<Value, BasicLibProcessError> {
    return self.map_inner(f, &items, &items);
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
      "Map" => {
        if args.len() != 2 {
          return Err(BasicLibProcessError::InvalidNumArgs {
            fun: String::from("Map"),
            expected: 2,
            actual: args.len(),
          });
        }
        let mut args_iter = args.into_iter();
        let arg0 = args_iter.next().unwrap();
        let arg1 = args_iter.next().unwrap();
        return self.map(arg0, arg1);
      }
      _ => return Err(BasicLibProcessError::UnknownFunction(name)),
    };
  }
}

fn main() {
  Lib.run_main();
}
