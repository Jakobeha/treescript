use treescript_interpreter::lib_process::{BasicLibProcessError, Config, LibProcess};
use treescript_interpreter::session::LibrarySpec;
use treescript_interpreter::value::{Value, Prim};

struct Lib;

impl Lib {
  fn is_num(&self, x: Value) -> Value {
    match x {
      Value::Prim(Prim::Integer(_)) | Value::Prim(Prim::Float(_)) => return Value::bool(true),
      _ => return Value::bool(false),
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
      "IsNum" => {
        if args.len() != 1 {
          return Err(BasicLibProcessError::InvalidNumArgs {
            fun: String::from("IsNum"),
            expected: 1,
            actual: args.len(),
          });
        }
        let mut args_iter = args.into_iter();
        let arg0 = args_iter.next().unwrap();
        return Ok(self.is_num(arg0));
      }
      _ => return Err(BasicLibProcessError::UnknownFunction(name)),
    };
  }
}

fn main() {
  Lib.run_main();
}
