use std::path::PathBuf;
use treescript_interpreter::lib_process::{BasicLibProcessError, Config, LibProcess};
use treescript_interpreter::session::LibrarySpec;
use treescript_interpreter::value::{Prim, Value};

struct LibFile {
  path: Option<PathBuf>,
  ast: Option<Vec<Value>>,
}

impl LibFile {
  fn new() -> LibFile {
    return LibFile {
      path: None,
      ast: None,
    };
  }

  fn get_path(&self) -> Value {
    return Value::option(
      self
        .path
        .as_ref()
        .map(|path| Value::Prim(Prim::String(String::from(path.to_string_lossy())))),
    );
  }

  fn get_ast(&self) -> Value {
    return Value::option(
      self
        .ast
        .as_ref()
        .map(|ast| Value::list(&mut ast.clone().into_iter())),
    );
  }
}

impl LibProcess for LibFile {
  type Error = BasicLibProcessError;

  fn dependencies() -> Vec<LibrarySpec> {
    return Vec::new();
  }

  fn configure(&mut self, config: Config) {
    self.ast = config.read_ast();
    self.path = config.path;
  }

  fn call_fun(&mut self, name: String, args: Vec<Value>) -> Result<Value, Self::Error> {
    match name.as_str() {
      "Path" => {
        if args.len() != 0 {
          return Err(BasicLibProcessError::InvalidNumArgs {
            fun: String::from("Path"),
            expected: 0,
            actual: args.len(),
          });
        }
        return Ok(self.get_path());
      }
      "Ast" => {
        if args.len() != 0 {
          return Err(BasicLibProcessError::InvalidNumArgs {
            fun: String::from("Ast"),
            expected: 0,
            actual: args.len(),
          });
        }
        return Ok(self.get_ast());
      }
      _ => return Err(BasicLibProcessError::UnknownFunction(name)),
    };
  }
}

fn main() {
  LibFile::new().run_main();
}
