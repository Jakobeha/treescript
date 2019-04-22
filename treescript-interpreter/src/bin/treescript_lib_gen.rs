#![feature(try_trait)]

use std::collections::{HashMap, HashSet};
use std::ops::Try;
use std::option::NoneError;
use treescript_interpreter::lib_process::{BasicLibProcessError, Config, LibProcess};
use treescript_interpreter::session::LibrarySpec;
use treescript_interpreter::value::{Prim, Value};

struct LibGen {
  ast: Option<Vec<Value>>,
  used_idxs: HashSet<usize>,
}

impl LibGen {
  fn new() -> LibGen {
    return LibGen {
      ast: None,
      used_idxs: HashSet::new(),
    };
  }

  fn cant_read_ast_err() -> BasicLibProcessError {
    return BasicLibProcessError::Unsupported(String::from("can't read AST"));
  }

  fn free_string_lit(i: usize) -> String {
    return format!("free{}", i);
  }

  fn free_string(&mut self, template: Value) -> Result<Value, BasicLibProcessError> {
    let ast = self
      .ast
      .as_ref()
      .into_result()
      .map_err(|NoneError| LibGen::cant_read_ast_err())?;
    let mut i = 0;
    let mut res = template.clone();
    res.subst(
      &Value::hole(1),
      &Value::Prim(Prim::String(LibGen::free_string_lit(i))),
    );
    loop {
      if self.used_idxs.contains(&i) {
        i += 1;
        res = template.clone();
        res.subst(
          &Value::hole(1),
          &Value::Prim(Prim::String(LibGen::free_string_lit(i))),
        );
        continue;
      }
      for val in ast {
        if val == &res {
          i += 1;
          res = template.clone();
          res.subst(
            &Value::hole(1),
            &Value::Prim(Prim::String(LibGen::free_string_lit(i))),
          );
          continue;
        }
        for sub in val.breadth_first() {
          if sub == &res {
            i += 1;
            res = template.clone();
            res.subst(
              &Value::hole(1),
              &Value::Prim(Prim::String(LibGen::free_string_lit(i))),
            );
            continue;
          }
        }
      }
      break;
    }
    self.used_idxs.insert(i);
    return Ok(res);
  }
}

impl LibProcess for LibGen {
  type Error = BasicLibProcessError;

  fn dependencies() -> HashMap<String, LibrarySpec> {
    return HashMap::new();
  }

  fn configure(&mut self, config: Config) {
    self.ast = config.read_ast();
  }

  fn call_fun(&mut self, name: String, args: Vec<Value>) -> Result<Value, Self::Error> {
    match name.as_str() {
      "FreeString" => {
        if args.len() != 1 {
          return Err(BasicLibProcessError::InvalidNumArgs {
            fun: String::from("FreeString"),
            expected: 1,
            actual: args.len(),
          });
        }
        let mut args_iter = args.into_iter();
        let arg0 = args_iter.next().unwrap();
        return self.free_string(arg0);
      }
      _ => return Err(BasicLibProcessError::UnknownFunction(name)),
    };
  }
}

fn main() {
  LibGen::new().run_main();
}
