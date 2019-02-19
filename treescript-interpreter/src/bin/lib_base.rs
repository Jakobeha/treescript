use std::collections::{HashMap, HashSet};
use treescript_interpreter::lib_process::{BasicLibProcessError, LibProcess};
use treescript_interpreter::value::{Float, Prim, Value};

struct LibBase {
  values: HashMap<Value, Value>,
  sets: HashMap<Value, HashSet<Value>>,
}

impl LibBase {
  fn new() -> LibBase {
    return LibBase {
      values: HashMap::new(),
      sets: HashMap::new(),
    };
  }

  fn is_equal(&self, x: Value, y: Value) -> Value {
    return Value::bool(x == y);
  }

  fn values_match(&self, x: Value, y: Value) -> Value {
    return Value::bool(x.matches(&y));
  }

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

  fn get_value(&self, key: Value) -> Value {
    return self.values.get(&key).cloned().unwrap_or(Value::unit());
  }

  fn set_value(&mut self, key: Value, value: Value) {
    self.values.insert(key, value);
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

impl LibProcess for LibBase {
  type Error = BasicLibProcessError;

  fn call_fun(&mut self, name: String, args: Vec<Value>) -> Result<Value, Self::Error> {
    match name.as_str() {
      "IsEqual" => {
        if args.len() != 2 {
          return Err(BasicLibProcessError::InvalidNumArgs {
            fun: String::from("IsEqual"),
            expected: 2,
            actual: args.len(),
          });
        }
        let mut args_iter = args.into_iter();
        let arg0 = args_iter.next().unwrap();
        let arg1 = args_iter.next().unwrap();
        return Ok(self.is_equal(arg0, arg1));
      }
      "Matches" => {
        if args.len() != 2 {
          return Err(BasicLibProcessError::InvalidNumArgs {
            fun: String::from("Matches"),
            expected: 2,
            actual: args.len(),
          });
        }
        let mut args_iter = args.into_iter();
        let arg0 = args_iter.next().unwrap();
        let arg1 = args_iter.next().unwrap();
        return Ok(self.values_match(arg0, arg1));
      }
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
      "Value_Get" => {
        if args.len() != 2 {
          return Err(BasicLibProcessError::InvalidNumArgs {
            fun: String::from("Value_Get"),
            expected: 1,
            actual: args.len(),
          });
        }
        let mut args_iter = args.into_iter();
        let arg0 = args_iter.next().unwrap();
        return Ok(self.get_value(arg0));
      }
      "Value_Set" => {
        if args.len() != 2 {
          return Err(BasicLibProcessError::InvalidNumArgs {
            fun: String::from("Value_Set"),
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
      "Set_Has" => {
        if args.len() != 2 {
          return Err(BasicLibProcessError::InvalidNumArgs {
            fun: String::from("Set_Has"),
            expected: 2,
            actual: args.len(),
          });
        }
        let mut args_iter = args.into_iter();
        let arg0 = args_iter.next().unwrap();
        let arg1 = args_iter.next().unwrap();
        return Ok(self.set_has_value(arg0, arg1));
      }
      "Set_Add" => {
        if args.len() != 2 {
          return Err(BasicLibProcessError::InvalidNumArgs {
            fun: String::from("Set_Add"),
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
  LibBase::new().run_main();
}
