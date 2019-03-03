use std::cell::RefCell;
use std::collections::HashMap;
use std::env;

//From https://stackoverflow.com/questions/27588416/how-to-send-output-to-stderr
macro_rules! dprint(
    ($var:expr, $($arg:tt)*) => {
      $crate::debug::Env::with_shared(|env| {
        if env.borrow_mut().has_var($var) {
          use std::io::Write;

          write!(&mut ::std::io::stderr(), $($arg)*).expect("failed printing to stderr");
        }
      });
    }
);

pub struct Env {
  vars: HashMap<String, bool>,
}

thread_local! {
  static SHARED_ENV: RefCell<Env> = RefCell::new(Env::new());
}

impl Env {
  pub fn with_shared<F: FnOnce(&RefCell<Env>) -> R, R>(f: F) -> R {
    return SHARED_ENV.with(f);
  }

  fn new() -> Env {
    return Env {
      vars: HashMap::new(),
    };
  }

  pub fn has_var(&mut self, key: &str) -> bool {
    return *self.vars.entry(String::from(key)).or_insert_with(|| {
      env::var_os(String::from("TS_") + key).map_or(false, |val| val != "false" && val != "0")
    });
  }
}
