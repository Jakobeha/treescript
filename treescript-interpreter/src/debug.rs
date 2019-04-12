use std::cell::RefCell;
use std::collections::HashMap;
use std::env;

//From https://stackoverflow.com/questions/27588416/how-to-send-output-to-stderr
macro_rules! dprintq(
    ($done:expr, $var:expr, $($arg:tt)*) => {
      $crate::debug::Env::with_shared(|env| {
        let mut env = env.borrow_mut();
        if env.has_var($var) {
          use std::io::Write;

          if env.at_line_start {
            for _ in 0..env.indents {
              write!(&mut ::std::io::stderr(), "  ").expect("failed printing to stderr");
            }
          }
          write!(&mut ::std::io::stderr(), $($arg)*).expect("failed printing to stderr");
          env.at_line_start = $done;
        }
      });
    }
);

macro_rules! dprint(
    ($var:expr, $($arg:tt)*) => {dprintq!(false, $var, $($arg)*)}
);

macro_rules! dprintln(
    ($var:expr, $($arg:tt)*) => {
      dprintq!(false, $var, $($arg)*);
      dprintq!(true, $var, "\n");
    }
);

macro_rules! dprint_begin_indent(
    ($var:expr) => {
      $crate::debug::Env::with_shared(|env| {
        let mut env = env.borrow_mut();
        if env.has_var($var) {
          env.indents += 1;
        }
      });
    }
);

macro_rules! dprint_end_indent(
    ($var:expr) => {
      $crate::debug::Env::with_shared(|env| {
        let mut env = env.borrow_mut();
        if env.has_var($var) {
          env.indents -= 1;
        }
      });
    }
);

pub struct Env {
  vars: HashMap<String, bool>,
  pub indents: usize,
  pub at_line_start: bool,
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
      indents: 0,
      at_line_start: true,
    };
  }

  pub fn has_var(&mut self, key: &str) -> bool {
    return *self.vars.entry(String::from(key)).or_insert_with(|| {
      env::var_os(String::from("TS_") + key).map_or(false, |val| val != "false" && val != "0")
    });
  }
}
