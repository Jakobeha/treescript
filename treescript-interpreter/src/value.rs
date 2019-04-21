extern crate serde;
use crate::util::GeneratorIterator;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::Generator;
use std::slice::{Iter, IterMut};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Float(pub f32);

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum Prim {
  Integer(i32),
  Float(Float),
  String(String),
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct Symbol {
  pub module: String,
  pub local: String,
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct Record {
  pub head: Symbol,
  pub props: Vec<Value>,
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum Value {
  Prim(Prim),
  Record(Record),
  Splice(usize),
}

impl PartialEq for Float {
  fn eq(&self, other: &Float) -> bool {
    return self.0.to_bits() == other.0.to_bits();
  }
}

impl Eq for Float {}

impl Hash for Float {
  fn hash<H: Hasher>(&self, hasher: &mut H) {
    self.0.to_bits().hash(hasher);
  }
}

impl Display for Float {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    return self.0.fmt(f);
  }
}

impl Display for Prim {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Prim::Integer(x) => x.fmt(f),
      Prim::Float(x) => x.fmt(f),
      Prim::String(x) => write!(
        f,
        "\"{}\"",
        x.chars()
          .flat_map(|c| c.escape_default())
          .collect::<String>()
      ),
    }
  }
}

impl Prim {
  pub fn type_str(&self) -> &'static str {
    match self {
      Prim::Integer(_) => "integer",
      Prim::Float(_) => "float",
      Prim::String(_) => "string",
    }
  }
}

impl Display for Symbol {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    if self.module.is_empty() {
      return write!(f, "{}", self.local);
    } else {
      return write!(f, "{}_{}", self.module, self.local);
    }
  }
}

impl<S: AsRef<str>> From<S> for Symbol {
  fn from(qualified: S) -> Symbol {
    let mut iter = qualified.as_ref().rsplitn(2, "_");
    let local = String::from(
      iter
        .next()
        .expect("expected rsplitn to return at least 1 item"),
    );
    let module = String::from(iter.next().unwrap_or(""));
    return Symbol {
      module: module,
      local: local,
    };
  }
}

impl Symbol {
  pub fn main_group(module: String) -> Symbol {
    return Symbol {
      module: module,
      local: String::from("Main"),
    };
  }

  pub fn tuple_head() -> Symbol {
    return Symbol::from("T");
  }
}

impl Display for Value {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Value::Splice(idx) => write!(f, "\\{}", idx),
      Value::Prim(prim) => prim.fmt(f),
      Value::Record(Record { head, props }) => {
        write!(f, "{}[", head)?;
        let mut first = true;
        for prop in props {
          if first {
            first = false;
          } else {
            write!(f, ", ")?;
          }
          prop.fmt(f)?;
        }
        return write!(f, "]");
      }
    }
  }
}

#[allow(dead_code)]
impl Value {
  pub fn unit() -> Value {
    return Value::Record(Record {
      head: Symbol::from("Unit"),
      props: Vec::default(),
    });
  }

  pub fn true_() -> Value {
    return Value::Record(Record {
      head: Symbol::from("True"),
      props: Vec::default(),
    });
  }

  pub fn false_() -> Value {
    return Value::Record(Record {
      head: Symbol::from("False"),
      props: Vec::default(),
    });
  }

  pub fn nil() -> Value {
    return Value::Record(Record {
      head: Symbol::from("Nil"),
      props: Vec::default(),
    });
  }

  pub fn bool(x: bool) -> Value {
    if x {
      return Value::true_();
    } else {
      return Value::false_();
    }
  }

  pub fn cons(first: Value, rest: Value) -> Value {
    return Value::Record(Record {
      head: Symbol::from("Cons"),
      props: vec![first, rest],
    });
  }

  pub fn hole(idx: i32) -> Value {
    return Value::Record(Record {
      head: Symbol::from("Hole"),
      props: vec![Value::Prim(Prim::Integer(idx))],
    });
  }

  pub fn option(val: Option<Value>) -> Value {
    match val {
      None => {
        return Value::Record(Record {
          head: Symbol::from("None"),
          props: Vec::default(),
        });
      }
      Some(val) => {
        return Value::Record(Record {
          head: Symbol::from("Some"),
          props: vec![val],
        });
      }
    };
  }

  pub fn list<I: Iterator<Item = Value>>(vals: &mut I) -> Value {
    match vals.next() {
      None => return Value::nil(),
      Some(fst) => return Value::cons(fst, Value::list(vals)),
    };
  }

  pub fn record_head_to_fun(head: &String) -> Option<(String, String)> {
    if let Some(sep_pos) = head.chars().position(|c| c == '_') {
      let (lib, name) = head.split_at(sep_pos);
      let name = &name[1..];
      return Some((String::from(lib), String::from(name)));
    } else {
      return None;
    }
  }

  pub fn to_args(&self) -> Vec<Value> {
    if let Value::Record(Record { head, props }) = self {
      if head == &Symbol::tuple_head() {
        return props.clone();
      }
    }
    panic!("to_args: expected tuple, got: {}", self)
  }

  pub fn is_splice(&self) -> bool {
    if let Value::Splice(_) = self {
      return true;
    } else {
      return false;
    }
  }

  pub fn is_hole(&self) -> bool {
    if let Value::Record(Record { head, .. }) = self {
      return head == &Symbol::from("Hole");
    } else {
      return false;
    }
  }

  pub fn matches(&self, other: &Value) -> bool {
    if self.is_hole() {
      return true;
    }
    if let Value::Record(Record { head, props }) = self {
      if let Value::Record(Record {
        head: other_head,
        props: other_props,
      }) = other
      {
        return head == other_head
          && Iterator::zip(props.iter(), other_props.iter())
            .all(|(prop, other_prop)| prop.matches(other_prop));
      }
    } else {
      return self == other;
    }
    return false;
  }

  pub fn subst(&mut self, old: &Value, new: &Value) {
    if self == old {
      *self = new.clone();
    } else if let Value::Record(Record { props, .. }) = self {
      for prop in props {
        prop.subst(old, new);
      }
    }
  }

  pub fn fill_splices<F: Copy + Fn(usize) -> Value>(&mut self, f: F) {
    match self {
      Value::Splice(idx) => *self = f(*idx),
      Value::Prim(_) => (),
      Value::Record(Record { head: _, props }) => {
        for prop in props {
          prop.fill_splices(f);
        }
      }
    };
  }

  pub fn modify_children<F: FnMut(&mut Value) -> bool>(&mut self, f: &mut F) {
    if let Value::Record(Record { props, .. }) = self {
      for prop in props {
        if f(prop) {
          prop.modify_children(f);
        }
      }
    }
  }

  pub fn breadth_first(&self) -> GeneratorIterator<impl Generator<Yield = &Value, Return = ()>> {
    return GeneratorIterator(move || {
      if let Value::Record(Record { props, .. }) = self {
        let mut yielded = true;
        let mut level = 0;
        let mut props_stack: Vec<Iter<Value>> = Vec::new();
        while yielded {
          yielded = false;
          level = level + 1;
          props_stack.push(props.iter());
          while let Option::Some(top_props) = props_stack.last_mut() {
            if let Option::Some(top_prop) = top_props.next() {
              if props_stack.len() == level {
                yield top_prop;
                yielded = true;
              } else {
                if let Value::Record(Record {
                  props: top_sub_props,
                  ..
                }) = top_prop
                {
                  props_stack.push(top_sub_props.iter())
                }
              }
            } else {
              props_stack.pop();
            }
          }
        }
      }
    });
  }

  pub fn breadth_first_mut(
    &mut self,
  ) -> GeneratorIterator<impl Generator<Yield = &mut Value, Return = ()>> {
    return GeneratorIterator(move || unsafe {
      if let Value::Record(Record { props, .. }) = self {
        let props = props as *mut Vec<Value>;
        let mut yielded = true;
        let mut level = 0;
        let mut props_stack: Vec<IterMut<Value>> = Vec::new();
        while yielded {
          yielded = false;
          level = level + 1;
          props_stack.push((&mut *props).iter_mut());
          while let Option::Some(top_props) = props_stack.last_mut() {
            if let Option::Some(top_prop) = top_props.next() {
              if props_stack.len() == level {
                yield top_prop;
                yielded = true;
              } else {
                if let Value::Record(Record {
                  props: top_sub_props,
                  ..
                }) = top_prop
                {
                  props_stack.push(top_sub_props.iter_mut())
                }
              }
            } else {
              props_stack.pop();
            }
          }
        }
      }
    });
  }

  pub fn sub_splices(&self) -> Vec<usize> {
    let mut res: Vec<usize> = Vec::new();
    if let Value::Splice(idx) = self {
      res.push(*idx);
    }
    for child in self.breadth_first() {
      if let Value::Splice(idx) = child {
        res.push(*idx);
      }
    }
    return res;
  }

  pub fn any_sub<F: Fn(&Value) -> bool>(&self, pred: F) -> bool {
    if pred(self) {
      return true;
    }
    for child in self.breadth_first() {
      if pred(child) {
        return true;
      }
    }
    return false;
  }
}

#[test]
fn test_breadth_first_mut() {
  let mut x = Value::Record(Record {
    head: Symbol::from("Foo"),
    props: vec![
      Value::Record(Record {
        head: Symbol::from("Bar"),
        props: vec![
          Value::Record(Record {
            head: Symbol::from("Baz"),
            props: vec![Value::Prim(Prim::Integer(2)), Value::Prim(Prim::Integer(3))],
          }),
          Value::Prim(Prim::Integer(1)),
          Value::Record(Record {
            head: Symbol::from("Qux"),
            props: vec![Value::Prim(Prim::Integer(4))],
          }),
        ],
      }),
      Value::Prim(Prim::Integer(0)),
    ],
  });
  for i in 0..5 {
    let mut cur_idx = i;
    for prop in x.breadth_first_mut() {
      if let Value::Prim(Prim::Integer(idx)) = prop {
        if *idx >= 0 {
          assert_eq!(cur_idx, *idx);
          *prop = Value::Record(Record {
            head: Symbol::from("Lower"),
            props: vec![Value::Prim(Prim::Integer(-(*idx + 1)))],
          });
          cur_idx = cur_idx + 1;
        }
      }
    }
    for prop in x.breadth_first_mut() {
      if let Value::Prim(Prim::Integer(idx)) = prop {
        if *idx < 0 {
          *idx = -*idx;
        }
      }
    }
  }
}
