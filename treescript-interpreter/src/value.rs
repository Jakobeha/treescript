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
pub enum Value {
  Splice(usize),
  Prim(Prim),
  Record { head: String, props: Vec<Value> },
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

#[allow(dead_code)]
impl Value {
  pub fn unit() -> Value {
    return Value::Record {
      head: String::from("Unit"),
      props: Vec::default(),
    };
  }

  pub fn true_() -> Value {
    return Value::Record {
      head: String::from("True"),
      props: Vec::default(),
    };
  }

  pub fn false_() -> Value {
    return Value::Record {
      head: String::from("False"),
      props: Vec::default(),
    };
  }

  pub fn nil() -> Value {
    return Value::Record {
      head: String::from("Nil"),
      props: Vec::default(),
    };
  }

  pub fn bool(x: bool) -> Value {
    if x {
      return Value::true_();
    } else {
      return Value::false_();
    }
  }

  pub fn cons(first: Value, rest: Value) -> Value {
    return Value::Record {
      head: String::from("Cons"),
      props: vec![first, rest],
    };
  }

  pub fn hole(idx: i32) -> Value {
    return Value::Record {
      head: String::from("Hole"),
      props: vec![Value::Prim(Prim::Integer(idx))],
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

  pub fn is_hole(&self) -> bool {
    if let Value::Record { head, .. } = self {
      return head == "Hole";
    } else {
      return false;
    }
  }

  pub fn matches(&self, other: &Value) -> bool {
    return self.is_hole() || self == other;
  }

  pub fn subst(&mut self, old: &Value, new: &Value) {
    if self == old {
      *self = new.clone();
    } else if let Value::Record { props, .. } = self {
      for prop in props {
        prop.subst(old, new);
      }
    }
  }

  pub fn modify_children<F: FnMut(&mut Value) -> bool>(&mut self, f: &mut F) {
    if let Value::Record { props, .. } = self {
      for prop in props {
        if f(prop) {
          prop.modify_children(f);
        }
      }
    }
  }

  pub fn breadth_first(&self) -> GeneratorIterator<impl Generator<Yield = &Value, Return = ()>> {
    return GeneratorIterator(move || {
      if let Value::Record { props, .. } = self {
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
                if let Value::Record {
                  props: top_sub_props,
                  ..
                } = top_prop
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
      if let Value::Record { props, .. } = self {
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
                if let Value::Record {
                  props: top_sub_props,
                  ..
                } = top_prop
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
  let mut x = Value::Record {
    head: String::from("Foo"),
    props: vec![
      Value::Record {
        head: String::from("Bar"),
        props: vec![
          Value::Record {
            head: String::from("Baz"),
            props: vec![Value::Prim(Prim::Integer(2)), Value::Prim(Prim::Integer(3))],
          },
          Value::Prim(Prim::Integer(1)),
          Value::Record {
            head: String::from("Qux"),
            props: vec![Value::Prim(Prim::Integer(4))],
          },
        ],
      },
      Value::Prim(Prim::Integer(0)),
    ],
  };
  for i in 0..5 {
    let mut cur_idx = i;
    for prop in x.breadth_first_mut() {
      if let Value::Prim(Prim::Integer(idx)) = prop {
        if *idx >= 0 {
          assert_eq!(cur_idx, *idx);
          *prop = Value::Record {
            head: String::from("Lower"),
            props: vec![Value::Prim(Prim::Integer(-(*idx + 1)))],
          };
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
