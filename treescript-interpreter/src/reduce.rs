extern crate enum_map;
extern crate serde;
use crate::value::{Prim, Value};
use enum_map::{Enum, EnumMap};
use serde::{Deserialize, Deserializer, Serialize, Serializer};

const MAX_NUM_BINDS: usize = 32;

pub type Binds = [Option<Value>; MAX_NUM_BINDS];

#[derive(Clone, Copy, Debug, Deserialize, Enum, Serialize)]
pub enum ReduceType {
  Regular,
  EvalCtx,
  AltConsume,
}

pub type ReduceResult = bool;

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum Consume {
  Bind(usize),
  Prim(Prim),
  Record(String),
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ReducerClause {
  pub consumes: Vec<Consume>,
  pub produce: Value,
  pub groups: Vec<Group>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Reducer {
  pub input: ReducerClause,
  pub output: ReducerClause,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum Statement {
  Reducer(Reducer),
  Group(Group),
}

#[derive(Clone, Debug)]
pub struct Group {
  pub loops: bool,
  pub statements: EnumMap<ReduceType, Vec<Statement>>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
struct GroupSerial {
  pub loops: bool,
  pub statements: Vec<Vec<Statement>>,
}

impl From<GroupSerial> for Group {
  fn from(x: GroupSerial) -> Group {
    return Group {
      loops: x.loops,
      statements: EnumMap::from(|typ| {
        if x.statements.len() > typ as usize {
          return x.statements[typ as usize].clone();
        } else {
          return Vec::new();
        }
      }),
    };
  }
}

impl From<Group> for GroupSerial {
  fn from(x: Group) -> GroupSerial {
    return GroupSerial {
      loops: x.loops,
      statements: Vec::from(x.statements.as_slice()),
    };
  }
}

impl Serialize for Group {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    return GroupSerial::from(self.clone()).serialize(serializer);
  }
}

impl<'de> Deserialize<'de> for Group {
  fn deserialize<D>(deserializer: D) -> Result<Group, D::Error>
  where
    D: Deserializer<'de>,
  {
    return GroupSerial::deserialize(deserializer).map(|x| Group::from(x));
  }
}

impl Group {
  /*
    fn apply_consume_direct(
    &self,
    consumed: &Value,
    stack: &mut Vec<Value>,
    binds: &mut Binds,
    consume: &Consume,
  ) -> ReduceResult {
    let consumed = consumed.clone();
    match consume.clone() {
      Consume::Prim(in_prim) => {
        return consumed == Value::Prim(in_prim);
      }
      Consume::Record(in_head) => {
        if let Value::Record {
          head: consumed_head,
          props: mut consumed_props,
        } = consumed
        {
          if consumed_head == in_head {
            stack.append(&mut consumed_props);
            return true;
          } else {
            return false;
          }
        } else {
          return false;
        }
      }
      Consume::Bind(idx) => {
        if binds[idx] == Option::None {
          binds[idx] = Option::Some(consumed);
          return true;
        } else if binds[idx] == Option::Some(consumed) {
          return true;
        } else {
          return false;
        }
      }
    }
  }

  fn apply_consume(
    &self,
    stack: &mut Vec<Value>,
    binds: &mut Binds,
    consume: &Consume,
  ) -> ReduceResult {
    let consumed = stack.pop().unwrap();
    if self.apply_consume_direct(&consumed, stack, binds, consume) {
      return true;
    }

    let mut alt_out = Value::Record {
      head: String::from("C"),
      props: vec![consumed.clone()],
    };
    for alt_out_stmt in &self.statements[ReduceType::AltConsume] {
      if self.apply_statement(binds, &mut alt_out, alt_out_stmt) {
        for alt_in in alt_out.breadth_first_mut() {
          if let Value::Record { head, props } = alt_in {
            if head == "C" {
              *alt_in = props.first().unwrap().clone();
              if self.apply_consume_direct(&alt_in, stack, binds, consume) {
                return true;
              }
              break;
            }
          }
        }
      }
    }

    return false;
  }
  */

  fn apply_consume(
    &self,
    stack: &mut Vec<Value>,
    binds: &mut Binds,
    consume: &Consume,
  ) -> ReduceResult {
    let consumed = stack.pop().unwrap();
    match consume.clone() {
      Consume::Prim(in_prim) => {
        return consumed == Value::Prim(in_prim);
      }
      Consume::Record(in_head) => {
        if let Value::Record {
          head: consumed_head,
          props: mut consumed_props,
        } = consumed
        {
          if consumed_head == in_head {
            consumed_props.reverse();
            stack.append(&mut consumed_props);
            return true;
          } else {
            return false;
          }
        } else {
          return false;
        }
      }
      Consume::Bind(idx) => {
        if idx == 0 {
          return true;
        } else {
          let idx = idx - 1;
          if binds[idx] == Option::None {
            binds[idx] = Option::Some(consumed);
            return true;
          } else if binds[idx] == Option::Some(consumed) {
            return true;
          } else {
            return false;
          }
        }
      }
    }
  }

  fn apply_produce(&self, binds: &Binds, produce: &Value) -> Value {
    let mut res = produce.clone();
    for idx in 0..MAX_NUM_BINDS {
      if let Option::Some(bind) = &binds[idx] {
        res.subst(&Value::Splice(idx + 1), bind);
      }
    }
    return res;
  }

  fn guard_clause(&self, binds: &mut Binds, x: &Value, clause: &ReducerClause) -> ReduceResult {
    if x.has_splice() {
      return true;
    }

    let mut stack = vec![x.clone()];
    for consume in &clause.consumes {
      if !self.apply_consume(&mut stack, binds, &consume) {
        return false;
      }
    }
    for group in &clause.groups {
      if !group.guard(binds) {
        return false;
      }
    }
    return true;
  }

  fn apply_clause(&self, binds: &Binds, clause: &ReducerClause) -> Value {
    let mut out = self.apply_produce(&binds, &clause.produce);
    for group in &clause.groups {
      group.reduce_nested(binds, &mut out);
    }
    return out;
  }

  fn guard_reducer(&self, binds: &mut Binds, reducer: &Reducer) -> ReduceResult {
    let out = self.apply_clause(binds, &reducer.input);
    return self.guard_clause(binds, &out, &reducer.output);
  }

  fn apply_reducer(&self, in_binds: &Binds, x: &mut Value, reducer: &Reducer) -> ReduceResult {
    let mut binds = in_binds.clone();
    if !self.guard_clause(&mut binds, x, &reducer.input) {
      return false;
    }
    *x = self.apply_clause(&binds, &reducer.output);
    return true;
  }

  fn apply_statement(
    &self,
    in_binds: &Binds,
    x: &mut Value,
    statement: &Statement,
  ) -> ReduceResult {
    match statement {
      Statement::Reducer(reducer) => return self.apply_reducer(in_binds, x, reducer),
      Statement::Group(group) => return group.reduce_nested(in_binds, x),
    }
  }

  fn guard_statement(&self, binds: &mut Binds, statement: &Statement) -> ReduceResult {
    match statement {
      Statement::Reducer(reducer) => return self.guard_reducer(binds, reducer),
      Statement::Group(group) => return group.guard(binds),
    }
  }

  fn guard(&self, binds: &mut Binds) -> ReduceResult {
    for stmt in &self.statements[ReduceType::Regular] {
      if !self.guard_statement(binds, &stmt) {
        return false;
      }
    }
    return true;
  }

  fn reduce_nested(&self, in_binds: &Binds, x: &mut Value) -> ReduceResult {
    let mut progress = false;
    for stmt in self.statements[ReduceType::Regular].iter() {
      let mut reduce_res = self.apply_statement(in_binds, x, stmt);
      if !reduce_res {
        let mut alt_out = Value::Record {
          head: String::from("C"),
          props: vec![x.clone()],
        };
        for alt_out_stmt in &self.statements[ReduceType::AltConsume] {
          if self.apply_statement(in_binds, &mut alt_out, alt_out_stmt) {
            for alt_in in alt_out.breadth_first_mut() {
              if let Value::Record { head, props } = alt_in {
                if head == "C" {
                  *alt_in = props.first().unwrap().clone();
                  if self.apply_statement(in_binds, alt_in, stmt) {
                    reduce_res = true;
                  }
                  break;
                }
              }
            }
            if reduce_res {
              *x = alt_out;
              break;
            }
          }
        }
      }
      if reduce_res {
        progress = true;
        if self.loops {
          self.reduce_nested(in_binds, x);
          return true;
        }
      }
    }
    for alt_out_stmt in self.statements[ReduceType::EvalCtx].iter() {
      let mut alt_out = Value::Record {
        head: String::from("E"),
        props: vec![x.clone()],
      };
      if self.apply_statement(in_binds, &mut alt_out, &alt_out_stmt) {
        alt_out.modify_children(&mut |alt_in| {
          if let Value::Record { head, props: p } = alt_in {
            if head == "E" {
              *alt_in = p.first().unwrap().clone();
              if self.reduce_nested(in_binds, alt_in) {
                progress = true;
              }
              return false;
            }
          }
          return true;
        });
        if progress {
          *x = alt_out;
          if self.loops {
            self.reduce_nested(in_binds, x);
            return true;
          }
          break;
        }
      }
    }
    return progress;
  }

  pub fn reduce(&self, x: &mut Value) -> ReduceResult {
    let binds: Binds = Default::default();
    return self.reduce_nested(&binds, x);
  }
}
