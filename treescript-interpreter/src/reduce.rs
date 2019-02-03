extern crate enum_map;
extern crate serde;
use crate::value::{Prim, Value};
use enum_map::{Enum, EnumMap};
use serde::{Deserialize, Serialize};
use std::rc::Weak;

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
pub struct GroupRef {
  pub idx: usize,
  pub props: Vec<Value>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ReducerClause {
  pub consumes: Vec<Consume>,
  pub produce: Value,
  pub groups: Vec<GroupRef>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Reducer {
  pub input: ReducerClause,
  pub output: ReducerClause,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum Statement {
  Reducer(Reducer),
  Group(GroupRef),
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GroupDefSerial {
  pub props: Vec<usize>,
  pub loops: bool,
  pub statements: Vec<Vec<Statement>>,
}

#[derive(Clone, Debug)]
pub struct GroupDef {
  pub props: Vec<usize>,
  pub loops: bool,
  pub statements: EnumMap<ReduceType, Vec<Statement>>,
  pub env: Weak<Vec<GroupDef>>,
}

impl Consume {
  fn seq_from(value: Value) -> Vec<Consume> {
    match value {
      Value::Splice(idx) => vec![Consume::Bind(idx)],
      Value::Prim(prim) => vec![Consume::Prim(prim)],
      Value::Record { head, props } => {
        let mut res = vec![Consume::Record(head)];
        for prop in props {
          res.append(&mut Consume::seq_from(prop));
        }
        return res;
      }
    }
  }

  fn subst_bind(self, old: usize, new: &Value) -> Vec<Consume> {
    match self {
      Consume::Bind(idx) if idx == old => return Consume::seq_from(new.clone()),
      _ => return vec![self],
    }
  }
}

impl GroupRef {
  fn subst_bind(&mut self, old: usize, new: &Value) {
    for prop in self.props.iter_mut() {
      prop.subst(&Value::Splice(old), new);
    }
  }
}

impl ReducerClause {
  fn subst_bind(&mut self, old: usize, new: &Value) {
    self.consumes = self
      .consumes
      .iter()
      .flat_map(|consume| consume.clone().subst_bind(old, new))
      .collect();
    self.produce.subst(&Value::Splice(old), new);
    for group_ref in self.groups.iter_mut() {
      group_ref.subst_bind(old, new);
    }
  }
}

impl Reducer {
  fn subst_bind(&mut self, old: usize, new: &Value) {
    self.input.subst_bind(old, new);
    self.output.subst_bind(old, new);
  }
}

impl Statement {
  fn subst_bind(&mut self, old: usize, new: &Value) {
    match self {
      Statement::Reducer(reducer) => reducer.subst_bind(old, new),
      Statement::Group(group_ref) => group_ref.subst_bind(old, new),
    }
  }
}

impl GroupDef {
  pub fn from_no_env(x: GroupDefSerial) -> GroupDef {
    let stmts = x.statements;
    return GroupDef {
      props: x.props,
      loops: x.loops,
      statements: EnumMap::from(|typ| stmts[typ as usize].clone()),
      env: Weak::new(),
    };
  }

  fn subst_bind(&mut self, old: usize, new: &Value) {
    for (_, stmts) in self.statements.iter_mut() {
      for stmt in stmts {
        stmt.subst_bind(old, new);
      }
    }
  }

  fn subst_props(&mut self, in_props: &Vec<Value>) {
    assert!(in_props.len() == self.props.len());

    for (in_prop, prop_idx) in Iterator::zip(in_props.iter(), self.props.clone().into_iter()) {
      self.subst_bind(prop_idx, in_prop);
    }
  }

  fn resolve(&self, group_ref: &GroupRef) -> GroupDef {
    let mut group = self.env.upgrade().unwrap()[group_ref.idx].clone();
    group.subst_props(&group_ref.props);
    return group;
  }

  fn is_unset_splice(&self, binds: &Binds, x: &Value) -> bool {
    if let Value::Splice(idx) = x {
      return *idx == 0 || binds[idx - 1].is_none();
    } else {
      return false;
    }
  }

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
    let mut stack = vec![x.clone()];
    for consume in &clause.consumes {
      if !self.apply_consume(&mut stack, binds, &consume) {
        return false;
      }
    }
    for group_ref in &clause.groups {
      if !self.resolve(group_ref).guard(binds) {
        return false;
      }
    }
    return true;
  }

  fn apply_clause(&self, binds: &Binds, clause: &ReducerClause) -> Value {
    let mut out = self.apply_produce(&binds, &clause.produce);
    for group_ref in &clause.groups {
      self.resolve(group_ref).reduce_nested(binds, &mut out);
    }
    return out;
  }

  fn guard_reducer(&self, binds: &mut Binds, reducer: &Reducer) -> ReduceResult {
    let mut out = self.apply_produce(&binds, &reducer.input.produce);
    if out.any_sub(|sub| self.is_unset_splice(binds, sub)) {
      return true;
    }
    for group_ref in &reducer.input.groups {
      self.resolve(group_ref).reduce_nested(binds, &mut out);
    }
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
      Statement::Group(group_ref) => return self.resolve(group_ref).reduce_nested(in_binds, x),
    }
  }

  fn guard_statement(&self, binds: &mut Binds, statement: &Statement) -> ReduceResult {
    match statement {
      Statement::Reducer(reducer) => return self.guard_reducer(binds, reducer),
      Statement::Group(group_ref) => return self.resolve(group_ref).guard(binds),
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
    assert!(self.props.is_empty());
    let binds: Binds = Default::default();
    return self.reduce_nested(&binds, x);
  }
}
