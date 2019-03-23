extern crate enum_map;
extern crate serde;
extern crate serde_repr;
use crate::session::Session;
use crate::value::{Prim, Value};
use enum_map::{Enum, EnumMap};
use serde::{Deserialize, Serialize};
use serde_repr::{Deserialize_repr, Serialize_repr};
use std::collections::{HashSet, VecDeque};
use std::iter::FromIterator;
use std::ops::{Index, IndexMut};
use std::rc::Weak;

const MAX_NUM_BINDS: usize = 32;

struct BindFrame {
  local: [Option<Value>; MAX_NUM_BINDS],
  parent_idxs: HashSet<usize>,
}

struct BindStack(VecDeque<BindFrame>);

#[derive(Clone, Copy, Debug, Deserialize, Enum, Eq, PartialEq, Serialize)]
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
  Function { head: String, props: Vec<Value> },
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

#[derive(Clone, Debug, Deserialize_repr, Serialize_repr)]
#[repr(usize)]
pub enum GroupMode {
  Continue,
  Stop,
  Loop,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GroupDefSerial {
  pub props: Vec<usize>,
  pub mode: GroupMode,
  pub statements: Vec<Vec<Statement>>,
}

#[derive(Clone, Debug)]
pub struct GroupDef {
  pub props: Vec<usize>,
  pub mode: GroupMode,
  pub statements: EnumMap<ReduceType, Vec<Statement>>,
  pub env: Weak<Vec<GroupDef>>,
}

impl BindFrame {
  fn new<'a, I: IntoIterator<Item = &'a usize>>(parent_idxs: I) -> BindFrame {
    return BindFrame {
      local: Default::default(),
      parent_idxs: HashSet::from_iter(parent_idxs.into_iter().map(|idx| *idx)),
    };
  }
}

impl Index<usize> for BindStack {
  type Output = Option<Value>;

  fn index(&self, idx: usize) -> &Option<Value> {
    if idx == 0 {
      panic!("can't reference bind 0")
    }
    for frame in self.0.iter() {
      if !frame.parent_idxs.contains(&idx) {
        return &frame.local[idx - 1];
      }
      dprint!("PRINT_FRAME", "index {} fell\n", idx);
    }
    panic!("root (undefined) index: {}", idx);
  }
}

impl IndexMut<usize> for BindStack {
  fn index_mut(&mut self, idx: usize) -> &mut Option<Value> {
    if idx == 0 {
      panic!("can't reference bind 0")
    }
    dprint!("PRINT_FRAME", "index_mut: {}", idx);
    for frame in self.0.iter_mut() {
      if !frame.parent_idxs.contains(&idx) {
        dprint!("PRINT_FRAME", "\n");
        return &mut frame.local[idx - 1];
      }
      dprint!("PRINT_FRAME", "+");
    }
    panic!("root (undefined) index: {}", idx);
  }
}

impl BindStack {
  fn root() -> BindStack {
    return BindStack(VecDeque::new());
  }

  fn push<'a, I: IntoIterator<Item = &'a usize>>(&mut self, parent_idxs: I) {
    self.0.push_front(BindFrame::new(parent_idxs));
    dprint!(
      "PRINT_FRAME",
      "pushing: {:?}\n",
      self.0.front().unwrap().parent_idxs
    );
  }

  fn pop(&mut self) {
    dprint!(
      "PRINT_FRAME",
      "popping: {:?}\n",
      self.0.front().unwrap().parent_idxs
    );
    self.0.pop_front().expect("no frames left to pop");
  }
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
      mode: x.mode,
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

  fn subst_props<I: IntoIterator<Item = Value>>(&mut self, in_props: I) {
    let old_props = self.props.clone();
    self.props = Vec::new();
    for (prop_idx, in_prop) in Iterator::zip(old_props.into_iter(), in_props) {
      let mut in_prop_idxs = in_prop.sub_splices();
      self.props.append(&mut in_prop_idxs);
      self.subst_bind(prop_idx, &in_prop)
    }
  }

  fn resolve<F: FnMut(&Value) -> Value>(
    &self,
    group_ref: &GroupRef,
    prop_transformer: F,
  ) -> GroupDef {
    let mut group = self.env.upgrade().unwrap()[group_ref.idx].clone();
    dprint!("PRINT_REDUCE", "Resolving {:?}\n", group_ref.idx);
    let props = group_ref.props.iter().map(prop_transformer);
    group.subst_props(props);
    return group;
  }

  fn apply_if_function(&self, session: &mut Session, x: &mut Value) {
    if let Value::Record { head, props } = x {
      if head == "#Flush" {
        *x = props[0].clone();
        x.flush();
      } else if head.starts_with("#") {
        let head = String::from(&head[1..]);
        *x = session.call_fun_val(head, props.clone());
      }
    }
  }

  fn apply_functions(&self, session: &mut Session, x: &mut Value) {
    if let Value::Record { head: _, props } = x {
      for sub in props {
        self.apply_functions(session, sub);
      }
    }
    self.apply_if_function(session, x);
  }

  fn apply_consume(
    &self,
    session: &mut Session,
    stack: &mut Vec<Value>,
    binds: &mut BindStack,
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
          let bind = binds.index_mut(idx);
          if bind == &Option::None {
            *bind = Option::Some(consumed);
            return true;
          } else if bind == &Option::Some(consumed) {
            return true;
          } else {
            return false;
          }
        }
      }
      Consume::Function { head, props } => {
        let mut out = session.call_fun_val(head, props);
        self.apply_functions(session, &mut out);
        return consumed == out;
      }
    }
  }

  fn apply_produce(&self, session: &mut Session, binds: &mut BindStack, produce: &Value) -> Value {
    let mut res = produce.clone();
    //Keep raw splices, don't error
    res.fill_splices(|idx| binds[idx].as_ref().cloned().unwrap_or(Value::Splice(idx)));
    self.apply_functions(session, &mut res);
    return res;
  }

  fn guard_clause(
    &self,
    session: &mut Session,
    binds: &mut BindStack,
    x: &Value,
    clause: &ReducerClause,
  ) -> ReduceResult {
    dprint!("PRINT_REDUCE", "Consuming {} ", clause.produce);
    let mut stack = vec![x.clone()];
    for consume in &clause.consumes {
      if !self.apply_consume(session, &mut stack, binds, &consume) {
        dprint!("PRINT_REDUCE", "- Failed\n");
        return false;
      }
    }
    for group_ref in &clause.groups {
      dprint!("PRINT_REDUCE", "+ Group");
      if !self
        .resolve(group_ref, |prop| self.apply_produce(session, binds, prop))
        .guard(session, binds)
      {
        dprint!("PRINT_REDUCE", "- Failed\n");
        return false;
      }
    }
    dprint!("PRINT_REDUCE", "- Succeeded\n");
    return true;
  }

  fn apply_clause(
    &self,
    session: &mut Session,
    binds: &mut BindStack,
    clause: &ReducerClause,
  ) -> Value {
    dprint!("PRINT_REDUCE", "Producing {}\n", clause.produce);
    let mut out = self.apply_produce(session, binds, &clause.produce);
    for group_ref in &clause.groups {
      self
        .resolve(group_ref, |prop| self.apply_produce(session, binds, prop))
        .reduce_nested(session, binds, ReduceType::Regular, &mut out);
    }
    return out;
  }

  fn guard_reducer(
    &self,
    session: &mut Session,
    binds: &mut BindStack,
    reducer: &Reducer,
  ) -> ReduceResult {
    let mut out = self.apply_produce(session, binds, &reducer.input.produce);
    if out.any_sub(|sub| sub.is_splice()) {
      return true;
    }
    for group_ref in &reducer.input.groups {
      self
        .resolve(group_ref, |prop| self.apply_produce(session, binds, prop))
        .reduce_nested(session, binds, ReduceType::Regular, &mut out);
    }
    binds.push(&self.props);
    let res = self.guard_clause(session, binds, &out, &reducer.output);
    binds.pop();
    return res;
  }

  fn apply_reducer(
    &self,
    session: &mut Session,
    in_binds: &mut BindStack,
    x: &mut Value,
    reducer: &Reducer,
  ) -> ReduceResult {
    in_binds.push(&self.props);
    if !self.guard_clause(session, in_binds, x, &reducer.input) {
      in_binds.pop();
      return false;
    }
    *x = self.apply_clause(session, in_binds, &reducer.output);
    in_binds.pop();
    return true;
  }

  fn apply_statement(
    &self,
    session: &mut Session,
    in_binds: &mut BindStack,
    reduce_type: ReduceType,
    x: &mut Value,
    statement: &Statement,
  ) -> ReduceResult {
    match statement {
      Statement::Reducer(reducer) => return self.apply_reducer(session, in_binds, x, reducer),
      Statement::Group(group_ref) => {
        //clone is probably unnecessary
        return self.resolve(group_ref, |prop| prop.clone()).reduce_nested(
          session,
          in_binds,
          reduce_type,
          x,
        );
      }
    }
  }

  fn guard_statement(
    &self,
    session: &mut Session,
    binds: &mut BindStack,
    statement: &Statement,
  ) -> ReduceResult {
    match statement {
      Statement::Reducer(reducer) => return self.guard_reducer(session, binds, reducer),
      Statement::Group(group_ref) => {
        //clone is probably unnecessary
        return self
          .resolve(group_ref, |prop| prop.clone())
          .guard(session, binds);
      }
    }
  }

  fn guard(&self, session: &mut Session, binds: &mut BindStack) -> ReduceResult {
    for stmt in &self.statements[ReduceType::Regular] {
      if !self.guard_statement(session, binds, &stmt) {
        return false;
      }
    }
    return true;
  }

  fn reduce_nested(
    &self,
    session: &mut Session,
    in_binds: &mut BindStack,
    reduce_type: ReduceType,
    x: &mut Value,
  ) -> ReduceResult {
    dprint!("PRINT_REDUCE", "Reducing {}\n", x);
    let mut progress = false;
    for stmt in self.statements[reduce_type].iter() {
      let mut reduce_res = self.apply_statement(session, in_binds, reduce_type, x, stmt);
      if !reduce_res {
        let mut alt_out = Value::Record {
          head: String::from("C"),
          props: vec![x.clone()],
        };
        for alt_out_stmt in &self.statements[ReduceType::AltConsume] {
          if self.apply_statement(session, in_binds, reduce_type, &mut alt_out, alt_out_stmt) {
            for alt_in in alt_out.breadth_first_mut() {
              if let Value::Record { head, props } = alt_in {
                if head == "C" {
                  *alt_in = props.first().unwrap().clone();
                  if self.apply_statement(session, in_binds, reduce_type, alt_in, stmt) {
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
        match self.mode {
          GroupMode::Continue => (),
          GroupMode::Stop => return true,
          GroupMode::Loop => {
            self.reduce_nested(session, in_binds, reduce_type, x);
            return true;
          }
        };
      }
    }
    if reduce_type == ReduceType::Regular {
      for alt_out_stmt in self.statements[ReduceType::EvalCtx].iter() {
        let mut alt_out = Value::Record {
          head: String::from("E"),
          props: vec![x.clone()],
        };
        if self.apply_statement(
          session,
          in_binds,
          ReduceType::EvalCtx,
          &mut alt_out,
          &alt_out_stmt,
        ) {
          alt_out.modify_children(&mut |alt_in| {
            if let Value::Record { head, props: p } = alt_in {
              if head == "E" {
                *alt_in = p.first().unwrap().clone();
                if self.reduce_nested(session, in_binds, reduce_type, alt_in) {
                  progress = true;
                }
                return false;
              }
            }
            return true;
          });
          if progress {
            *x = alt_out;
            match self.mode {
              GroupMode::Continue => (),
              GroupMode::Stop => return true,
              GroupMode::Loop => {
                self.reduce_nested(session, in_binds, reduce_type, x);
                return true;
              }
            };
            break;
          }
        }
      }
    }
    return progress;
  }

  pub fn reduce(&self, session: &mut Session, x: &mut Value) -> ReduceResult {
    assert!(
      self.props.is_empty(),
      "can't reduce as initial group, not the initial group, has properties"
    );
    return self.reduce_nested(session, &mut BindStack::root(), ReduceType::Regular, x);
  }
}
