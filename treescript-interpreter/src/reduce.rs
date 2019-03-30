extern crate enum_map;
extern crate serde;
use crate::session::Session;
use crate::value::{Prim, Value};
use enum_map::{Enum, EnumMap};
use serde::{Deserialize, Serialize};
use std::borrow::Borrow;
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
  pub is_prop: bool,
  pub idx: usize,
  pub group_props: Vec<GroupRef>,
  pub value_props: Vec<Value>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Reducer {
  pub input: Vec<Consume>,
  pub output: Value,
  pub nexts: Vec<GroupRef>,
  pub guards: Vec<Statement>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum Statement {
  Reducer(Reducer),
  Group(GroupRef),
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GroupDefSerial {
  pub group_props: Vec<usize>,
  pub value_props: Vec<usize>,
  pub statements: Vec<Vec<Statement>>,
}

#[derive(Clone, Debug)]
pub struct GroupDef {
  pub group_props: Vec<usize>,
  pub value_props: Vec<usize>,
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
  fn sub_group_props(&self) -> Vec<usize> {
    let mut res: Vec<usize> = Vec::new();
    if self.is_prop {
      res.push(self.idx);
    }
    for prop in self.group_props.iter() {
      res.append(&mut prop.sub_group_props());
    }
    return res;
  }

  fn subst_bind(&mut self, old: usize, new: &Value) {
    for prop in self.group_props.iter_mut() {
      prop.subst_bind(old, new);
    }
    for prop in self.value_props.iter_mut() {
      prop.subst(&Value::Splice(old), new);
    }
  }

  fn subst_group_prop(&mut self, old: usize, new: &GroupRef) {
    for prop in self.group_props.iter_mut() {
      prop.subst_group_prop(old, new);
    }
    // Subst this group specifically - WARN: must be after?
    if self.is_prop && self.idx == old {
      let mut old_group_props = self.group_props.clone();
      let mut old_value_props = self.value_props.clone();
      *self = new.clone();
      self.group_props.append(&mut old_group_props);
      self.value_props.append(&mut old_value_props);
    }
  }

  fn map_values<F: FnMut(&Value) -> Value>(&self, f: F) -> GroupRef {
    fn run<F: FnMut(&Value) -> Value>(this: &GroupRef, mut f: std::rc::Rc<F>) -> GroupRef {
      return GroupRef {
        is_prop: this.is_prop,
        idx: this.idx,
        group_props: this
          .group_props
          .iter()
          .map(|group| run(group, f.clone()))
          .collect(),
        value_props: this
          .value_props
          .iter()
          .map(std::rc::Rc::get_mut(&mut f).unwrap())
          .collect(),
      };
    }

    return run(self, std::rc::Rc::new(f));
  }
}

impl Reducer {
  fn subst_bind(&mut self, old: usize, new: &Value) {
    self.input = self
      .input
      .iter()
      .flat_map(|consume| consume.clone().subst_bind(old, new))
      .collect();
    self.output.subst(&Value::Splice(old), new);
    for next in self.nexts.iter_mut() {
      next.subst_bind(old, new);
    }
    for guard in self.guards.iter_mut() {
      guard.subst_bind(old, new);
    }
  }

  fn subst_group_prop(&mut self, old: usize, new: &GroupRef) {
    for next in self.nexts.iter_mut() {
      next.subst_group_prop(old, new);
    }
    for guard in self.guards.iter_mut() {
      guard.subst_group_prop(old, new);
    }
  }
}

impl Statement {
  fn subst_bind(&mut self, old: usize, new: &Value) {
    match self {
      Statement::Reducer(reducer) => reducer.subst_bind(old, new),
      Statement::Group(group_ref) => group_ref.subst_bind(old, new),
    }
  }

  fn subst_group_prop(&mut self, old: usize, new: &GroupRef) {
    match self {
      Statement::Reducer(reducer) => reducer.subst_group_prop(old, new),
      Statement::Group(group_ref) => group_ref.subst_group_prop(old, new),
    }
  }
}

impl GroupDef {
  pub fn from_no_env(x: GroupDefSerial) -> GroupDef {
    let stmts = x.statements;
    return GroupDef {
      group_props: x.group_props,
      value_props: x.value_props,
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

  fn subst_group_prop(&mut self, old: usize, new: &GroupRef) {
    for (_, stmts) in self.statements.iter_mut() {
      for stmt in stmts {
        stmt.subst_group_prop(old, new);
      }
    }
  }

  fn subst_group_props<G: Borrow<GroupRef>, I: IntoIterator<Item = G>>(&mut self, in_props: I) {
    let old_props = self.group_props.clone();
    self.group_props = Vec::new();
    for (prop_idx, in_prop) in Iterator::zip(old_props.into_iter(), in_props) {
      let in_prop = in_prop.borrow();
      let mut in_prop_idxs = in_prop.sub_group_props();
      self.group_props.append(&mut in_prop_idxs);
      self.subst_group_prop(prop_idx, in_prop)
    }
  }

  fn subst_value_props<V: Borrow<Value>, I: IntoIterator<Item = V>>(&mut self, in_props: I) {
    let old_props = self.value_props.clone();
    self.value_props = Vec::new();
    for (prop_idx, in_prop) in Iterator::zip(old_props.into_iter(), in_props) {
      let in_prop = in_prop.borrow();
      let mut in_prop_idxs = in_prop.sub_splices();
      self.value_props.append(&mut in_prop_idxs);
      dprint!(
        "PRINT_REDUCE",
        "Substituting {:?} with {}",
        prop_idx,
        in_prop
      );
      self.subst_bind(prop_idx, in_prop);
    }
  }

  fn resolve(
    &self,
    session: &mut Session,
    binds: &mut BindStack,
    group_ref: &GroupRef,
  ) -> GroupDef {
    let mut group = self.env.upgrade().unwrap()[group_ref.idx].clone();
    dprint!("PRINT_REDUCE", "Resolving {:?} [", group_ref.idx);
    let group_props = group_ref
      .group_props
      .iter()
      .map(|gprop| gprop.map_values(|val| self.apply_produce(session, binds, val)));
    group.subst_group_props(group_props);
    let value_props = group_ref
      .value_props
      .iter()
      .map(|val| self.apply_produce(session, binds, val));
    group.subst_value_props(value_props);
    dprint!("PRINT_REDUCE", "] done resolving {:?}\n", group_ref.idx);
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

  fn apply_produce(&self, session: &mut Session, binds: &mut BindStack, output: &Value) -> Value {
    dprint!("PRINT_REDUCE", "Producing {} -> ", output);
    let mut res = output.clone();
    //Keep raw splices, don't error
    res.fill_splices(|idx| binds[idx].as_ref().cloned().unwrap_or(Value::Splice(idx)));
    self.apply_functions(session, &mut res);
    dprint!("PRINT_REDUCE", "{}\n", res);
    return res;
  }

  fn consume(
    &self,
    session: &mut Session,
    binds: &mut BindStack,
    x: &Value,
    consumes: &Vec<Consume>,
    guards: &Vec<Statement>,
  ) -> ReduceResult {
    dprint!("PRINT_REDUCE", "Consuming {:?} ", consumes);
    let mut stack = vec![x.clone()];
    for consume in consumes {
      if !self.apply_consume(session, &mut stack, binds, &consume) {
        dprint!("PRINT_REDUCE", "- Failed\n");
        return false;
      }
    }
    for guard in guards {
      dprint!("PRINT_REDUCE", "+ Guard[");
      let guard = guard.clone();
      let guard_res = self.guard_statement(session, binds, &guard);
      dprint!("PRINT_REDUCE", "] ");
      if !guard_res {
        dprint!("PRINT_REDUCE", "- Failed\n");
        return false;
      }
    }
    dprint!("PRINT_REDUCE", "- Succeeded\n");
    return true;
  }

  fn produce(
    &self,
    session: &mut Session,
    binds: &mut BindStack,
    output: &Value,
    nexts: &Vec<GroupRef>,
  ) -> Value {
    let mut res = self.apply_produce(session, binds, &output);
    for next in nexts {
      let next = next.clone();
      self.resolve(session, binds, &next).reduce_nested(
        session,
        binds,
        ReduceType::Regular,
        &mut res,
      );
    }
    return res;
  }

  fn guard_reducer(
    &self,
    session: &mut Session,
    binds: &mut BindStack,
    reducer: &Reducer,
  ) -> ReduceResult {
    let mut out = self.apply_produce(session, binds, &reducer.output);
    for next in &reducer.nexts {
      let next = next.clone();
      let next_res = self.resolve(session, binds, &next).reduce_nested(
        session,
        binds,
        ReduceType::Regular,
        &mut out,
      );
      if !next_res {
        return false;
      }
    }
    return self.consume(session, binds, &out, &reducer.input, &reducer.guards);
  }

  fn apply_reducer(
    &self,
    session: &mut Session,
    binds: &mut BindStack,
    x: &mut Value,
    reducer: &Reducer,
  ) -> ReduceResult {
    binds.push(&self.value_props);
    if !self.consume(session, binds, x, &reducer.input, &reducer.guards) {
      binds.pop();
      return false;
    }
    *x = self.produce(session, binds, &reducer.output, &reducer.nexts);
    binds.pop();
    return true;
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
        return self
          .resolve(session, binds, group_ref)
          .guard(session, binds);
      }
    }
  }

  fn apply_statement(
    &self,
    session: &mut Session,
    binds: &mut BindStack,
    reduce_type: ReduceType,
    x: &mut Value,
    statement: &Statement,
  ) -> ReduceResult {
    match statement {
      Statement::Reducer(reducer) => return self.apply_reducer(session, binds, x, reducer),
      Statement::Group(group_ref) => {
        // WARN: Binds necessary?
        return self.resolve(session, binds, group_ref).reduce_nested(
          session,
          binds,
          reduce_type,
          x,
        );
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
    for stmt in self.statements[reduce_type].iter() {
      let reduce_res = self.apply_statement(session, in_binds, reduce_type, x, stmt);
      if reduce_res {
        return true;
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
          let mut progress = false;
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
            self.reduce_nested(session, in_binds, reduce_type, x);
            return true;
          }
        }
      }
    }
    return false;
  }

  pub fn reduce(&self, session: &mut Session, x: &mut Value) -> ReduceResult {
    assert!(
      self.group_props.is_empty() && self.value_props.is_empty(),
      "can't reduce as initial group, not the initial group, has properties"
    );
    dprint!("PRINT_REDUCE", "--- NEW PROGRAM\n");
    return self.reduce_nested(session, &mut BindStack::root(), ReduceType::Regular, x);
  }
}
