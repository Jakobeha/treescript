extern crate serde;
use crate::session::Session;
use crate::value::{Prim, Value};
use serde::{Deserialize, Serialize};
use std::borrow::Borrow;
use std::ops::{Index, IndexMut};
use std::rc::Weak;

macro_rules! dprint_wrap(
    ($var:expr, $name:expr, $inner:expr) => {{
      dprintln!($var, "Applying {} [", $name);
      dprint_begin_indent!($var);
      let res = $inner;
      dprint_end_indent!($var);
      if res.is_success() {
        dprintln!($var, "] Succeeded");
      } else {
        dprintln!($var, "] Failed");
      }
      return res;
    }}
);

const MAX_NUM_BINDS: usize = 32;

#[derive(Clone)]
struct BindFrame([Option<Value>; MAX_NUM_BINDS]);

pub enum ReduceResult {
  Fail,
  Success(Value),
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum Consume {
  Bind(usize),
  Prim(Prim),
  Record(String),
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum GroupLoc {
  Global(usize),
  Local(usize),
  Function(String),
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GroupRef {
  pub loc: GroupLoc,
  pub props: Vec<GroupRef>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Guard {
  pub input: Vec<Consume>,
  pub output: Value,
  pub nexts: Vec<GroupRef>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Reducer {
  pub main: Guard,
  pub guards: Vec<Guard>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GroupDefSerial {
  pub props: Vec<usize>,
  pub reducers: Vec<Reducer>,
}

#[derive(Clone, Debug)]
pub struct GroupDef {
  pub props: Vec<usize>,
  pub reducers: Vec<Reducer>,
  pub env: Weak<Vec<GroupDef>>,
}

trait Succeedable {
  fn is_success(&self) -> bool;
}

impl Succeedable for bool {
  fn is_success(&self) -> bool {
    return *self;
  }
}

impl Succeedable for ReduceResult {
  fn is_success(&self) -> bool {
    match self {
      ReduceResult::Fail => return false,
      ReduceResult::Success(_) => return true,
    };
  }
}

impl Index<usize> for BindFrame {
  type Output = Option<Value>;

  fn index(&self, idx: usize) -> &Option<Value> {
    if idx == 0 {
      panic!("can't reference bind 0")
    }
    return &self.0[idx - 1];
  }
}

impl IndexMut<usize> for BindFrame {
  fn index_mut(&mut self, idx: usize) -> &mut Option<Value> {
    if idx == 0 {
      panic!("can't reference bind 0")
    }
    return &mut self.0[idx - 1];
  }
}

impl BindFrame {
  fn new() -> BindFrame {
    return BindFrame(Default::default());
  }
}

impl GroupRef {
  fn sub_props(&self) -> Vec<usize> {
    let mut res: Vec<usize> = Vec::new();
    if let GroupLoc::Local(idx) = self.loc {
      res.push(idx);
    }
    for prop in self.props.iter() {
      res.append(&mut prop.sub_props());
    }
    return res;
  }

  fn subst_prop(&mut self, old: usize, new: &GroupRef) {
    for prop in self.props.iter_mut() {
      prop.subst_prop(old, new);
    }
    // Subst this group specifically - WARN: must be after?
    if let GroupLoc::Local(idx) = self.loc {
      if idx == old {
        let mut old_props = self.props.clone();
        *self = new.clone();
        self.props.append(&mut old_props);
      }
    }
  }
}

impl Guard {
  fn subst_prop(&mut self, old: usize, new: &GroupRef) {
    for next in self.nexts.iter_mut() {
      next.subst_prop(old, new);
    }
  }
}

impl Reducer {
  fn subst_prop(&mut self, old: usize, new: &GroupRef) {
    self.main.subst_prop(old, new);
    for guard in self.guards.iter_mut() {
      guard.subst_prop(old, new);
    }
  }
}

impl GroupDef {
  pub fn from_no_env(x: GroupDefSerial) -> GroupDef {
    return GroupDef {
      props: x.props,
      reducers: x.reducers,
      env: Weak::new(),
    };
  }

  fn subst_prop(&mut self, old: usize, new: &GroupRef) {
    for red in self.reducers.iter_mut() {
      red.subst_prop(old, new);
    }
  }

  fn subst_props<G: Borrow<GroupRef>, I: IntoIterator<Item = G>>(&mut self, in_props: I) {
    let old_props = self.props.clone();
    self.props = Vec::new();
    for (prop_idx, in_prop) in Iterator::zip(old_props.into_iter(), in_props) {
      let in_prop = in_prop.borrow();
      let mut in_prop_idxs = in_prop.sub_props();
      self.props.append(&mut in_prop_idxs);
      self.subst_prop(prop_idx, in_prop)
    }
  }

  fn resolve(&self, group_idx: usize, group_props: &Vec<GroupRef>) -> GroupDef {
    let mut group = self.env.upgrade().unwrap()[group_idx].clone();
    dprint!("PRINT_REDUCE", "Resolving {:?} [", group_idx);
    group.subst_props(group_props);
    dprintln!("PRINT_REDUCE", "] done resolving {:?}", group_idx);
    return group;
  }

  fn apply_function(&self, session: &mut Session, x: &Value, head: &String) -> ReduceResult {
    match session.call_fun_val(&head, &x) {
      None => return ReduceResult::Fail,
      Some(out) => return ReduceResult::Success(out),
    };
  }

  fn apply_consume(
    &self,
    stack: &mut Vec<Value>,
    binds: &mut BindFrame,
    consume: &Consume,
  ) -> bool {
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
    }
  }

  fn consume(&self, binds: &mut BindFrame, x: &Value, consumes: &Vec<Consume>) -> bool {
    dprint!("PRINT_REDUCE", "Consuming {:?} ", consumes);
    let mut stack = vec![x.clone()];
    for consume in consumes {
      if !self.apply_consume(&mut stack, binds, &consume) {
        dprintln!("PRINT_REDUCE", "- Failed");
        return false;
      }
    }
    dprintln!("PRINT_REDUCE", "- Succeeded");
    return true;
  }

  fn apply_produce(&self, binds: &BindFrame, output: &Value) -> Value {
    dprint!("PRINT_REDUCE", "Producing {} -> ", output);
    let mut out = output.clone();
    out.fill_splices(|idx| binds[idx].as_ref().cloned().unwrap_or(Value::Splice(idx)));
    dprintln!("PRINT_REDUCE", "{}", out);
    return out;
  }

  fn _advance(&self, session: &mut Session, x: &Value, next: &GroupRef) -> ReduceResult {
    match &next.loc {
      GroupLoc::Global(idx) => {
        return self
          .resolve(*idx, &next.props)
          .transform_nested(session, &x);
      }
      GroupLoc::Local(idx) => panic!("Unexpected unresolved group with index: {}", idx),
      GroupLoc::Function(head) => return self.apply_function(session, x, head),
    };
  }

  fn advance(&self, session: &mut Session, x: &Value, next: &GroupRef) -> ReduceResult {
    dprint_wrap!("PRINT_REDUCE", "Advance", self._advance(session, &x, &next));
  }

  fn advance_all(&self, session: &mut Session, x: &Value, nexts: &Vec<GroupRef>) -> ReduceResult {
    let mut out = x.clone();
    for next in nexts {
      match self.advance(session, &out, &next) {
        ReduceResult::Fail => return ReduceResult::Fail,
        ReduceResult::Success(res) => out = res,
      };
    }
    return ReduceResult::Success(out);
  }

  fn produce(
    &self,
    session: &mut Session,
    binds: &BindFrame,
    output: &Value,
    nexts: &Vec<GroupRef>,
  ) -> ReduceResult {
    let out = self.apply_produce(binds, &output);
    return self.advance_all(session, &out, &nexts);
  }

  fn _guard(&self, session: &mut Session, binds: &mut BindFrame, guard: &Guard) -> bool {
    match self.produce(session, binds, &guard.output, &guard.nexts) {
      ReduceResult::Fail => return false,
      ReduceResult::Success(out) => return self.consume(binds, &out, &guard.input),
    };
  }

  fn guard(&self, session: &mut Session, binds: &mut BindFrame, guard: &Guard) -> bool {
    dprint_wrap!("PRINT_REDUCE", "Guard", self._guard(session, binds, &guard));
  }

  fn guard_all(&self, session: &mut Session, binds: &mut BindFrame, guards: &Vec<Guard>) -> bool {
    for guard in guards {
      let guard_res = self.guard(session, binds, &guard);
      if !guard_res {
        return false;
      }
    }
    return true;
  }

  fn _reduce(&self, session: &mut Session, x: &Value, reducer: &Reducer) -> ReduceResult {
    let mut binds = BindFrame::new();
    if !self.consume(&mut binds, x, &reducer.main.input) {
      return ReduceResult::Fail;
    }
    if !self.guard_all(session, &mut binds, &reducer.guards) {
      return ReduceResult::Fail;
    }
    return self.produce(session, &binds, &reducer.main.output, &reducer.main.nexts);
  }

  fn reduce(&self, session: &mut Session, x: &Value, reducer: &Reducer) -> ReduceResult {
    dprint_wrap!(
      "PRINT_REDUCE",
      "Reducer",
      self._reduce(session, &x, &reducer)
    );
  }

  fn transform_nested(&self, session: &mut Session, x: &Value) -> ReduceResult {
    dprintln!("PRINT_REDUCE", "Reducing {}", x);
    for red in self.reducers.iter() {
      let reduce_res = self.reduce(session, x, red);
      if let ReduceResult::Success(out) = reduce_res {
        return ReduceResult::Success(out);
      }
    }
    return ReduceResult::Fail;
  }

  pub fn transform(&self, session: &mut Session, x: &Value) -> ReduceResult {
    assert!(
      self.props.is_empty(),
      "can't reduce as initial group, not the initial group, has properties"
    );
    dprintln!("PRINT_REDUCE", "--- NEW PROGRAM");
    return self.transform_nested(session, x);
  }
}
