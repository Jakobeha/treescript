extern crate serde;
use crate::session::Session;
use crate::value::{Record, Value};
use crate::vtype::{SType, Symbol};
use serde::{Deserialize, Serialize};
use std::borrow::Borrow;
use std::collections::{BTreeSet, HashMap};
use std::iter;
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
pub enum GroupLoc {
  Global(Symbol),
  Local(usize),
  Function(Symbol),
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GroupRef {
  pub loc: GroupLoc,
  pub vprops: Vec<Value>,
  pub gprops: Vec<GroupRef>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Cast {
  pub inner_path: Vec<usize>,
  pub out_types: BTreeSet<SType>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum Next {
  Cast(Cast),
  GroupRef(GroupRef),
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Guard {
  pub input: Value,
  pub output: Value,
  pub nexts: Vec<Next>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Reducer {
  pub main: Guard,
  pub guards: Vec<Guard>,
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct CastSurface {
  pub input: SType,
  pub output: SType,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GroupDefSerial {
  pub vprops: Vec<usize>,
  pub gprops: Vec<usize>,
  pub reducers: Vec<Reducer>,
}

#[derive(Clone, Debug)]
pub struct GroupDef {
  pub vprops: Vec<usize>,
  pub gprops: Vec<usize>,
  pub reducers: Vec<Reducer>,
  pub env: Weak<GroupEnv>,
}

#[derive(Clone, Debug)]
pub struct GroupEnv {
  pub casts: HashMap<CastSurface, Reducer>,
  pub groups: HashMap<Symbol, GroupDef>,
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
  fn _map_values<F: FnMut(&Value) -> Value>(&self, f: &mut Box<F>) -> GroupRef {
    let vprops = self.vprops.iter().map(f.as_mut()).collect();
    let mut gprops: Vec<GroupRef> = Vec::new();
    for gprop in &self.gprops {
      gprops.push(gprop._map_values(f));
    }
    return GroupRef {
      loc: self.loc.clone(),
      vprops: vprops,
      gprops: gprops,
    };
  }

  fn map_values<F: FnMut(&Value) -> Value>(&self, f: F) -> GroupRef {
    return self._map_values(&mut Box::new(f));
  }

  fn subst_gprop(&mut self, old: usize, new: &GroupRef) {
    for prop in self.gprops.iter_mut() {
      prop.subst_gprop(old, new);
    }
    // Subst this group specifically
    if let GroupLoc::Local(idx) = self.loc {
      if idx == old {
        let mut old_props = self.gprops.clone();
        *self = new.clone();
        self.gprops.append(&mut old_props);
      }
    }
  }
}

impl Next {
  fn subst_gprop(&mut self, old: usize, new: &GroupRef) {
    match self {
      Next::Cast(_) => (),
      Next::GroupRef(x) => x.subst_gprop(old, new),
    };
  }
}

impl Guard {
  fn subst_gprop(&mut self, old: usize, new: &GroupRef) {
    for next in self.nexts.iter_mut() {
      next.subst_gprop(old, new);
    }
  }
}

impl Reducer {
  fn subst_gprop(&mut self, old: usize, new: &GroupRef) {
    self.main.subst_gprop(old, new);
    for guard in self.guards.iter_mut() {
      guard.subst_gprop(old, new);
    }
  }
}

impl GroupDef {
  pub fn from_no_env(x: GroupDefSerial) -> GroupDef {
    return GroupDef {
      vprops: x.vprops,
      gprops: x.gprops,
      reducers: x.reducers,
      env: Weak::new(),
    };
  }

  fn subst_gprop(&mut self, old: usize, new: &GroupRef) {
    for red in self.reducers.iter_mut() {
      red.subst_gprop(old, new);
    }
  }

  fn subst_vprops<V: Borrow<Value>, I: IntoIterator<Item = V>>(&self, in_props: I) -> BindFrame {
    let mut binds = BindFrame::new();
    for (prop_idx, in_prop) in Iterator::zip(self.vprops.iter(), in_props) {
      let in_prop = in_prop.borrow();
      binds[*prop_idx] = Some(in_prop.clone());
    }
    return binds;
  }

  fn subst_gprops<G: Borrow<GroupRef>, I: IntoIterator<Item = G>>(&mut self, in_props: I) {
    let old_props = self.gprops.clone();
    self.gprops = Vec::new();
    for (prop_idx, in_prop) in Iterator::zip(old_props.into_iter(), in_props) {
      let in_prop = in_prop.borrow();
      self.subst_gprop(prop_idx, in_prop)
    }
  }

  fn resolve_cast(&self, in_type: &Option<SType>, out_tparts: &BTreeSet<SType>) -> Option<Reducer> {
    if let Some(in_tpart) = in_type {
      if out_tparts.contains(in_tpart) {
        return None;
      }

      let casts = &self.env.upgrade().unwrap().casts;
      let surfaces = out_tparts.iter().map(|out_tpart| CastSurface {
        input: in_tpart.clone(),
        output: out_tpart.clone(),
      });
      let valid_casts: Vec<Reducer> = surfaces
        .filter_map(|surface| casts.get(&surface).cloned())
        .collect();
      match valid_casts.as_slice() {
        [] => panic!("can't cast from {:?} to {:?}", in_tpart, out_tparts),
        [res] => return Some(res.clone()),
        _ => panic!(
          "ambiguous: multiple casts from {:?} to {:?}",
          in_tpart, out_tparts
        ),
      };
    } else {
      panic!("can't cast bind");
    }
  }

  fn resolve(&self, group_head: &Symbol, group_gprops: &Vec<GroupRef>) -> GroupDef {
    let mut group = self.env.upgrade().unwrap().groups[group_head].clone();
    dprint!("PRINT_REDUCE", "Resolving {:?} [", group_head);
    group.subst_gprops(group_gprops);
    dprintln!("PRINT_REDUCE", "] done resolving {:?}", group_head);
    return group;
  }

  fn apply_function(&self, session: &mut Session, x: &Value, head: &Symbol) -> ReduceResult {
    match session.call_fun(&head, &x) {
      None => return ReduceResult::Fail,
      Some(out) => return ReduceResult::Success(out),
    };
  }

  fn _consume(&self, binds: &mut BindFrame, x: &Value, input: &Value) -> bool {
    match input {
      Value::Prim(_) => {
        return x == input;
      }
      Value::Record(Record {
        head: in_head,
        props: in_props,
      }) => {
        if let Value::Record(Record {
          head: x_head,
          props: x_props,
        }) = x
        {
          if x_head != in_head {
            return false;
          }
          for (x_prop, in_prop) in Iterator::zip(x_props.iter(), in_props.iter()) {
            if !self.consume(binds, x_prop, in_prop) {
              return false;
            }
          }
          return true;
        } else {
          return false;
        }
      }
      Value::Splice(idx) => {
        if *idx == 0 {
          return true;
        } else {
          let bind = binds.index_mut(*idx);
          if bind == &Option::None {
            dprint!("PRINT_REDUCE", "={}", &x);
            *bind = Option::Some(x.clone());
            return true;
          } else if bind.as_ref() == Option::Some(x) {
            return true;
          } else {
            dprint!("PRINT_REDUCE", "!{}", bind.as_ref().unwrap());
            return false;
          }
        }
      }
    };
  }

  fn consume(&self, binds: &mut BindFrame, x: &Value, input: &Value) -> bool {
    dprint_wrap!("PRINT_REDUCE", "Consume", self._consume(binds, x, input));
  }

  fn apply_produce(&self, binds: &BindFrame, output: &Value) -> Value {
    dprint!("PRINT_REDUCE", "Producing {} -> ", output);
    let mut out = output.clone();
    out.fill_splices(|idx| binds[idx].as_ref().cloned().unwrap_or(Value::Splice(idx)));
    dprintln!("PRINT_REDUCE", "{}", out);
    return out;
  }

  fn _advance(
    &self,
    session: &mut Session,
    binds: &BindFrame,
    x: &Value,
    next: &Next,
  ) -> ReduceResult {
    match next {
      Next::Cast(next) => {
        let mut x = x.clone();
        let child = x.sub_at_path_mut(&next.inner_path);
        if let Some(cast) = self.resolve_cast(&child.vtype(), &next.out_types) {
          match self.reduce(session, &BindFrame::new(), child, &cast) {
            ReduceResult::Fail => return ReduceResult::Fail,
            ReduceResult::Success(new_child) => {
              *child = new_child;
            }
          };
        }
        return ReduceResult::Success(x);
      }
      Next::GroupRef(next) => {
        match &next.loc {
          GroupLoc::Global(head) => {
            let next = next.map_values(|val| self.apply_produce(binds, val));
            return self
              .resolve(head, &next.gprops)
              .transform_nested(session, next.vprops, &x);
          }
          GroupLoc::Local(idx) => panic!("Unexpected unresolved group with index: {}", idx),
          GroupLoc::Function(head) => return self.apply_function(session, x, head),
        };
      }
    };
  }

  fn advance(
    &self,
    session: &mut Session,
    binds: &BindFrame,
    x: &Value,
    next: &Next,
  ) -> ReduceResult {
    dprint_wrap!(
      "PRINT_REDUCE",
      "Advance",
      self._advance(session, binds, x, next)
    );
  }

  fn advance_all(
    &self,
    session: &mut Session,
    binds: &BindFrame,
    x: &Value,
    nexts: &Vec<Next>,
  ) -> ReduceResult {
    let mut out = x.clone();
    for next in nexts {
      match self.advance(session, binds, &out, &next) {
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
    nexts: &Vec<Next>,
  ) -> ReduceResult {
    let out = self.apply_produce(binds, &output);
    return self.advance_all(session, binds, &out, &nexts);
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
    for guard in guards.iter().rev() {
      let guard_res = self.guard(session, binds, &guard);
      if !guard_res {
        return false;
      }
    }
    return true;
  }

  fn _reduce(
    &self,
    session: &mut Session,
    in_binds: &BindFrame,
    x: &Value,
    reducer: &Reducer,
  ) -> ReduceResult {
    let mut binds = in_binds.clone();
    if !self.consume(&mut binds, x, &reducer.main.input) {
      return ReduceResult::Fail;
    }
    if !self.guard_all(session, &mut binds, &reducer.guards) {
      return ReduceResult::Fail;
    }
    return self.produce(session, &binds, &reducer.main.output, &reducer.main.nexts);
  }

  fn reduce(
    &self,
    session: &mut Session,
    in_binds: &BindFrame,
    x: &Value,
    reducer: &Reducer,
  ) -> ReduceResult {
    dprint_wrap!(
      "PRINT_REDUCE",
      "Reducer",
      self._reduce(session, in_binds, &x, &reducer)
    );
  }

  fn transform_nested<V: Borrow<Value>, I: IntoIterator<Item = V>>(
    &self,
    session: &mut Session,
    vprops: I,
    x: &Value,
  ) -> ReduceResult {
    let in_binds = self.subst_vprops(vprops);
    dprintln!("PRINT_REDUCE", "Reducing {}", x);
    for red in self.reducers.iter() {
      let reduce_res = self.reduce(session, &in_binds, x, red);
      if let ReduceResult::Success(out) = reduce_res {
        return ReduceResult::Success(out);
      }
    }
    return ReduceResult::Fail;
  }

  pub fn transform(&self, session: &mut Session, x: &Value) -> ReduceResult {
    assert!(
      self.vprops.is_empty() && self.gprops.is_empty(),
      "can't reduce as initial group, not the initial group, has properties"
    );
    dprintln!("PRINT_REDUCE", "--- NEW PROGRAM");
    return self.transform_nested(session, iter::empty::<Value>(), x);
  }
}
