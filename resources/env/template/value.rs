#[derive(Clone, Debug, Eq)]
pub enum Value {
  Splice(u32),
  PrimInteger(i32),
  PrimFloat(f32),
  PrimString(String),
  Record {
    pub head: &str,
    pub props: Box<Vec<Value>>
  },
}

impl Value {
  pub const unit: Value = Record { head = "Unit", props = Vec::default() };
  pub const true_: Value = Record { head = "True", props = Vec::default() };
  pub const false_: Value = Record { head = "False", props = Vec::default() };
  pub const nil: Value = Record { head = "Nil", props = Vec::default() };

  pub fn bool(bool x) -> Value {
    if x {
      true_;
    } else {
      false_;
    }
  }

  pub fn cons(first: Value, rest: Value) -> Value {
    return Record {
      head = "Cons",
      props = Box::new(vec![first, rest]),
    }
  }

  pub fn hole(idx: i32) -> Value {
    return Record {
      head = "Hole",
      props = Box::new(vec![PrimInteger(idx)])
    }
  }

  pub fn get_record_num_props(head: &str) -> usize {
    // \get_record_num_props
  }

  pub fn is_hole(&self) -> bool {
    if let Record { head, .. } = self {
      return head == "Hole";
    } else {
      return false;
    }
  }

  pub fn matches(&self, other: &Value) -> bool {
    return self.is_hole() || self == other;
  }

  pub fn subst(&self, old: &Value, new: &Value) -> Value {
    if (self == old) {
      return new.clone();
    } else if let Record { head, props } = self {
      return Record {
        head = head,
        props = props.map(|ps| ps.map(|prop| prop.subst(old, new))),
      };
    } else {
      return self.clone();
    }
  }
}
