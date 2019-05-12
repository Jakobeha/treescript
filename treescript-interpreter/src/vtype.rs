use serde::{Deserialize, Serialize};
use serde_repr::{Deserialize_repr, Serialize_repr};
use std::collections::BTreeSet;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::BitOr;

#[derive(Clone, Debug, Deserialize_repr, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize_repr)]
#[repr(usize)]
pub enum PrimType {
  Integer,
  Float,
  String,
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct Symbol {
  pub module: String,
  pub local: String,
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum SType {
  Prim(PrimType),
  Record(Symbol),
  Tuple(Vec<SType>),
  Cons(Box<SType>),
}

#[derive(Clone, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
pub enum MType {
  Any,
  Parts(BTreeSet<SType>),
}

impl Hash for MType {
  fn hash<H: Hasher>(&self, state: &mut H) {
    match self {
      MType::Any => (),
      MType::Parts(parts) => {
        for part in parts {
          part.hash(state);
        }
      }
    };
  }
}

impl Display for PrimType {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      PrimType::Integer => return write!(f, "integer"),
      PrimType::Float => return write!(f, "float"),
      PrimType::String => return write!(f, "string"),
    }
  }
}

impl BitOr for &MType {
  type Output = MType;

  fn bitor(self, rhs: &MType) -> MType {
    match (self, rhs) {
      (MType::Any, _) => return MType::Any,
      (_, MType::Any) => return MType::Any,
      (MType::Parts(xparts), MType::Parts(yparts)) => return MType::Parts(xparts | yparts),
    };
  }
}

impl MType {
  pub fn bottom() -> MType {
    return MType::Parts(BTreeSet::new());
  }
}
