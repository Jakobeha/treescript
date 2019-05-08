use serde::{Deserialize, Serialize};
use serde_repr::{Serialize_repr, Deserialize_repr};
use std::collections::HashSet;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::iter;
use std::iter::FromIterator;
use std::ops::BitOr;

#[derive(Clone, Debug, Deserialize_repr, Eq, Hash, PartialEq, Serialize_repr)]
#[repr(usize)]
pub enum PrimType {
  Integer,
  Float,
  String,
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct Symbol {
  pub module: String,
  pub local: String,
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum TypePart {
  Prim(PrimType),
  Record(Symbol),
  Tuple(Vec<Type>),
  List(Type),
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum Type {
  Any,
  Parts(HashSet<TypePart>),
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum SType {
  Splice,
  Atom(TypePart),
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

impl Hash for Type {
  fn hash<H: Hasher>(&self, state: &mut H) {
    match self {
      Type::Any => (),
      Type::Parts(parts) => {
        for part in parts {
          part.hash(state);
        }
      }
    };
  }
}

impl From<SType> for Type {
  fn from(stype: SType) -> Type {
    match stype {
      SType::Atom(tpart) => return Type::Parts(HashSet::from_iter(iter::once(tpart))),
      SType::Splice => return Type::Any,
    };
  }
}

impl BitOr for &Type {
  type Output = Type;

  fn bitor(self, rhs: &Type) -> Type {
    match (self, rhs) {
      (Type::Any, _) => return Type::Any,
      (_, Type::Any) => return Type::Any,
      (Type::Parts(xparts), Type::Parts(yparts)) => return Type::Parts(xparts | yparts),
    };
  }
}

impl Type {
  pub fn bottom() -> Type {
    return Type::Parts(HashSet::new());
  }
}
