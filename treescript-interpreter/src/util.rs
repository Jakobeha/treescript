extern crate serde;
use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter, Result};
use std::ops::{Generator, GeneratorState};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Float(pub f32);

pub struct GeneratorIterator<G: Generator>(pub G);

impl<G> Iterator for GeneratorIterator<G>
where
  G: Generator<Return = ()>,
{
  type Item = G::Yield;

  fn next(&mut self) -> Option<Self::Item> {
    match unsafe { self.0.resume() } {
      GeneratorState::Yielded(x) => Some(x),
      GeneratorState::Complete(()) => None,
    }
  }
}

impl PartialEq for Float {
  fn eq(&self, other: &Float) -> bool {
    return self.0.to_bits() == other.0.to_bits();
  }
}

impl Eq for Float {}

impl Display for Float {
  fn fmt(&self, f: &mut Formatter) -> Result {
    return self.0.fmt(f);
  }
}
