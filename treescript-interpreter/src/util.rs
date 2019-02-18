extern crate tempfile;
use std::fs;
use std::fs::File;
use std::io;
use std::ops::{Generator, GeneratorState};
use std::path::PathBuf;
use tempfile::NamedTempFile;

#[allow(dead_code)]
pub struct AtomicFileCommit {
  dst_path: PathBuf,
  temp_path: PathBuf,
}

#[allow(dead_code)]
pub struct AtomicFile {
  pub commit: AtomicFileCommit,
  pub temp_file: File,
}

pub struct GeneratorIterator<G: Generator>(pub G);

#[allow(dead_code)]
impl AtomicFileCommit {
  /// Temporary file can't be used anymore afterward
  pub fn run(&self) -> io::Result<()> {
    return fs::rename(&self.temp_path, &self.dst_path);
  }
}

#[allow(dead_code)]
impl AtomicFile {
  pub fn create(path: PathBuf) -> io::Result<AtomicFile> {
    let temp = NamedTempFile::new()?;
    return Ok(AtomicFile {
      commit: AtomicFileCommit {
        dst_path: path,
        temp_path: PathBuf::from(temp.path()),
      },
      temp_file: temp.into_file(),
    });
  }
}

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
