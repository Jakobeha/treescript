use std::borrow::Borrow;
use std::ffi::OsString;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::fs;
use std::fs::File;
#[cfg(unix)]
use std::fs::Permissions;
use std::io;
use std::ops::{Generator, GeneratorState};
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::vec::IntoIter;

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

pub struct LazyList<Item: Clone, I: Iterator<Item = Item>> {
  cached: Rc<Vec<Item>>,
  iter: Rc<I>,
}

pub struct LazyListIter<Item, I: Iterator<Item = Item>> {
  cached: Rc<Vec<Item>>,
  local_cached: IntoIter<Item>,
  iter: Rc<I>,
}

#[allow(dead_code)]
impl AtomicFileCommit {
  /// Temporary file can't be used anymore afterward
  pub fn run(self) -> io::Result<()> {
    return fs::rename(&self.temp_path, &self.dst_path).map(|_| ());
  }
}

impl Drop for AtomicFileCommit {
  fn drop(&mut self) {
    //Ignore if file can't be removed - will happen if it got moved
    let _ = fs::remove_file(&self.temp_path);
  }
}

#[allow(dead_code)]
impl AtomicFile {
  pub fn temp_path<P: AsRef<Path>>(path: P) -> PathBuf {
    let mut temp_base = OsString::from(path.as_ref().as_os_str());
    temp_base.push(".temp");
    let mut counter = 0;
    let mut temp_path = PathBuf::from(&temp_base);
    while temp_path.exists() {
      let mut temp_path_os = temp_base.clone();
      temp_path_os.push(counter.to_string());
      temp_path = PathBuf::from(temp_path_os);
      counter += 1;
    }
    return temp_path;
  }

  pub fn create(path: PathBuf) -> io::Result<AtomicFile> {
    let temp_path = AtomicFile::temp_path(&path);
    let temp_file = File::create(&temp_path)?;
    return Ok(AtomicFile {
      commit: AtomicFileCommit {
        dst_path: path,
        temp_path: temp_path,
      },
      temp_file: temp_file,
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

impl<Item: Clone, I: Iterator<Item = Item>, II: IntoIterator<Item = Item, IntoIter = I>> From<II>
  for LazyList<Item, I>
{
  fn from(iter: II) -> LazyList<Item, I> {
    return LazyList {
      cached: Rc::new(Vec::new()),
      iter: Rc::new(iter.into_iter()),
    };
  }
}

impl<Item: Clone + Display, I: Iterator<Item = Item>> Display for LazyList<Item, I> {
  fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
    return self
      .iter()
      .map(|x| x.to_string())
      .collect::<Vec<String>>()
      .join(",")
      .fmt(f);
  }
}

impl<Item: Clone, I: Iterator<Item = Item>> LazyList<Item, I> {
  pub fn is_empty(&self) -> bool {
    return self.iter().next().is_none();
  }

  pub fn iter(&self) -> LazyListIter<Item, I> {
    return LazyListIter {
      cached: Rc::clone(&self.cached),
      local_cached: (self.cached.borrow() as &Vec<Item>).clone().into_iter(),
      iter: Rc::clone(&self.iter),
    };
  }
}

impl<Item: Clone, I: Iterator<Item = Item>> Iterator for LazyListIter<Item, I> {
  type Item = Item;

  fn next(&mut self) -> Option<Item> {
    return self.local_cached.next().or_else(|| {
      let next = Rc::get_mut(&mut self.iter).unwrap().next()?;
      Rc::get_mut(&mut self.cached).unwrap().push(next.clone());
      return Some(next);
    });
  }
}

/// Sets permissions to 755 on Unix-based systems
pub fn allow_execute<P: AsRef<Path>>(path: P) -> io::Result<()> {
  if cfg!(unix) {
    return fs::set_permissions(path, Permissions::from_mode(0o755));
  } else {
    return Ok(());
  }
}
