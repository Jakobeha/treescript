use std::ffi::OsString;
use std::fs;
use std::fs::File;
#[cfg(unix)]
use std::fs::Permissions;
use std::io;
use std::ops::{Generator, GeneratorState};
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};

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

/// Sets permissions to 755 on Unix-based systems
pub fn allow_execute<P: AsRef<Path>>(path: P) -> io::Result<()> {
  if cfg!(unix) {
    return fs::set_permissions(path, Permissions::from_mode(0o755));
  } else {
    return Ok(());
  }
}
