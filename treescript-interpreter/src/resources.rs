use std::fs;
use std::io;
use std::path::PathBuf;

fn resources_path() -> PathBuf {
  let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  path.push("resources");
  return path;
}

pub fn js_ffi() -> io::Result<String> {
  let mut path = resources_path();
  path.push("ffi.js");
  return fs::read_to_string(path);
}
