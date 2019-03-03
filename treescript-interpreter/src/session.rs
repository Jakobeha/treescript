extern crate app_dirs;
extern crate serde;
use crate::parse::Parser;
use crate::print::Printer;
use crate::util::{AtomicFile, AtomicFileCommit};
use crate::value::Value;
use app_dirs::{AppDataType, AppInfo};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::fs::{DirEntry, File};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};

#[derive(Clone, Debug, Deserialize, Serialize)]
struct LanguageSpec {
  name: String,
  extension: String,
}

pub struct Language {
  pub name: String,
  pub extension: String,
  dir: PathBuf,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct LibrarySpec {
  code_name: String,
  dir_name: String,
}

struct Library {
  name: String,
  process: Option<Child>,
}

pub struct Session {
  langs: Vec<Language>,
  lang_idxs_by_name: HashMap<String, usize>,
  lang_idxs_by_ext: HashMap<String, usize>,
  libs: HashMap<String, Library>,
  root_dir: PathBuf,
}

impl Language {
  fn new(dir: DirEntry) -> Language {
    let dir_name = dir.file_name();
    let inferred_name = dir_name.to_string_lossy();
    let dir_path = dir.path();
    let mut spec_path = dir_path.clone();
    spec_path.push("spec.yaml");
    let spec_file = File::open(spec_path).expect(
      format!(
        "failed to read language specification for '{}'",
        inferred_name
      )
      .as_str(),
    );
    let spec: LanguageSpec = serde_yaml::from_reader(spec_file)
      .expect(format!("invalid language specification for '{}'", inferred_name).as_str());
    return Language {
      name: spec.name,
      extension: spec.extension,
      dir: dir_path,
    };
  }

  pub fn encode_ast<R: Into<Stdio>>(&self, in_code: R) -> ChildStdout {
    let mut parser_path = self.dir.clone();
    parser_path.push("parser");
    return Command::new(parser_path.as_os_str())
      .stdin(in_code)
      .stdout(Stdio::piped())
      .spawn()
      .expect(format!("failed to start parser for language: {}", self.name).as_str())
      .stdout
      .unwrap();
  }

  pub fn decode_ast<W: Into<Stdio>>(&self, out_code: W) -> ChildStdin {
    let mut printer_path = self.dir.clone();
    printer_path.push("printer");
    return Command::new(printer_path.as_os_str())
      .stdin(Stdio::piped())
      .stdout(out_code)
      .spawn()
      .expect(format!("failed to start printer for language: {}", self.name).as_str())
      .stdin
      .unwrap();
  }
}

impl Library {
  fn new(name: String) -> Library {
    return Library {
      name: name,
      process: Option::None,
    };
  }

  fn start(&mut self, path: &Option<PathBuf>, libs_dir: &PathBuf) {
    assert!(
      self.process.is_none(),
      "can't start session - already started"
    );
    let mut lib_path = libs_dir.clone();
    lib_path.push(self.name.clone());
    lib_path.push("exec");
    self.process = Option::Some(
      Command::new(lib_path.as_os_str())
        .args(path.into_iter())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect(format!("failed to start library process: {}", self.name).as_str()),
    );
  }

  fn call_fun(&mut self, name: String, args: Vec<Value>) -> Value {
    let process = self
      .process
      .as_mut()
      .expect("can't call function - not started");
    let mut parser = Parser {
      input: process
        .stdout
        .as_mut()
        .expect("can't call function - process output not available"),
    };
    let mut printer = Printer {
      output: process
        .stdin
        .as_mut()
        .expect("can't call function - process input not available"),
    };
    printer
      .print_value(Value::Record {
        head: name,
        props: args,
      })
      .expect("can't call function - couldn't send input to library process");
    return parser
      .scan_value()
      .expect("can't call function - library process didn't give output");
  }

  fn stop(&mut self) {
    let mut process = self
      .process
      .take()
      .expect("can't stop session - not started");
    process
      .kill()
      .expect(format!("library process exited early: {}", self.name).as_str());
  }
}

impl Session {
  fn root_dir() -> PathBuf {
    return app_dirs::get_app_root(
      AppDataType::UserData,
      &AppInfo {
        name: "treescript",
        author: "jakobeha",
      },
    )
    .expect("failed to get user session directory");
  }

  pub fn new(libs: Vec<LibrarySpec>) -> Session {
    let root_dir = Session::root_dir();
    let mut langs_dir = root_dir.clone();
    langs_dir.push("languages");
    let lang_dirs = fs::read_dir(langs_dir)
      .expect("failed to find languages")
      .map(|lang_dir| lang_dir.expect("failed to find a language"))
      .filter(|lang_dir| lang_dir.file_name() != ".DS_Store");
    let langs: Vec<Language> = lang_dirs.map(|lang_dir| Language::new(lang_dir)).collect();
    let lang_idxs_by_name = langs
      .iter()
      .enumerate()
      .map(|(idx, lang)| (lang.name.clone(), idx))
      .collect();
    let lang_idxs_by_ext = langs
      .iter()
      .enumerate()
      .map(|(idx, lang)| (lang.extension.clone(), idx))
      .collect();
    return Session {
      langs: langs,
      lang_idxs_by_name: lang_idxs_by_name,
      lang_idxs_by_ext: lang_idxs_by_ext,
      libs: libs
        .into_iter()
        .map(|lib| (lib.code_name, Library::new(lib.dir_name)))
        .collect(),
      root_dir: root_dir,
    };
  }

  pub fn lang_with_name(&self, name: &String) -> Option<&Language> {
    return self
      .lang_idxs_by_name
      .get(name)
      .map(|idx| &self.langs[*idx]);
  }

  fn lang_with_ext(&self, ext: &String) -> Option<&Language> {
    return self.lang_idxs_by_ext.get(ext).map(|idx| &self.langs[*idx]);
  }

  pub fn read_ast<P: AsRef<Path>>(&self, input_path: P) -> Box<Read> {
    let input_ext = String::from(
      input_path
        .as_ref()
        .extension()
        .expect("input must have extension - can't determine language")
        .to_string_lossy(),
    );
    let raw_input = File::open(input_path).expect("failed to open input file");
    if input_ext == "tast" {
      return Box::new(raw_input);
    } else {
      let input_lang = self
        .lang_with_ext(&input_ext)
        .expect(format!("unsupported input language: {}", input_ext).as_str());
      return Box::new(input_lang.encode_ast(raw_input));
    }
  }

  pub fn write_ast(&self, output_path: PathBuf) -> (Box<Write>, AtomicFileCommit) {
    let output_ext = String::from(
      output_path
        .extension()
        .expect("output must have extension - can't determine language")
        .to_string_lossy(),
    );
    let raw_output =
      AtomicFile::create(output_path).expect("can't open temporary atomic output file");
    if output_ext == "tast" {
      return (Box::new(raw_output.temp_file), raw_output.commit);
    } else {
      let output_lang = self
        .lang_with_ext(&output_ext)
        .expect(format!("unsupported output language: {}", output_ext).as_str());
      return (
        Box::new(output_lang.decode_ast(raw_output.temp_file)),
        raw_output.commit,
      );
    }
  }

  pub fn start(&mut self, path: &Option<PathBuf>) {
    let mut libs_dir = self.root_dir.clone();
    libs_dir.push("libraries");
    for lib in self.libs.values_mut() {
      lib.start(&path, &libs_dir);
    }
  }

  fn call_fun(&mut self, lib: &String, name: String, args: Vec<Value>) -> Value {
    let lib = self
      .libs
      .get_mut(lib)
      .expect(format!("failed to call function - library not setup: {}", lib).as_str());
    return lib.call_fun(name, args);
  }

  pub fn call_fun_val(&mut self, head: String, args: Vec<Value>) -> Value {
    if let Some((lib, name)) = Value::record_head_to_fun(&head) {
      return self.call_fun(&lib, name, args);
    } else {
      panic!(
        "Can't extract function lib and name from record head: {}",
        head
      )
    }
  }

  pub fn stop(&mut self) {
    for lib in self.libs.values_mut() {
      lib.stop();
    }
  }
}
