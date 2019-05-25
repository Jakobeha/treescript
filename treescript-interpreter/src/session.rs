extern crate app_dirs;
extern crate serde;
use crate::parse::Parser;
use crate::print::Printer;
use crate::resources;
use crate::util;
use crate::util::{AtomicFile, AtomicFileCommit};
use crate::value::{Record, Value};
use crate::vtype::Symbol;
use app_dirs::{AppDataType, AppInfo};
use serde::{Deserialize, Serialize};
use serde_bytes::ByteBuf;
use serde_json::Value as JsValue;
use std::borrow::Cow;
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::fs;
use std::fs::{DirEntry, File, OpenOptions};
use std::io::{BufRead, BufReader, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};

pub struct Language {
  pub extension: String,
  dir: PathBuf,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum LibrarySpec {
  CmdBinary(ByteBuf),
  JavaScript(String),
}

pub enum Library {
  CmdBinary {
    path: PathBuf,
    process: Option<Child>,
  },
  JavaScript {
    code: String,
    process: Option<Child>,
  },
}

pub struct Session {
  langs: Vec<Language>,
  lang_idxs_by_ext: HashMap<String, usize>,
  libs: HashMap<String, Library>,
  tmp_lib_dir: PathBuf,
}

impl Language {
  fn new(dir: DirEntry) -> Language {
    let dir_name = dir.file_name();
    let dir_path = dir.path();
    return Language {
      extension: String::from(dir_name.to_string_lossy()),
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
      .expect(format!("failed to start parser for language: {}", self.extension).as_str())
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
      .expect(format!("failed to start printer for language: {}", self.extension).as_str())
      .stdin
      .unwrap();
  }
}

impl Library {
  fn new(name: &String, spec: LibrarySpec, tmp_dir: &PathBuf) -> Library {
    match spec {
      LibrarySpec::CmdBinary(bytes) => {
        let mut path = tmp_dir.clone();
        path.push(name);
        let mut file = OpenOptions::new()
          .write(true)
          .create_new(true)
          .open(&path)
          .expect(format!("failed to start create file for binary lib: {}", name).as_str());
        file
          .write_all(bytes.as_slice())
          .expect(format!("failed to write binary lib to file: {}", name).as_str());
        util::allow_execute(&path)
          .expect(format!("failed to set binary lib's perms to executable: {}", name).as_str());
        return Library::CmdBinary {
          path: path,
          process: None,
        };
      }
      LibrarySpec::JavaScript(lib_txt) => {
        let ffi_txt = resources::js_ffi().expect("failed to get JS ffi module");
        return Library::JavaScript {
          code: ffi_txt + "\nObject.assign(this, ffi);\n" + lib_txt.as_str(),
          process: None,
        };
      }
    };
  }

  fn start(&mut self, prog_path: &Option<PathBuf>) {
    match self {
      Library::CmdBinary { path, process } => {
        let name = path
          .file_stem()
          .map(|st| st.to_string_lossy())
          .unwrap_or(Cow::Borrowed("???"));
        assert!(
          process.is_none(),
          "can't start library process, already started: {}",
          name
        );
        *process = Some(
          Command::new(path.as_os_str())
            .args(prog_path.into_iter())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect(format!("failed to start library process: {}", name).as_str()),
        );
      }
      Library::JavaScript { code, process } => {
        assert!(
          process.is_none(),
          "can't start JS library process, already started"
        );
        let mut p = Command::new("node")
          .stdin(Stdio::piped())
          .stdout(Stdio::piped())
          .stderr(Stdio::inherit())
          .spawn()
          .expect("failed to start JS library process");
        dprintln!("PRINT_SESSION", "Started JS session");
        write!(p.stdin.as_mut().unwrap(), "{}\n", code)
          .expect("failed to write initial code to JS library process");
        if let Some(prog_path) = prog_path {
          write!(
            p.stdin.as_mut().unwrap(),
            "var progPath = {};\n",
            prog_path.to_string_lossy()
          )
          .expect("failed to write program path to JS library process");
        }
        *process = Some(p);
      }
    }
  }

  fn call_fun(&mut self, fun: &String, args: Vec<Value>) -> Option<Value> {
    match self {
      Library::CmdBinary { path, process } => {
        let name = path
          .file_stem()
          .map(|st| st.to_string_lossy())
          .unwrap_or(Cow::Borrowed("???"));
        let process = process
          .as_mut()
          .expect(format!("can't call function, library process not started: {}", name).as_str());
        let mut parser = Parser {
          input: process.stdout.as_mut().expect(
            format!(
              "can't call function, library process output not available: {}",
              name,
            )
            .as_str(),
          ),
        };
        let mut printer = Printer {
          output: process.stdin.as_mut().expect(
            format!(
              "can't call function, library process input not available: {}",
              name,
            )
            .as_str(),
          ),
        };
        printer
          .print_value(Value::Record(Record {
            head: Symbol::from(fun),
            props: args,
          }))
          .expect(
            format!(
              "can't call function, couldn't send input to library process: {}",
              name,
            )
            .as_str(),
          );
        return parser.scan_value();
      }
      Library::JavaScript { code: _, process } => {
        let process = process
          .as_mut()
          .expect("can't call function, JS library process not started");
        let args = args
          .into_iter()
          .map(|arg| {
            serde_json::to_string::<JsValue>(
              &arg
                .try_into()
                .expect(format!("can't all function, bad input args: {}", fun).as_str()),
            )
            .unwrap()
          })
          .collect::<Vec<String>>()
          .join(", ");
        write!(
          process.stdin.as_mut().unwrap(),
          "callFfi({}, {});\n",
          fun,
          args
        )
        .expect(format!("can't all function, couldn't send input: {}", fun).as_str());
        dprintln!("PRINT_SESSION", "> callFfi({}, {});\n", fun, args);
        let out = serde_json::from_str::<JsValue>(
          BufReader::new(process.stdout.as_mut().unwrap())
            .lines()
            .next()
            .expect(format!("can't all function, no output: {}", fun).as_str())
            .expect(format!("can't all function, error getting output: {}", fun).as_str())
            .as_str(),
        )
        .expect(format!("can't all function, invalid output: {}", fun).as_str());
        // Want to still fail on undefined
        if out.is_null() {
          return None;
        } else {
          return Some(
            Value::try_from(out)
              .expect(format!("can't all function, bad output: {}", fun).as_str()),
          );
        }
      }
    };
  }

  fn stop(&mut self) {
    match self {
      Library::CmdBinary { path, process } => {
        let name = path
          .file_stem()
          .map(|st| st.to_string_lossy())
          .unwrap_or(Cow::Borrowed("???"));
        let mut process = process
          .take()
          .expect(format!("can't stop library session, not started: {}", name).as_str());
        process
          .kill()
          .expect(format!("library process exited early: {}", name).as_str());
      }
      Library::JavaScript { code: _, process } => {
        let mut process = process
          .take()
          .expect("can't stop JS library session, not started");
        process.kill().expect("JS library process exited early");
      }
    };
  }

  fn pre_drop(&mut self) {
    match self {
      Library::CmdBinary { path, process } => {
        if let Some(mut process) = process.take() {
          let _ = process.kill();
        }
        let _ = fs::remove_file(path);
      }
      Library::JavaScript { code: _, process } => {
        if let Some(mut process) = process.take() {
          let _ = process.kill();
        }
      }
    }
  }
}

impl Drop for Session {
  fn drop(&mut self) {
    for lib in self.libs.values_mut() {
      lib.pre_drop();
    }
    let _ = fs::remove_dir(&self.tmp_lib_dir);
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

  pub fn new(libs: HashMap<String, LibrarySpec>) -> Session {
    let mut langs_dir = Session::root_dir();
    langs_dir.push("languages");
    let lang_dirs = fs::read_dir(langs_dir)
      .expect("failed to find languages")
      .map(|lang_dir| lang_dir.expect("failed to find a language"))
      .filter(|lang_dir| lang_dir.file_name() != ".DS_Store");
    let langs: Vec<Language> = lang_dirs.map(|lang_dir| Language::new(lang_dir)).collect();
    let lang_idxs_by_ext = langs
      .iter()
      .enumerate()
      .map(|(idx, lang)| (lang.extension.clone(), idx))
      .collect();
    let tmp_lib_dir = AtomicFile::temp_path(std::env::temp_dir());
    fs::create_dir(&tmp_lib_dir)
      .expect("failed to create temporary directory for session libraries");
    return Session {
      langs: langs,
      lang_idxs_by_ext: lang_idxs_by_ext,
      libs: libs
        .into_iter()
        .map(|(name, spec)| {
          let lib = Library::new(&name, spec, &tmp_lib_dir);
          return (name, lib);
        })
        .collect(),
      tmp_lib_dir: tmp_lib_dir,
    };
  }

  pub fn lang_with_ext(&self, ext: &String) -> Option<&Language> {
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
    for lib in self.libs.values_mut() {
      lib.start(path);
    }
  }

  pub fn call_fun(&mut self, head: &Symbol, args: &Value) -> Option<Value> {
    let lib = self.libs.get_mut(&head.module).expect(
      format!(
        "failed to call function - library not setup: {}",
        head.module
      )
      .as_str(),
    );
    return lib.call_fun(&head.local, args.to_args());
  }

  pub fn stop(&mut self) {
    for lib in self.libs.values_mut() {
      lib.stop();
    }
  }
}
