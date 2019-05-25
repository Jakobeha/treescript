#![feature(
  bind_by_move_pattern_guards,
  bufreader_seek_relative,
  refcell_map_split,
  generators,
  generator_trait,
  try_from
)]

extern crate clap;
#[macro_use]
mod debug;
mod parse;
mod print;
mod program;
mod reduce;
mod resources;
mod session;
mod util;
mod value;
mod vtype;
use crate::program::Program;
use crate::util::AtomicFileCommit;
use clap::{App, AppSettings, Arg};
use std::env;
use std::env::Args;
use std::fs::File;
use std::io;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

fn main() {
  let mut args: Args = env::args();
  args.next().expect("missing path to treescript-interpreter");
  let prog_path_str = args
    .next()
    .expect("missing a compiled '.tprg' as first argument to treescript-interpreter");
  let prog_path = Path::new(&prog_path_str);
  let prog_name = prog_path
    .file_stem()
    .expect("provided compiled '.tprg' must be a file")
    .to_str()
    .expect("provided compiled '.tprg' must have a non-UTF8 name");
  let prog_file = File::open(prog_path).expect("provided compiled '.tprg' doesn't exist");
  let prog = Program::from(prog_file);
  let mut session = prog.new_session();
  let matches = App::new(prog_name)
    .version("1.0")
    .arg(Arg::with_name("INPUT")
          .help("The input file (or directory if recursive)")
          .index(1)
          .required_unless("stdin"))
    .arg(Arg::with_name("output")
          .short("o")
          .takes_value(true)
          .value_name("OUTPUT")
          .help("The output file (or directory). Defaults to same as input file, but possibly with different extension")
          .required_unless_one(&["INPUT", "stdout"]))
    .arg(Arg::with_name("recurse")
          .short("r")
          .help("Runs on all files in a directory"))
    .arg(Arg::with_name("stdin")
          .long("stdin")
          .help("reads input as AST data from STDIN. Useful for piping/shell scripts")
          .conflicts_with("recurse"))
    .arg(Arg::with_name("stdout")
          .long("stdout")
          .help("prints input as AST data to STDOUT. Useful for piping/shell scripts")
          .conflicts_with("output")
          .conflicts_with("recurse"))
    .setting(AppSettings::NoBinaryName)
    .get_matches_from(args);
  if matches.is_present("recurse") {
    panic!("TODO");
  } else {
    let in_path: Option<PathBuf> = matches
      .value_of("INPUT")
      .map(|in_path| PathBuf::from(in_path));
    let mut input: Box<Read>;
    if matches.is_present("stdin") {
      input = Box::new(io::stdin());
    } else {
      input = session.read_ast(in_path.as_ref().unwrap());
    }
    let mut raw_output_commit: Option<AtomicFileCommit> = None;
    let mut output: Box<Write>;
    if matches.is_present("stdout") {
      output = Box::new(io::stdout());
    } else {
      let output_path = matches.value_of("output").map_or_else(
        || {
          let mut path = PathBuf::from(matches.value_of("INPUT").unwrap());
          let inferred_lang = prog
            .inferred_lang()
            .expect("can't infer output language - you must specify an output file explicitly");
          let output_lang = session.lang_with_ext(&inferred_lang).expect(
            format!(
              "program has unsupported inferred language: {}",
              inferred_lang
            )
            .as_str(),
          );
          path.set_extension(&output_lang.extension);
          return path;
        },
        |output_path| PathBuf::from(output_path),
      );
      let (output_, raw_output_commit_) = session.write_ast(output_path);
      output = output_;
      raw_output_commit = Some(raw_output_commit_);
    }
    prog.run(
      &mut session,
      &in_path,
      &mut input.as_mut(),
      &mut output.as_mut(),
    );
    if let Some(raw_output) = raw_output_commit {
      raw_output.run().expect("can't finish writing output");
    }
  }
}
