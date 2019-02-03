#![feature(generators, generator_trait)]

mod parse;
mod print;
mod program;
mod reduce;
mod util;
mod value;
use crate::program::Program;
use std::env;
use std::fs::File;
use std::io;

fn main() {
  let args: Vec<String> = env::args().collect();
  if args.len() != 2 {
    panic!(
      "invalid number of arguments - expected 1, got {}",
      args.len()
    );
  }
  let prog_path = &args[1];
  let prog_file = File::open(prog_path).unwrap();
  let prog = Program::from(prog_file);

  prog.run(&mut io::stdin(), &mut io::stdout());
}

//fn main() {}
