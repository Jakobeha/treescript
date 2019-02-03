extern crate enum_map;
extern crate serde;
extern crate treescript_interpreter;
use enum_map::enum_map;
use std::fs;
use std::fs::DirEntry;
use std::fs::File;
use std::io::{Cursor, Read, Write};
use std::path::PathBuf;
use treescript_interpreter::program::Program;
use treescript_interpreter::reduce::{
  Consume, Group, ReduceType, Reducer, ReducerClause, Statement,
};
use treescript_interpreter::value::{Prim, Value};

struct ExampleIOPair {
  input: PathBuf,
  output: PathBuf,
}

struct ExampleExec {
  exec: Program,
  io_pairs: Vec<ExampleIOPair>,
}

impl From<&[PathBuf]> for ExampleIOPair {
  fn from(paths: &[PathBuf]) -> ExampleIOPair {
    assert!(paths.len() == 2);
    assert!(paths[0]
      .file_stem()
      .map_or(false, |stem| stem.to_str().unwrap().ends_with("in")));
    assert!(paths[1]
      .file_stem()
      .map_or(false, |stem| stem.to_str().unwrap().ends_with("out")));
    return ExampleIOPair {
      input: paths[0].clone(),
      output: paths[1].clone(),
    };
  }
}

impl From<DirEntry> for ExampleExec {
  fn from(entry: DirEntry) -> ExampleExec {
    let mut exec_path = entry.path();
    exec_path.push(entry.file_name());
    exec_path.set_extension("tprg");

    let exec_file = File::open(exec_path.clone()).unwrap();
    let exec = Program::from(exec_file);

    let mut exec_files: Vec<PathBuf> = read_dir(entry.path())
      .into_iter()
      .map(|entry| entry.path())
      .collect();
    exec_files.sort_unstable();
    exec_files.remove(
      exec_files
        .iter()
        .position(|exec_file| *exec_file == exec_path)
        .unwrap(),
    );
    let io_pairs = exec_files
      .chunks(2)
      .map(|chunk| ExampleIOPair::from(chunk))
      .collect();

    return ExampleExec {
      exec: exec,
      io_pairs: io_pairs,
    };
  }
}

fn read_dir(path: PathBuf) -> Vec<DirEntry> {
  return fs::read_dir(path)
    .unwrap()
    .map(|entry| entry.unwrap())
    .filter(|entry| entry.file_name() != ".DS_Store")
    .collect();
}

fn test_resources_path() -> PathBuf {
  let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  path.push("test-resources");
  return path;
}

fn examples() -> Vec<ExampleExec> {
  return read_dir(test_resources_path())
    .into_iter()
    .map(|entry| ExampleExec::from(entry))
    .collect();
}

/*#[test]
fn test_serialize_prog() {
  let prog = Program {
    num_props_by_head: Default::default(),
    libraries: Default::default(),
    main_group: Group {
      loops: false,
      statements: enum_map![
        ReduceType::Regular => vec![Statement::Group(Group {
          loops: true,
          statements: enum_map![
            ReduceType::Regular => vec![Statement::Reducer(Reducer {
              input: ReducerClause {
                consumes: vec![Consume::Prim(Prim::String(String::from("Scheme_Cons")))],
                produce: Value::Prim(Prim::String(String::from("Scheme_Cons"))),
                groups: Default::default(),
              },
              output: ReducerClause {
                consumes: vec![Consume::Record(String::from("Foo")), Consume::Bind(1)],
                produce: Value::Record {
                  head: String::from("Foo"),
                  props: vec![Value::Splice(1)],
                },
                groups: Default::default(),
              },
            })],
            ReduceType::EvalCtx => Default::default(),
            ReduceType::AltConsume => Default::default()
          ],
        })],
        ReduceType::EvalCtx => Default::default(),
        ReduceType::AltConsume => Default::default()
      ],
    },
  };

  let vec = rmp_serde::to_vec(&prog).unwrap();
  let mut write_path = test_resources_path();
  write_path.push("A-Simple");
  write_path.push("A-Simple.messagepack");
  let mut file = File::create(write_path).unwrap();
  file.write_all(vec.as_slice()).unwrap();
}*/

#[test]
fn test_all_raw_ast() {
  for example in examples() {
    for io_pair in example.io_pairs {
      if io_pair.input.extension().unwrap() == "tast"
        && io_pair.output.extension().unwrap() == "tast"
      {
        let mut input_file = File::open(io_pair.input).unwrap();
        let mut output_file = File::open(io_pair.output).unwrap();
        let mut exp_output = String::new();
        output_file.read_to_string(&mut exp_output).unwrap();
        let mut act_output_cursor = Cursor::new(Vec::new());
        example.exec.run(&mut input_file, &mut act_output_cursor);
        let act_output = String::from_utf8(act_output_cursor.get_ref().clone()).unwrap();
        assert_eq!(exp_output, act_output);
      }
    }
  }
}
