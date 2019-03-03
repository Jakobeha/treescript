extern crate enum_map;
extern crate serde;
extern crate treescript_interpreter;
use std::fs;
use std::fs::DirEntry;
use std::fs::File;
use std::io::{Cursor, Read};
use std::path::PathBuf;
use treescript_interpreter::program::Program;
/*use treescript_interpreter::reduce::{
  Consume, GroupDefSerial, GroupRef, Reducer, ReducerClause, Statement,
};*/

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

/*fn test_serialize_prog() {
  let prog = ProgramSerial {
    num_props_by_head: Default::default(),
    libraries: Default::default(),
    main_statements: vec![Statement::Group(GroupRef {
      idx: 1,
      props: vec![Value::Prim(Prim::String(String::from("foo")))],
    })],
    sub_groups: vec![GroupDefSerial {
      props: vec![1],
      loops: true,
      statements: vec![
        vec![Statement::Reducer(Reducer {
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
        Default::default(),
        Default::default(),
      ],
    }],
  };

  let vec = rmp_serde::to_vec(&prog).unwrap();
  let mut write_path = test_resources_path();
  write_path.push("A-Simple");
  write_path.push("A-Simple.msgpack");
  let mut file = File::create(write_path).unwrap();
  file.write_all(vec.as_slice()).unwrap();
}*/

#[test]
fn test_all_raw_ast() {
  for example in examples() {
    let mut session = example.exec.new_session();
    for io_pair in example.io_pairs {
      let mut input = session.read_ast(io_pair.input.as_path());
      let mut output = session.read_ast(io_pair.output.as_path());
      let in_path = Some(io_pair.input);
      let mut exp_output = String::new();
      output.read_to_string(&mut exp_output).unwrap();
      let mut act_output_cursor = Cursor::new(Vec::new());
      example
        .exec
        .run(&mut session, &in_path, &mut input, &mut act_output_cursor);
      let act_output = String::from_utf8(act_output_cursor.get_ref().clone()).unwrap();
      assert_eq!(exp_output.trim(), act_output.trim());
    }
  }
}
