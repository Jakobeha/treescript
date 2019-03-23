extern crate rmp_serde;
use crate::program::ProgramSerial;
use crate::reduce::{
  Consume, GroupDefSerial, GroupMode, GroupRef, Reducer, ReducerClause, Statement,
};
use crate::value::{Prim, Value};
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

fn test_resources_path(module: &str) -> PathBuf {
  let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  path.push("test-resources");
  path.push(module);
  return path;
}

#[test]
fn test_serialize_prog() {
  let prog = ProgramSerial {
    libraries: Default::default(),
    main_statements: vec![Statement::Group(GroupRef {
      idx: 0,
      props: vec![Value::Prim(Prim::String(String::from("foo")))],
    })],
    sub_groups: vec![GroupDefSerial {
      props: vec![0],
      mode: GroupMode::Loop,
      statements: vec![
        vec![Statement::Reducer(Reducer {
          input: ReducerClause {
            consumes: vec![Consume::Prim(Prim::String(String::from("bar")))],
            produce: Value::Prim(Prim::String(String::from("bar"))),
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
  let mut write_path = test_resources_path("program");
  write_path.push("SerializeRust.msgpack");
  let mut file = File::create(write_path).unwrap();
  file.write_all(vec.as_slice()).unwrap();
}
