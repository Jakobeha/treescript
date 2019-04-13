extern crate rmp_serde;
use crate::program::ProgramSerial;
use crate::reduce::{Consume, GroupDefSerial, GroupLoc, GroupRef, Guard, Reducer};
use crate::value::{Prim, Value};
use std::fs::File;
use std::io::{Read, Write};
use std::path::PathBuf;
use std::process::Command;

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
    groups: vec![
      GroupDefSerial {
        vprops: vec![],
        gprops: vec![],
        reducers: vec![Reducer {
          main: Guard {
            input: vec![Consume::Bind(1)],
            output: Value::Splice(1),
            nexts: vec![GroupRef {
              loc: GroupLoc::Global(1),
              vprops: vec![],
              gprops: vec![GroupRef {
                loc: GroupLoc::Global(2),
                vprops: vec![],
                gprops: vec![],
              }],
            }],
          },
          guards: vec![],
        }],
      },
      GroupDefSerial {
        vprops: vec![],
        gprops: vec![1],
        reducers: vec![
          Reducer {
            main: Guard {
              input: vec![Consume::Prim(Prim::String(String::from("bar")))],
              output: Value::Record {
                head: String::from("Foo"),
                props: vec![Value::Splice(1)],
              },
              nexts: vec![],
            },
            guards: vec![Guard {
              input: vec![Consume::Bind(1)],
              output: Value::Prim(Prim::Integer(5)),
              nexts: vec![GroupRef {
                loc: GroupLoc::Local(1),
                vprops: vec![],
                gprops: vec![],
              }],
            }],
          },
          Reducer {
            main: Guard {
              input: vec![
                Consume::Record(String::from("Foo")),
                Consume::Record(String::from("Bar")),
              ],
              output: Value::Record {
                head: String::from("Bar"),
                props: vec![],
              },
              nexts: vec![],
            },
            guards: vec![],
          },
        ],
      },
      GroupDefSerial {
        vprops: vec![],
        gprops: vec![],
        reducers: vec![],
      },
    ],
  };

  let vec = rmp_serde::to_vec(&prog).unwrap();
  let path = test_resources_path("program");
  let mut msgpack_path = path.clone();
  msgpack_path.push("SerializeRust.msgpack");
  let mut json_path = path.clone();
  json_path.push("SerializeRust.json");
  let mut haskell_path = path.clone();
  haskell_path.push("SerializeHaskell.json");
  let mut msgpack_file = File::create(&msgpack_path).unwrap();
  msgpack_file.write_all(vec.as_slice()).unwrap();
  let mut process = Command::new("msgpack2json")
    .arg("-i")
    .arg(&msgpack_path)
    .arg("-o")
    .arg(&json_path)
    .arg("-p")
    .spawn()
    .unwrap();
  let pout = process.wait().unwrap();
  assert!(pout.success(), "failed to convert msgpack to json");
  let mut json_file = File::open(&json_path).unwrap();
  let mut haskell_file = File::open(&haskell_path).unwrap();
  let mut json = String::new();
  let mut haskell = String::new();
  json_file.read_to_string(&mut json).unwrap();
  haskell_file.read_to_string(&mut haskell).unwrap();
  assert_eq!(json, haskell);
}
