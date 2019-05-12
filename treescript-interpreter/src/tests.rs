extern crate rmp_serde;
use crate::program::ProgramSerial;
use crate::reduce::{Cast, GroupDefSerial, GroupLoc, GroupRef, Guard, Next, Reducer};
use crate::session::LibrarySpec;
use crate::value::{Prim, Record, Value};
use crate::vtype::{PrimType, SType, Symbol};
use std::collections::BTreeSet;
use std::fs::File;
use std::io::{Read, Write};
use std::iter::FromIterator;
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
    path: String::from("Serialize"),
    casts: vec![],
    groups: vec![
      (
        Symbol {
          module: String::from("Serialize"),
          local: String::from("Bar"),
        },
        GroupDefSerial {
          vprops: vec![],
          gprops: vec![],
          reducers: vec![],
        },
      ),
      (
        Symbol {
          module: String::from("Serialize"),
          local: String::from("Foo"),
        },
        GroupDefSerial {
          vprops: vec![],
          gprops: vec![1],
          reducers: vec![
            Reducer {
              main: Guard {
                input: Value::Prim(Prim::String(String::from("bar"))),
                output: Value::Record(Record {
                  head: Symbol {
                    module: String::from("Serialize"),
                    local: String::from("Baz"),
                  },
                  props: vec![Value::Splice(1)],
                }),
                nexts: vec![Next::Cast(Cast {
                  inner_path: vec![0],
                  out_types: BTreeSet::from_iter(vec![
                    SType::Record(Symbol::from("Nil")),
                    SType::Cons(Box::new(SType::Prim(PrimType::Integer))),
                    SType::Cons(Box::new(SType::Prim(PrimType::Float))),
                    SType::Cons(Box::new(SType::Prim(PrimType::String))),
                    SType::Cons(Box::new(SType::Record(Symbol::from("False")))),
                    SType::Cons(Box::new(SType::Record(Symbol::from("True")))),
                  ]),
                })],
              },
              guards: vec![Guard {
                input: Value::Splice(1),
                output: Value::Prim(Prim::Integer(5)),
                nexts: vec![Next::GroupRef(GroupRef {
                  loc: GroupLoc::Local(1),
                  vprops: vec![],
                  gprops: vec![],
                })],
              }],
            },
            Reducer {
              main: Guard {
                input: Value::Record(Record {
                  head: Symbol {
                    module: String::from("Serialize"),
                    local: String::from("Foo"),
                  },
                  props: vec![Value::Record(Record {
                    head: Symbol {
                      module: String::from("Serialize"),
                      local: String::from("Bar"),
                    },
                    props: vec![],
                  })],
                }),
                output: Value::Record(Record {
                  head: Symbol {
                    module: String::from("Scheme_Lang"),
                    local: String::from("SNil"),
                  },
                  props: vec![],
                }),
                nexts: vec![],
              },
              guards: vec![],
            },
          ],
        },
      ),
      (
        Symbol {
          module: String::from("Serialize"),
          local: String::from("Main"),
        },
        GroupDefSerial {
          vprops: vec![],
          gprops: vec![],
          reducers: vec![Reducer {
            main: Guard {
              input: Value::Splice(1),
              output: Value::Splice(1),
              nexts: vec![Next::GroupRef(GroupRef {
                loc: GroupLoc::Global(Symbol {
                  module: String::from("Serialize"),
                  local: String::from("Foo"),
                }),
                vprops: vec![],
                gprops: vec![GroupRef {
                  loc: GroupLoc::Global(Symbol {
                    module: String::from("Serialize"),
                    local: String::from("Bar"),
                  }),
                  vprops: vec![],
                  gprops: vec![],
                }],
              })],
            },
            guards: vec![],
          }],
        },
      ),
    ],
    libraries: vec![(
      String::from("Serialize"),
      LibrarySpec::JavaScript(String::from("Foo = function(_foo, bar) { return bar; };\n")),
    )],
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
