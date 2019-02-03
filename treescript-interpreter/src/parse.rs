extern crate unicode_reader;
use crate::util::Float;
use crate::value::{Prim, Value};
use std::collections::HashMap;
use std::io::Read;
use unicode_reader::CodePoints;

pub struct Parser<R: Read> {
  pub input: R,
  pub num_props_by_head: HashMap<String, usize>,
}

impl<R: Read> Parser<R> {
  pub fn scan_word(&mut self) -> String {
    let mut word = String::new();
    let mut is_empty = true;
    for next_res in CodePoints::from(self.input.by_ref()) {
      let next = next_res.unwrap();
      if next == ' ' || next == '\n' {
        if is_empty {
          continue;
        } else {
          break;
        }
      }
      is_empty = false;
      word.push(next);
    }
    return word;
  }

  pub fn scan_integer(&mut self) -> i32 {
    let word = self.scan_word();
    return word.parse().unwrap();
  }

  pub fn scan_float(&mut self) -> Float {
    let word = self.scan_word();
    return Float(word.parse().unwrap());
  }

  pub fn scan_string(&mut self) -> String {
    let mut word = String::new();
    let mut iter = CodePoints::from(self.input.by_ref()).map(|res| res.unwrap());
    assert!(iter.next() == Option::Some('"'));
    let mut is_escaping = false;
    let mut is_done = false;
    for next in iter.by_ref().take_while(|next| *next != '\n') {
      if is_escaping {
        is_escaping = false;
        match next {
          //TODO handle more escape codes
          'r' => word.push('\r'),
          'n' => word.push('\n'),
          't' => word.push('\t'),
          '"' | '\\' => word.push(next),
          _ => panic!("invalid escape: {}", next),
        }
      } else if next == '"' {
        is_done = true;
        break;
      } else if next == '\\' {
        is_escaping = true;
      } else {
        word.push(next);
      }
    }
    assert!(is_done && !is_escaping);
    //process following whitespace
    let next = iter.next();
    assert!(next == Option::Some(' ') || next == Option::Some('\n') || next == Option::None);
    return word;
  }

  pub fn scan_value(&mut self, word: String) -> Value {
    match word.as_str() {
      "splice" => return Value::Splice(self.scan_integer() as usize),
      "integer" => return Value::Prim(Prim::Integer(self.scan_integer())),
      "float" => return Value::Prim(Prim::Float(self.scan_float())),
      "string" => return Value::Prim(Prim::String(self.scan_string())),
      _ => {
        if word.chars().next().map_or(false, |fst| fst.is_uppercase()) {
          let num_props = *self.num_props_by_head.get(&word).unwrap();
          let mut props = Vec::with_capacity(num_props);
          for _ in 0..num_props {
            let word = self.scan_word();
            assert!(!word.is_empty());
            props.push(self.scan_value(word));
          }
          return Value::Record {
            head: word,
            props: props,
          };
        } else {
          panic!("word has unknown type: {}", word);
        }
      }
    }
  }
}
