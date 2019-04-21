#!/usr/bin/env node --experimental-modules --no-warnings

import escodegen from "escodegen";
import readline from "readline";
import spec from "./spec.mjs";

function raise(msg) {
  var wrappedMsg = "<error: " + msg + ">"
  console.error(wrappedMsg);
  return {
    type: "Literal",
    verbatim: wrappedMsg
  };
}

function raiseProps(node, expected, actual) {
  return raise("type '" + node + "' expected " + expected + " props, got " + actual);
}

function readWord(inp) {
  inp.str = inp.str.trimStart();
  var end = inp.str.indexOf(" ");
  if (end == -1) {
    end = inp.str.indexOf("\n");
  }
  if (end == -1) {
    end = inp.str.length;
  }
  var res = inp.str.substring(0, end);
  inp.str = inp.str.substring(end);
  return res;
}

function readInteger(inp) {
  var asStr = readWord(inp);
  var int = Number.parseInt(asStr);
  if (Number.isNaN(int)) {
    return raise("expected an integer, got " + asStr);
  }
  return int;
}

function readFloat(inp) {
  var asStr = readWord(inp);
  var float = Number.parseFloat(asStr);
  if (Number.isNaN(float) && inp != "NaN") {
    return raise("expected a float, got " + asStr);
  }
  return float;
}

function readString(inp) {
  inp.str = inp.str.trimStart();
  if (inp.str[0] != "\"") {
    return raise("expected a string, got " + inp.str);
  }
  inp.str = inp.str.substring(1);
  var end = 0;
  do {
    end = inp.str.indexOf("\"", end);
  } while (end != 0 && inp.str[end - 1] == "\\");
  var res = inp.str.substring(0, end);
  if (end == inp.str.length) {
    return raise("unterminated string: \"" + res + "\"");
  }
  inp.str = inp.str.substring(end + 1);
  return unescape(res);
}

function readList(inp, lst) {
  var numProps = readInteger(inp);
  if (numProps != 2) {
    return raiseProps("Cons", 2, numProps);
  }
  lst.push(readAst(inp));
  var nextWord = readWord(inp);
  switch (nextWord) {
    case "Nil":
      numProps = readInteger(inp);
      if (numProps != 0) {
        return raiseProps(word, 0, numProps);
      }
      break;
    case "Cons":
      readList(inp, lst);
      break;
  }
}

function readAst(inp) {
  var word = readWord(inp);
  var numProps;
  switch (word) {
    case "":
      return raise("expected more expressions");
    case "integer":
      return readInteger(inp);
    case "float":
      return readFloat(inp);
    case "string":
      return readString(inp);
    case "splice":
      return {
        type: "Literal",
        verbatim: "_splice_" + readInteger(inp)
      };
    case "Unit":
      numProps = readInteger(inp);
      if (numProps != 0) {
        return raiseProps(word, 0, numProps);
      }
      return null;
    case "True":
      numProps = readInteger(inp);
      if (numProps != 0) {
        return raiseProps(word, 0, numProps);
      }
      return true;
    case "False":
      numProps = readInteger(inp);
      if (numProps != 0) {
        return raiseProps(word, 0, numProps);
      }
      return false;
    case "Nil":
      numProps = readInteger(inp);
      if (numProps != 0) {
        return raiseProps(word, 0, numProps);
      }
      return [];
    case "Cons":
      var lst = [];
      readList(inp, lst);
      return lst;
    case "JS_Lang_Undefined":
      numProps = readInteger(inp);
      if (numProps != 0) {
        return raiseProps(word, 0, numProps);
      }
      return undefined;
    case "JS_Lang_RegExp":
      numProps = readInteger(inp);
      if (numProps != 1) {
        return raiseProps(word, 1, numProps);
      }
      var nextWord = readWord(inp);
      if (nextWord != "string") {
        return raise("expected regexp to have string property, got word: " + nextWord);
      }
      return new RegExp(readString(inp));
    default:
      if (word.startsWith("JS_Lang_")) {
        var type = word.substring("JS_Lang_".length);
        numProps = readInteger(inp);
        var props = spec.nodes[type];
        if (typeof props == "undefined") {
          return raise("unknown node type: " + type);
        }
        if (numProps != props.length) {
          return raiseProps(word, props.length, numProps);
        }
        var node = { type: type };
        props.forEach(prop => {
          node[prop] = readAst(inp);
        });
        return node;
      } else {
        return raise("unknown word: " + word);
      }
  }
}

var rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
});

rl.on('line', function (line) {
  var inp = { str: line };
  var ast = readAst(inp);
  var out = escodegen.generate(ast, { verbatim: "verbatim" });
  console.log(out);
  if (inp.str.trim() != "") {
    var errMsg = "expected less expressions, leftover: " + inp.str;
    console.error(errMsg);
    console.log(errMsg);
  }
});
