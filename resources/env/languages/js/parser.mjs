#!/usr/bin/env node --experimental-modules --no-warnings

import esprima from "esprima";
import getStdin from "get-stdin";
import spec from "./spec.mjs";

var spliceRegexp = /^_splice_([0-9]+)$/;

function writeSplice(out, idx) {
  out.write("splice " + idx + " ");
}

function writeInteger(out, int) {
  out.write("integer " + int + " ");
}

function writeFloat(out, float) {
  out.write("float " + float + " ");
}

function writeString(out, str) {
  out.write("string \"" + escape(str) + "\" ");
}

function writeNode(out, head, numArgs) {
  out.write(head + " " + numArgs + " ");
}

function writeAst(out, esAst) {
  switch (typeof esAst) {
    case "bigint":
      writeNode(out, "BigInt", 1);
      writeString(out, esAst.toString());
      break;
    case "boolean":
      writeNode(out, esAst ? "True" : "False", 0);
      break;
    case "function":
      console.error("<Node shouldn't be a function>");
      writeNode(out, "JS_BadFunction", 0);
      break;
    case "number":
      if (Number.isInteger(esAst)) {
        writeInteger(out, esAst);
      } else {
        writeFloat(out, esAst);
      }
      break;
    case "object":
      if (esAst == null) {
        // Null
        writeNode(out, "Unit", 0);
      } else if (Array.isArray(esAst)) {
        // List
        esAst.forEach(elem => {
          writeNode(out, "Cons", 2);
          writeAst(out, elem);
        });
        writeNode(out, "Nil", 0);
      } else if (esAst instanceof RegExp) {
        // Regexp
        writeNode(out, "JS_RegExp", 1);
        writeString(out, esAst.source);
      } else {
        // AST
        if (esAst.type == "Identifier") {
          var match = spliceRegexp.exec(esAst.name);
          if (match != null) {
            var spliceIdx = Number.parseInt(match[1]);
            writeSplice(out, spliceIdx);
            break;
          }
        }
        if (!esAst.hasOwnProperty("type")) {
          console.error("<Node shouldn't be a non-AST object: " + esAst + ">");
          writeNode(out, "JS_BadObject", 0);
          break;
        }
        var type = esAst.type;
        var props = spec.nodes[type];
        if (typeof props == "undefined") {
          console.error("<Unknown node type: " + type + ">");
          writeNode(out, "JS_BadNode", 1);
          writeString(out, type);
          break;
        }
        writeNode(out, "JS_" + type, props.length);
        props.forEach(prop => {
          var val = esAst[prop];
          writeAst(out, val);
        });
      }
      break;
    case "string":
      writeString(out, esAst);
      break;
    case "symbol":
      console.error("<Node shouldn't be a symbol>");
      writeNode(out, "JS_BadSymbol", 0);
      break;
    case "undefined":
      writeNode(out, "JS_Undefined", 0);
      break;
    default:
      console.error("<Node has invalid type: " + typeof esAst + ">");
      writeNode(out, "JS_BadTypeof", 1);
      writeString(typeof esAst);
      break;
  }
}

getStdin().then(str => {
  if (str.startsWith("#!")) {
    str = str.substr(str.indexOf("\n"));
  }
  str = str.replace(/\\([0-9]+)/g, "_splice_$1");
  var ast = esprima.parseScript(str, { tolerant: true });
  delete ast.errors;
  writeAst(process.stdout, ast);
});
