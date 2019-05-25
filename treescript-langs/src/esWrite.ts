import { ESNode, ESNodeArray, ESSpec } from "./esSpec";

function writeSplice(out: NodeJS.WriteStream, idx: number) {
  out.write("splice " + idx + " ");
}

function writeInteger(out: NodeJS.WriteStream, int: number) {
  out.write("integer " + int + " ");
}

function writeFloat(out: NodeJS.WriteStream, float: number) {
  out.write("float " + float + " ");
}

function writeString(out: NodeJS.WriteStream, str: string) {
  out.write('string "' + escape(str) + '" ');
}

function writeNode(out: NodeJS.WriteStream, head: string, numArgs: number) {
  out.write(head + " " + numArgs + " ");
}

function writeAst(out: NodeJS.WriteStream, esAst: ESNode, spec: ESSpec) {
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
      writeNode(out, "BadFunction", 0);
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
          writeAst(out, elem, spec);
        });
        writeNode(out, "Nil", 0);
      } else if (esAst instanceof RegExp) {
        // Regexp
        writeNode(out, "RegExp", 1);
        writeString(out, esAst.source);
      } else {
        // AST
        if (!esAst.hasOwnProperty("type")) {
          console.error("<Node shouldn't be a non-AST object: " + esAst + ">");
          writeNode(out, "BadObject", 0);
          break;
        }
        var type = esAst.type;
        if (type === "Splice") {
          writeSplice(out, esAst.index);
          break;
        }
        var props = spec.nodes[type];
        if (typeof props == "undefined") {
          console.error("<Unknown node type: " + type + ">");
          writeNode(out, "BadNode", 1);
          writeString(out, type);
          break;
        }
        writeNode(out, `${spec.prefix}_${type}`, props.length);
        props.forEach(prop => {
          var val = esAst[prop];
          writeAst(out, val, spec);
        });
      }
      break;
    case "string":
      writeString(out, esAst);
      break;
    case "symbol":
      console.error("<Node shouldn't be a symbol>");
      writeNode(out, "BadSymbol", 0);
      break;
    case "undefined":
      writeNode(out, "Undefined", 0);
      break;
    default:
      console.error("<Node has invalid type: " + typeof esAst + ">");
      writeNode(out, "BadTypeof", 1);
      writeString(out, typeof esAst);
      break;
  }
}

export default function esWrite(
  out: NodeJS.WriteStream,
  asts: ESNodeArray,
  spec: ESSpec
) {
  asts.forEach(ast => {
    writeAst(out, ast, spec);
    out.write("\n");
  });
}
