import getStdin from "get-stdin";
import readline from "readline";
import { esRead } from "./esRead";
import esWrite from "./esWrite";
import grammars, { IGrammar } from "./grammars";

const mode = process.argv[2];
const lang = process.argv[3];
const grammar = grammars[lang];
if (!grammar) {
  throw `unknown language: ${lang}`;
}
switch (mode) {
  case "parse":
    parse(grammar);
    break;
  case "print":
    print(grammar);
    break;
  default:
    throw `invalid mode: ${mode}`;
}

function parse(grammar: IGrammar) {
  getStdin().then((str: string) => {
    let nodes = grammar.parse(str);
    esWrite(process.stdout, nodes, grammar.spec);
  });
}

function print(grammar: IGrammar) {
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: false
  });
  rl.on("line", function(line) {
    const nodes = esRead(line, grammar.spec);
    nodes.forEach(node => {
      process.stdout.write(grammar.print(node) + "\n");
    });
  });
}
