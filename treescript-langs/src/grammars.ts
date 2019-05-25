import { ESNode, ESSpec } from "./esSpec";
import * as javascript from "./javascript/grammar";
import * as stxlisp from "./stxlisp/grammar";

export interface IGrammar {
  spec: ESSpec;
  parse(inp: string): ESNode[];
  print(inp: ESNode): string;
}

const grammars: { [lang: string]: IGrammar } = {
  stxlisp: stxlisp,
  javascript: javascript
};
export default grammars;
