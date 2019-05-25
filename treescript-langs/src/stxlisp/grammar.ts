import { ESNode } from "../esSpec";

export { parse } from "./parse";

export function print(inp: ESNode): string {
  return String(inp);
}

export const spec = {
  prefix: "StxLisp_Lang",
  nodes: {
    Word: ["text"],
    Punc: ["punc"],
    Block: ["punc", "content"],
    String: ["punc", "text"],
    Int: ["base", "value"]
  }
};
