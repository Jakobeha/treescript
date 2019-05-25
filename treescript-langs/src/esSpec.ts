type ESAtom = undefined | null | boolean | number | string | bigint | RegExp;

export type ESNode = ESAtom | ESRecord | ESNodeArray;

export class ESNodeArray extends Array<ESNode> {}

export interface ESRecord {
  type: string;
  [props: string]: any;
}

export interface ESVerbatim extends ESRecord {
  type: "Verbatim";
  verbatim: string;
}

export interface ESSpec {
  prefix: string;
  nodes: { [types: string]: string[] };
}
