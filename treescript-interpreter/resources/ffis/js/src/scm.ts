import assert from "./main/assert";
import { addFromValue } from "./main/convert";
import { Value } from "./main/types";
import { list } from "./main/value";

export class SList extends Array<any> {
  constructor(...items: any[]) {
    super(...items);
  }

  toValue(): Value {
    return list(this, "SNil", "SCons")
  }
}

addFromValue("Scheme_Lang_SNil", () => new SList([]))
addFromValue("Scheme_Lang_SCons", (head, tail) => {
  let res = tail.fromValue();
  assert(res instanceof SList);
  res.unshift(head.fromValue());
  return res;
});
