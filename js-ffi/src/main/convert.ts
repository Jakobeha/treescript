import assert from './assert';
import { IRecord, Value } from "./types";
import { bigInt, false_, list, null_, true_ } from "./value";

Object.prototype.fromValue = function (this: any) {
  switch (typeof this) {
    case "number":
    case "string":
      return this;
    case "object":
      if (typeof this.head === "undefined" || typeof this.props === "undefined") {
        throw `not a TreeScript value: ${this.toString()}`
      }
      let fromValue = fromValues[this.head]
      if (typeof fromValue !== "undefined") {
        return fromValue.apply(this, this.props)
      } else {
        return this
      }
    case "function":
    case "symbol":
    case "undefined":
      throw `not a TreeScript value: ${this.toString()}`
  }

}

Object.prototype.toValue = function (this: any) {
  switch (typeof this) {
    case "bigint":
      return bigInt(this)
    case "boolean":
      if (this) {
        return true_
      } else {
        return false_
      }
    case "function":
      throw `can't convert function into TreeScript value: ${this.toString()}`
    case "number":
      return this
    case "object":
      if (this == null) {
        return null_;
      } else {
        throw `can't convert this object into TreeScript value (override?): ${this.toString()}`;
      }
    case "string":
      return this;
    case "symbol":
      throw `can't convert symbol into TreeScript value: ${this.toString()}`;
    case "undefined":
      throw "can't convert undefined into TreeScript value";
    default:
      throw `can't convert ${typeof this} into TreeScript value: ${this.toString()}`;
  }
};

Array.prototype.toValue = function (this: any[]) {
  return list(this);
}

/// Add a function which will convert a TreeScript record into a JS value.
/// The value should have an overridden `toValue` to convert it back.
/// These converters will be used implicitly when passing values to/from the program.
export function addFromValue(head: string, f: (this: IRecord, ...props: Value[]) => (any | undefined)) {
  fromValues[head] = f;
}

let fromValues: { [key: string]: (this: IRecord, ...props: Value[]) => any } = {};

addFromValue("Nil", () => [])
addFromValue("Cons", (head, tail) => {
  let res = tail.fromValue()
  assert(Array.isArray(res))
  res.unshift(head.fromValue())
  return res
})
