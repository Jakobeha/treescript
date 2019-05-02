import { Value } from "./types";

/// Call a JS library function with TreeScript values, converts the values into JS going into the function and back to TreeScript going out.
export function callFfi(f: (...args: any[]) => any, ...args: Value[]) {
  let out = f(...(args.map((arg) => arg.fromValue()))).toValue()
  console.log(JSON.stringify(out));
}
