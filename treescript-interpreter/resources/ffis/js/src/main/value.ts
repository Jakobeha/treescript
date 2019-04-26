import { IRecord, Value } from './types';

export function record(head: string, ...props: Value[]): IRecord {
  return {
    head: head,
    props: props,
  }
}

export function bigInt(x: bigint): IRecord {
  return record("BigInt", x.toString())
}

export function list(x: any[], nilHead: string = "Nil", consHead: string = "Cons"): IRecord {
  let res = record(nilHead)
  let rem = new Array(...x)
  while (rem.length > 0) {
    res = record(consHead, rem.pop()!.toValue(), res)
  }
  return res
}

export const true_: IRecord = record("True")
export const false_: IRecord = record("False")
export const null_: IRecord = record("Null");
