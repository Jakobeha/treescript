export interface IRecord {
  head: string,
  props: Value[],
}

export type Primitive = number | string

export type Value = Primitive | IRecord
