import { IRecord, Value } from './types';

declare global {
  export interface Object {
    /// Converts a TreeScript value into a JS value
    fromValue(): any
    /// Converts a JS value into a TreeScript value
    toValue(): Value
  }

  /// Implicitly convert a TreeScript record into a JS value.
  let fromValues: { [key: string]: (this: IRecord, ...props: Value[]) => any };
}
