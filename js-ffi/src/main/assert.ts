export default function assert(pred: boolean) {
  if (!pred) {
    throw "assertion failed"
  }
}
