function IndexPath(hay, ndl) {
  if (hay == ndl) {
    return new SList();
  }
  if (hay instanceof SList) {
    for (var i = 0; i < hay.length(); i++) {
      var sub = IndexPath(hay[i], ndl);
      if (sub != null) {
        sub.unshift(i);
        return sub;
      }
    }
  }
  return false;
}
