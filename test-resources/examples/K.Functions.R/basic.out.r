x1 <- data.frame(matrix(runif(5L * 10000L), ncol = 5L))
medians <- vapply(x1, median, numeric(1L))
if (is.data.frame(x1)) {
  y1 <- as.list(x1)
  for (i1 in seq_along(medians)) {
    y1[[i1]] <- x1[[i1]] - medians[[i1]]
  }
  x1 <- as.data.frame(y1)
}
if (is.data.frame(x2)) {
  x2 <- data.frame(matrix(0L, ncol = 7L))
  y2 <- as.list(x2)
  for (i2 in 1L:7L) {
    y2[[i2]] <- 5L
  }
  x2 <- as.data.frame(y2)
}
