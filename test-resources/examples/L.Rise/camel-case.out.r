x1 <- data.frame(matrix(runif(5L * 10000L), ncol = 5L))
medians <- vapply(x1, median, numeric(1L))
y <- as.list(x1)
for (i1 in seq_along(medians)) {
  y[[i1]] <- x1[[i1]] - medians[[i1]]
}
x1 <- as.data.frame(y)
x2 <- data.frame(matrix(0L, ncol = 7L))
y <- as.list(x2)
for (i2 in 1L:7L) {
  y[[i2]] <- 5L
}
x2 <- as.data.frame(y)