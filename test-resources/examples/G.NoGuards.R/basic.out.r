x1 <- data.frame(matrix(runif(5 * 1e4), ncol = 5))
medians <- vapply(x1, median, numeric(1))
{
  y <- as.list(x1)
  for (i1 in seq_along(medians)) {
    y[[i1]] <- y[[i1]] - medians[[i1]]
  }
  x1 <<- as.data.frame(y)
}
x2 <- data.frame(matrix(0, ncol = 7))
{
  y <- as.list(x2)
  for (i2 in 1:7) {
    y[[i2]] <- 5
  }
  x2 <<- as.data.frame(y)
}
