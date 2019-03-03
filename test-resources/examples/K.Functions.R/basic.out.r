x1 <- data.frame(matrix(runif(5L * 10000L), ncol = 5L))
medians <- vapply(x1, median, numeric(1L))
if (is.data.frame(x1)) {
  free0 <- as.list(x1)
  for (i1 in seq_along(medians)) {
    free0[[i1]] <- x1[[i1]] - medians[[i1]]
    cat("Hello world!")
  }
  x1 <- as.data.frame(free0)
}
x2 <- data.frame(matrix(0L, ncol = 7L))
if (is.data.frame(x2)) {
  free1 <- as.list(x2)
  for (i2 in 1L:7L) {
    if (TRUE) {
      cat("Running action...")
      free1[[i2]] <- 5L
      cat("Ran action")
    }
  }
  x2 <- as.data.frame(free1)
}
