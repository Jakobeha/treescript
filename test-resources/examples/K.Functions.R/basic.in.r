# Section 2.7 of Advanced R
{
  x1 <- data.frame(matrix(runif(5 * 1e4), ncol = 5))
  medians <- vapply(x1, median, numeric(1))

  for (i1 in seq_along(medians)) {
    x1[[i1]] <- x1[[i1]] - medians[[i1]]
    cat("Hello world!")
  }

  x2 <- data.frame(matrix(0, ncol = 7))

  for (i2 in 1:7) {
    if (TRUE) {
      cat("Running action...")
      x2[[i2]] <- 5
      cat("Ran action")
    }
  }
}
