#!/usr/bin/env Rscript
# TODO Fix handling dots (specifically cases like (foo . (bar)))
library(stringr)
options(useFancyQuotes = FALSE)

stdin <- file("stdin")
open(stdin)

get_next_word <- function(inp) {
  str_match(inp, "^([^ ]+) (.*)$")
}

for (line in readLines(stdin)) {
  inp <- line
  word_and_inp <- get_next_word(inp)
  word <- word_and_rest[2]
  inp <- word_and_rest[3]
  if (word == "splice") {
    idx_and_inp <- get_next_word(inp)
    idx <- idx_and_inp[2]
    inp <- idx_and_inp[3]
    cat("\\", as.string())
  }
}