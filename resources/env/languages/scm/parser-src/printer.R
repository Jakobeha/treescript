#!/usr/bin/env Rscript
# TODO Fix handling dots (specifically cases like (foo . (bar)))
library(stringr)
options(useFancyQuotes = FALSE)

stdin <- file("stdin")
open(stdin)

get_next_word <- function(inp) {
  str_match(inp, "^([^ ]+) (.*)$")
}

read_expect_integer <- function(inp, n) {
  word_and_inp <- get_next_word(inp)
  word <- word_and_inp[2]
  if (is.na(word_and_inp[3])) {
      cat("<expected num props (specifically ", n, "), got nothing>", sep="")
  } else {
    inp <- word_and_inp[3]
    if (as.integer(word) != n) {
      cat("<for num props - expected", n, "got", word, ">")
    }
  }
  inp
}

for (line in readLines(stdin)) {
  in_cdr <- FALSE
  inp <- paste0(line, " ")
  while (!is.na(inp)) {
    word_and_inp <- get_next_word(inp)
    word <- word_and_inp[2]
    inp <- word_and_inp[3]

    if (is.na(inp)) {
      break
    }

    if (in_cdr && word != "Scheme_Lang_SCons" && word != "Scheme_Lang_SNil") {
      cat(" . ")
    }

    if (word == "splice") {
      idx_and_inp <- get_next_word(inp)
      idx <- idx_and_inp[2]
      inp <- idx_and_inp[3]
      cat("\\", idx, sep="")
    } else if (word == "Scheme_Lang_Symbol") {
      inp <- read_expect_integer(inp, 1)
      word_and_inp <- get_next_word(inp)
      word <- word_and_inp[2]
      inp <- word_and_inp[3]
      if (word == "string") {
        lit_and_inp <- str_match(inp, "^\"([^ ]+)\" (.*)$")
        lit <- lit_and_inp[2]
        inp <- lit_and_inp[3]
        cat(lit)
      } else {
        cat("<expected \"string\", got:", word, ">")
      }
    } else if (word == "Scheme_Lang_Atom") {
      inp <- read_expect_integer(inp, 1)
      word_and_inp <- get_next_word(inp)
      word <- word_and_inp[2]
      inp <- word_and_inp[3]
      if (word == "True") {
        inp <- read_expect_integer(inp, 0)
        cat("#true")
      } else if (word == "False") {
        inp <- read_expect_integer(inp, 0)
        cat("#false")
      } else if (word == "integer" || word == "float") {
        num_and_inp <- get_next_word(inp)
        num <- num_and_inp[2]
        inp <- num_and_inp[3]
        cat(num)
      } else if (word == "string") {
        str_and_inp <- str_match(inp, "^(\"(?:\\\\\"|[^\"])*\") (.*)$")
        str <- str_and_inp[2]
        inp <- str_and_inp[3]
        cat(str)
      } else {
        cat("<expected a primitive word, got:", word, ">")
      }
    } else if (word == "Scheme_Lang_SNil") {
      inp <- read_expect_integer(inp, 0)
      if (!in_cdr) {
        cat("()")
      }
    } else if (word == "Scheme_Lang_SCons") {
      inp <- read_expect_integer(inp, 2)
      if (in_cdr) {
        cat(" ")
        in_cdr <- FALSE
      } else {
        cat("(")
      }
    } else {
      cat("<unknown word:", word, ">")
    }

    if (word != "Scheme_Lang_SCons") {
      if (in_cdr) {
        cat(")")
      } else {
        in_cdr <- TRUE
      }
    }
  }

  cat("\n")
}
