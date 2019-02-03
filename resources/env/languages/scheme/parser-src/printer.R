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
  in_cdr <- FALSE
  inp <- paste0(line, " ")
  while (!is.na(inp)) {
    word_and_inp <- get_next_word(inp)
    word <- word_and_inp[2]
    inp <- word_and_inp[3]

    if (is.na(inp)) {
      break
    }

    if (in_cdr && word != "Scheme_Cons" && word != "Scheme_Nil") {
      cat(" . ")
    }

    if (word == "splice") {
      idx_and_inp <- get_next_word(inp)
      idx <- idx_and_inp[2]
      inp <- idx_and_inp[3]
      cat("\\", idx)
    } else if (word == "Scheme_Symbol") {
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
    } else if (word == "Scheme_Atom") {
      word_and_inp <- get_next_word(inp)
      word <- word_and_inp[2]
      inp <- word_and_inp[3]
      if (word == "True") {
        cat("#true")
      } else if (word == "False") {
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
    } else if (word == "Scheme_Nil") {
      if (!in_cdr) {
        cat("()")
      }
    } else if (word == "Scheme_Cons") {
      if (in_cdr) {
        cat(" ")
        in_cdr <- FALSE
      } else {
        cat("(")
      }
    } else {
      cat("<unknown word:", word, ">")
    }

    if (word != "Scheme_Cons") {
      if (in_cdr) {
        cat(")")
      } else {
        in_cdr <- TRUE
      }
    }
  }

  cat("\n")
}
