#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(magrittr)
  library(purrr)
  library(rlang)
  library(stringi)
  library(stringr)
})
options(useFancyQuotes = FALSE)

stdin <- file("stdin")
open(stdin)

raiseSyntax <- function(msg) {
  write(paste0("syntax error - ", msg), stderr())
  quit(save="no", status=1, runLast=FALSE)
}

formatFancy <- function(expr) {
  recurse <- function(expr, f) {
    recurseF <- function(expr) {
      recurse(f(expr), f)
    }
    if (is.call(expr)) {
      as.call(lapply(expr, recurseF))
    } else if (is.pairlist(expr)) {
      as.pairlist(lapply(expr, recurseF))
    } else {
      expr
    }
  }
  flattenNested <- function(expr) {
    if (is.call(expr) && expr[[1]] == as.symbol("{")) {
      as.call(flatten(lapply(expr, flatten1)))
    } else {
      expr
    }
  }
  flatten1 <- function(expr) {
    if (is_call(expr) && expr[[1]] == as.symbol("{")) {
      as.list(expr[-1])
    } else {
      list(expr)
    }
  }

  expr %>%
    recurse(flattenNested) %>%
    flatten1()
}

for (line in readLines(stdin)) {
  inp <- paste0(line, " ")

  checkRead <- function(type, nextAndInp) {
    if (is.na(nextAndInp[2])) {
      raiseSyntax(paste0("while parsing ", type, ", expected more words from: ", inp))
    }
  }
  peekWord <- function() {
    wordAndInp <- str_match(inp, "^([^ ]+) (.*)$")
    list(wordAndInp[2], wordAndInp[3])
  }
  readWord <- function() {
    wordAndInp <- str_match(inp, "^([^ ]+) (.*)$")
    checkRead("word", wordAndInp)
    inp <<- wordAndInp[3]
    wordAndInp[2]
  }
  readString <- function() {
    strAndInp <- str_match(inp, "^\"((?:\\\\\"|[^\"])*)\" (.*)$")
    checkRead("string", strAndInp)
    inp <<- strAndInp[3]
    stri_unescape_unicode(strAndInp[2])
  }
  readPrimString <- function() {
    lit <- readWord()
    if (lit != "string") {
      raiseSyntax(paste("expected string literal, got:", lit))
    }
    readString()
  }
  readList <- function(readElem) {
    word <- readWord()
    switch (word,
      "Cons" = {
        name <- {
          nextAndInp <- peekWord()
          if (nextAndInp[1] == "R_Named") {
            inp <<- nextAndInp[2]
            c(readPrimString())
          } else {
            NULL
          }
        }
        first <- readElem()
        head <- {
          if (is_missing(first)) {
            list(missing_arg())
          } else {
            force(first)
            list(first)
          }
        }
        names(head) <- name
        rest <- readList(readElem)
        c(head, rest)
      },
      "Nil" = list(),
      raiseSyntax(paste("expected a list, got:", word))
    )
  }
  readExpr <- function() {
    word <- readWord()
    switch (word,
      "R_Literal" = {
        lit <- readWord()
        switch (lit,
          "Unit" = NULL,
          "True" = TRUE,
          "False" = FALSE,
          "integer" = as.integer(readWord()),
          "float" = as.double(readWord()),
          "string" = readString(),
          raiseSyntax(paste("literal of unknown type:", lit))
        )
      },
      "splice" = {
        idx <- readWord()
        as.symbol(paste0("splice_", idx))
      },
      "R_Symbol" = as.symbol(readPrimString()),
      "R_Call" = {
        head <- readExpr()
        args <- readList(readExpr)
        call2(head, splice(args))
      },
      "R_PairList" = as.pairlist(readList(readExpr)),
      "R_Missing" = missing_arg(),
      raiseSyntax(paste("value of unknown type:", word))
    )
  }

  expr <- readExpr()

  if (!grepl("\\s*", inp)) {
    raiseSyntax(paste("extra words after value:", inp))
  }

  exprs <- formatFancy(expr)

  for (expr in exprs) {
    expr_print(expr)
  }
}
