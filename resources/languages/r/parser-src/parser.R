#!/usr/bin/env Rscript
# TODO Fix handling dots (specifically cases like (foo . (bar)))
library(rlang)
library(stringi)
library(stringr)
options(useFancyQuotes = FALSE)

raiseSyntax <- function(msg) {
  write(paste0("syntax error - ", msg), stderr())
  quit(save="no", status=1, runLast=FALSE)
}
writeWord <- function(word) {
  cat(word, " ", sep="")
}
writeNode <- function(node) {
  cat("R_", node, " ", sep="")
}
writeString <- function(expr) {
  writeWord("string")
  writeWord(dQuote(stri_escape_unicode(expr)))
}
writeSeparator <- function() {
  cat("\n")
}
writeList <- function(writeElem, lst) {
  lstNames <- names(lst)
  lstLength <- length(lst)
  if (lstLength > 0) {
    for (idx in 1:lstLength) {
      elem <- lst[[idx]]
      name <- lstNames[[idx]]
      writeWord("Cons")
      if (!is.null(name) && name != "") {
        writeNode("Named")
        writeString(name)
      }
      if (is_missing(elem)) {
        writeNode("Missing")
      } else {
        writeElem(elem)
      }
    }
  }
  writeWord("Nil")
}

writeExpr <- function(expr) {
  if (is_syntactic_literal(expr)) { # Constants
    writeNode("Literal")
    if (is.null(expr)) {
      writeWord("Unit")
    } else if (identical(expr, TRUE)) {
      writeWord("True")
    } else if (identical(expr, FALSE)) {
      writeWord("False")
    } else if (is.numeric(expr)) {
      if (expr%%1 == 0) {
        writeWord("integer")
      } else {
        writeWord("float")
      }
      writeWord(toString(expr))
    } else if (is_string(expr)) {
      writeString(expr)
    } else {
      raiseSyntax(paste("unknown constant -", str(expr)))
    }
  } else if (is_symbol(expr)) { # Symbols and splices
    lit <- as_string(expr)
    spliceIdx <- str_match(lit, "^splice_([0-9]+)$")[[2]]
    if (!is.na(spliceIdx)) { #Splice
      writeWord("splice")
      writeWord(spliceIdx)
    } else { # Symbol
      writeNode("Symbol")
      writeString(as_string(expr))
    }
  } else if (is_call(expr)) { # Function calls
    writeNode("Call")
    writeExpr(expr[[1]])
    writeList(writeExpr, as.list(expr[-1]))
  } else if (is_pairlist(expr)) { # Pair lists
    writeNode("PairList")
    writeList(writeExpr, expr)
  } else {
    raiseSyntax(paste("expression of unknown type -", str(expr)))
  }
}

stdin = file("stdin")
open(stdin)

text <- gsub("(?<!\\\\)\\\\([0-9]+)", "splice_\\1", readLines(stdin), perl=TRUE)
exprs <- parse(text = text)
for (expr in exprs) {
  writeExpr(expr)
  writeSeparator()
}

close(stdin)
