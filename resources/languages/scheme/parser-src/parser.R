#!/usr/bin/env Rscript
# TODO Fix handling dots (specifically cases like (foo . (bar)))
library(stringr)
options(useFancyQuotes = FALSE)

stdin <- file("stdin")
open(stdin)
level <- 0
didCons <- FALSE

writeWord <- function(word) {
  cat(word, " ", sep="")
}
writeNode <- function(node) {
  cat("Scheme_", node, " ", sep="")
}
writeSeparator <- function() {
  cat("\n")
}
raiseSyntax <- function(msg) {
  write(paste0("syntax error - ", msg), stderr())
  quit(save="no", status=1, runLast=FALSE)
}

for (line in readLines(stdin, warn=FALSE)) {
  for (token in str_extract_all(line, "\"(\\\\\"|[^\"])*\"|[\\(\\[\\{\\}\\]\\)]|[^ \\(\\[\\{\\}\\]\\)\"]+")[[1]]) {
    if (!(token %in% c(")", "]", "}", "."))) {
      if (level == 0) {
        writeSeparator()
      } else if (!didCons) {
        writeNode("Cons")
      }
    }

    if (grepl("^\\\\[0-9]+$", token)) {
      idxStr <- str_sub(token, 2)
      writeWord("splice")
      writeWord(idxStr)
    } else if (token %in% c("(", "[", "{")) {
      level <- level + 1
      didCons <- FALSE
    } else if (token %in% c(")", "]", "}")) {
      if (level == 0) {
        raiseSyntax("unmatched closing paren")
      }
      if (!didCons) {
        writeNode("Nil")
      }
      level <- level - 1
    } else if (token == ".") {
      if (level == 0) {
        raiseSyntax("dot at top level")
      } else if (didCons) {
        raiseSyntax("multiple dots in list")
      } else {
        didCons <- TRUE
      }
    } else if (suppressWarnings(all(!is.na(as.numeric(token))))) {
      writeNode("Atom")
      if (grepl("\\.", token)) {
        writeWord("float")
      } else {
        writeWord("integer")
      }
      writeWord(token)
    } else if (grepl("\"", token)) {
      writeNode("Atom")
      writeWord("string")
      writeWord(token)
    } else {
      writeNode("Symbol")
      writeWord("string")
      writeWord(dQuote(token))
    }
  }
}

if (level != 0) {
  raiseSyntax("unmatched open paren")
}

close(stdin)
