#!/usr/bin/env Rscript
# TODO Fix handling dots (specifically cases like (foo . (bar)))
library(stringr)
options(useFancyQuotes = FALSE)

stdin <- file("stdin")
open(stdin)
level <- 0
didCons <- FALSE

writeNode <- function(node) {
  cat(node, " ")
}
writeSeparator <- function() {
  cat("\n")
}

for (line in readLines(stdin)) {
  for (token in str_extract_all(line, "\"(\\\\\"|[^\"])*\"|[\\(\\[\\{\\}\\]\\)]|[^ \\(\\[\\{\\}\\]\\)\"]+")[[1]]) {
    if (!(token %in% c(")", "]", "}", "."))) {
      if (level == 0) {
        writeSeparator()
      } else if (!didCons) {
        writeNode("Cons")
      }
    }
    
    if (grepl("^\\\\[0-9]+$", token)) {
      idxStr <- grep("[0-9]+$", token)
      writeNode("splice")
      writeNode(idxStr)
    } else if (token %in% c("(", "[", "{")) {
      level <- level + 1
      didCons <- FALSE
    } else if (token %in% c(")", "]", "}")) {
      if (level == 0) {
        stop("syntax error - unmatched closing paren")
      }
      if (!didCons) {
        writeNode("Nil")
      }
      level <- level - 1
    } else if (token == ".") {
      if (level == 0) {
        stop("syntax error - dot at top level")
      } else if (didCons) {
        stop("syntax error - multiple dots in list")
      } else {
        didCons <- TRUE
      }
    } else if (suppressWarnings(all(!is.na(as.numeric(token))))) {
      writeNode("Atom")
      if (grepl("\\.", token)) {
        writeNode("float")
      } else {
        writeNode("integer")
      }
      writeNode(token)
    } else if (grepl("\"", token)) {
      writeNode("Atom")
      writeNode("string")
      writeNode(token)
    } else {
      writeNode("Symbol")
      writeNode("string")
      writeNode(dQuote(token))
    }
  }
}

if (level != 0) {
  stop("syntax error - unmatched open paren")
}

close(stdin)