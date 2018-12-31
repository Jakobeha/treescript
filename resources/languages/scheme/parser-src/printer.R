#!/usr/bin/env Rscript
# TODO Fix handling dots (specifically cases like (foo . (bar)))
library(stringr)
options(useFancyQuotes = FALSE)

stdin <- file("stdin")
open(stdin)

cat("TODO")