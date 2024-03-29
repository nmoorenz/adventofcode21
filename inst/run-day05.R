library(adventofcode21)
library(tidyverse)

x <- readLines("./inst/input05.txt")

p1 <- f05a(x)
p2 <- f05b(x)

stopifnot(p1 == aoc_solutions$day05a)
stopifnot(p2 == aoc_solutions$day05b)

# 955867
# 7831
#
