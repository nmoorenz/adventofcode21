library(adventofcode21)
library(tidyverse)

x <- readLines("./inst/input03.txt")

p1 <- f03a(x)
p2 <- f03b(x)

stopifnot(p1 == aoc_solutions$day03a)
stopifnot(p2 == aoc_solutions$day03b)

# part two
# attempt 1
# 4205097
# attempt 2
# 1877139
