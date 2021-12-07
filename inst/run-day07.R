library(adventofcode21)

x <- readLines("./inst/input07.txt")

p1 <- f07a(x)
p2 <- f07b(x)

stopifnot(p1 == aoc_solutions$day07a)
stopifnot(p2 == aoc_solutions$day07b)

# 96987919
# 96987874
