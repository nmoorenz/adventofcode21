#' Day 05: Hydrothermal Venture
#'
#' [Hydrothermal Venture](https://adventofcode.com/2021/day/5)
#'
#' @name day05
#' @rdname day05
#' @details
#'
#' **Part One**
#'
#' You come across a field of [hydrothermal
#' vents](https://en.wikipedia.org/wiki/Hydrothermal_vent) on the ocean
#' floor! These vents constantly produce large, opaque clouds, so it would
#' be best to avoid them if possible.
#'
#' They tend to form in *lines*; the submarine helpfully produces a list of
#' nearby [lines of vents]{title="Maybe they're Bresenham vents."} (your
#' puzzle input) for you to review. For example:
#'
#'     0,9 -> 5,9
#'     8,0 -> 0,8
#'     9,4 -> 3,4
#'     2,2 -> 2,1
#'     7,0 -> 7,4
#'     6,4 -> 2,0
#'     0,9 -> 2,9
#'     3,4 -> 1,4
#'     0,0 -> 8,8
#'     5,5 -> 8,2
#'
#' Each line of vents is given as a line segment in the format
#' `x1,y1 -> x2,y2` where `x1`,`y1` are the coordinates of one end the line
#' segment and `x2`,`y2` are the coordinates of the other end. These line
#' segments include the points at both ends. In other words:
#'
#' -   An entry like `1,1 -> 1,3` covers points `1,1`, `1,2`, and `1,3`.
#' -   An entry like `9,7 -> 7,7` covers points `9,7`, `8,7`, and `7,7`.
#'
#' For now, *only consider horizontal and vertical lines*: lines where
#' either `x1 = x2` or `y1 = y2`.
#'
#' So, the horizontal and vertical lines from the above list would produce
#' the following diagram:
#'
#'     .......1..
#'     ..1....1..
#'     ..1....1..
#'     .......1..
#'     .112111211
#'     ..........
#'     ..........
#'     ..........
#'     ..........
#'     222111....
#'
#' In this diagram, the top left corner is `0,0` and the bottom right
#' corner is `9,9`. Each position is shown as *the number of lines which
#' cover that point* or `.` if no line covers that point. The top-left pair
#' of `1`s, for example, comes from `2,2 -> 2,1`; the very bottom row is
#' formed by the overlapping lines `0,9 -> 5,9` and `0,9 -> 2,9`.
#'
#' To avoid the most dangerous areas, you need to determine *the number of
#' points where at least two lines overlap*. In the above example, this is
#' anywhere in the diagram with a `2` or larger - a total of `5` points.
#'
#' Consider only horizontal and vertical lines. *At how many points do at
#' least two lines overlap?*
#'
#' **Part Two**
#' Unfortunately, considering only horizontal and vertical lines doesn\'t
#' give you the full picture; you need to also consider *diagonal lines*.
#'
#' Because of the limits of the hydrothermal vent mapping system, the lines
#' in your list will only ever be horizontal, vertical, or a diagonal line
#' at exactly 45 degrees. In other words:
#'
#' -   An entry like `1,1 -> 3,3` covers points `1,1`, `2,2`, and `3,3`.
#' -   An entry like `9,7 -> 7,9` covers points `9,7`, `8,8`, and `7,9`.
#'
#' Considering all lines from the above example would now produce the
#' following diagram:
#'
#'     1.1....11.
#'     .111...2..
#'     ..2.1.111.
#'     ...1.2.2..
#'     .112313211
#'     ...1.2....
#'     ..1...1...
#'     .1.....1..
#'     1.......1.
#'     222111....
#'
#' You still need to determine *the number of points where at least two
#' lines overlap*. In the above example, this is still anywhere in the
#' diagram with a `2` or larger - now a total of `12` points.
#'
#' Consider all of the lines. *At how many points do at least two lines
#' overlap?*
#'
#' @param x lines for plumes of sea vents
#' @return For Part One, `f05a(x)` returns number of overlapping plumes
#' on horizontal and vertical alignments. For Part Two,
#'   `f05b(x)` returns overlapping plumes on any alignment.
#' @export
#' @examples
#' f05a(example_data_05())
#' f05b()
f05a <- function(x) {

  plumes = create_plumes(x)

  # range from 10 to 990
  sea_floor = array(0, c(1000,1000))

  for (j in 1:nrow(plumes)) {
    if (plumes$vert[[j]] | plumes$hori[[j]]) {

      sea_floor[plumes$x1[[j]]:plumes$x2[[j]], plumes$y1[[j]]:plumes$y2[[j]]] = 1 + sea_floor[plumes$x1[[j]]:plumes$x2[[j]], plumes$y1[[j]]:plumes$y2[[j]]]
    }
  }

  # part one answer
  sum(sea_floor > 1)
}


#' @rdname day05
#' @export
f05b <- function(x) {
  plumes = create_plumes(x)

  sea_floor = array(0, c(1000,1000))

  for (j in 1:nrow(plumes)) {
    # sequences for x and y
    xx = seq(plumes$x1[[j]], plumes$x2[[j]])
    yy = seq(plumes$y1[[j]], plumes$y2[[j]])
    # adjust for vert or horizontal
    if (length(xx) == 1) xx = rep(xx, length(yy))
    if (length(yy) == 1) yy = rep(yy, length(xx))
    # loop for sea floor positions
    for (k in seq_len(max(length(xx), length(yy)))) {
        sea_floor[xx[k], yy[k]] = sea_floor[xx[k], yy[k]] + 1
    }
  }

  # part one answer
  sum(sea_floor > 1)
}


create_plumes <- function(x) {
  tibble(init = x) %>%
    separate(init, c("x1y1", "x2y2"), sep = " ->") %>%
    separate(x1y1, c("x1", "y1"), sep = ",", convert = TRUE) %>%
    separate(x2y2, c("x2", "y2"), sep = ",", convert = TRUE) %>%
    mutate(vert = x1 == x2,
           hori = y1 == y2)
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day05
#' @export
example_data_05 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
