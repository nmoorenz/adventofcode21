#' Day 09: Smoke Basin
#'
#' [Smoke Basin](https://adventofcode.com/2021/day/9)
#'
#' @name day09
#' @rdname day09
#' @details
#'
#' **Part One**
#'
#' These caves seem to be [lava
#' tubes](https://en.wikipedia.org/wiki/Lava_tube). Parts are even still
#' volcanically active; small hydrothermal vents release smoke into the
#' caves that slowly [settles like
#' rain]{title="This was originally going to be a puzzle about watersheds, but we're already under water."}.
#'
#' If you can model how the smoke flows through the caves, you might be
#' able to avoid it and be that much safer. The submarine generates a
#' heightmap of the floor of the nearby caves for you (your puzzle input).
#'
#' Smoke flows to the lowest point of the area it\'s in. For example,
#' consider the following heightmap:
#'
#'     2199943210
#'     3987894921
#'     9856789892
#'     8767896789
#'     9899965678
#'
#' Each number corresponds to the height of a particular location, where
#' `9` is the highest and `0` is the lowest a location can be.
#'
#' Your first goal is to find the *low points* - the locations that are
#' lower than any of its adjacent locations. Most locations have four
#' adjacent locations (up, down, left, and right); locations on the edge or
#' corner of the map have three or two adjacent locations, respectively.
#' (Diagonal locations do not count as adjacent.)
#'
#' In the above example, there are *four* low points, all highlighted: two
#' are in the first row (a `1` and a `0`), one is in the third row (a `5`),
#' and one is in the bottom row (also a `5`). All other locations on the
#' heightmap have some lower adjacent location, and so are not low points.
#'
#' The *risk level* of a low point is *1 plus its height*. In the above
#' example, the risk levels of the low points are `2`, `1`, `6`, and `6`.
#' The sum of the risk levels of all low points in the heightmap is
#' therefore `15`.
#'
#' Find all of the low points on your heightmap. *What is the sum of the
#' risk levels of all low points on your heightmap?*
#'
#' **Part Two**
#' Next, you need to find the largest basins so you know what areas are
#' most important to avoid.
#'
#' A *basin* is all locations that eventually flow downward to a single low
#' point. Therefore, every low point has a basin, although some basins are
#' very small. Locations of height `9` do not count as being in any basin,
#' and all other locations will always be part of exactly one basin.
#'
#' The *size* of a basin is the number of locations within the basin,
#' including the low point. The example above has four basins.
#'
#' The top-left basin, size `3`:
#'
#'     2199943210
#'     3987894921
#'     9856789892
#'     8767896789
#'     9899965678
#'
#' The top-right basin, size `9`:
#'
#'     2199943210
#'     3987894921
#'     9856789892
#'     8767896789
#'     9899965678
#'
#' The middle basin, size `14`:
#'
#'     2199943210
#'     3987894921
#'     9856789892
#'     8767896789
#'     9899965678
#'
#' The bottom-right basin, size `9`:
#'
#'     2199943210
#'     3987894921
#'     9856789892
#'     8767896789
#'     9899965678
#'
#' Find the three largest basins and multiply their sizes together. In the
#' above example, this is `9 * 14 * 9 = 1134`.
#'
#' *What do you get if you multiply together the sizes of the three largest
#' basins?*
#'
#' @param x some data
#' @return For Part One, `f09a(x)` returns .... For Part Two,
#'   `f09b(x)` returns ....
#' @export
#' @examples
#' f09a(example_data_09())
#' f09b()
f09a <- function(x) {

  cave_width = nchar(x[[1]])
  cave_length = length(x)

  smokes_L = stringr::str_split_fixed(x, "", cave_width) %>% as.data.frame()
  smokes_L = sapply(smokes_L, as.numeric)

  smokes = array(NA, c(102, 102))
  smokes[2:101, 2:101] = smokes_L

  lowpoints = vector()

  for (i in seq(2, cave_width+1)) {
    for (j in seq(2, cave_length+1)) {
      surr = c(smokes[i,j], smokes[i-1,j], smokes[i+1,j], smokes[i,j-1], smokes[i,j+1])
      if (min(surr, na.rm=TRUE) == smokes[i,j]
          & smokes[i,j] != 9) {
        lowpoints = append(lowpoints, smokes[i,j])
        # print(i)
        # print(j)
      }
    }
  }

  sum(lowpoints) + length(lowpoints)
}


#' @rdname day09
#' @export
f09_chart <- function(x) {

  cave_width = nchar(x[[1]])
  cave_length = length(x)

  smokes = stringr::str_split_fixed(x, "", cave_width) %>% as.data.frame()
  smokes = sapply(smokes, as.numeric)

  smoker = smokes %>%
    as.data.frame() %>%
    mutate(rr = row_number()) %>%
    pivot_longer(-rr, "cols") %>%
    mutate(cols = parse_number(cols))

  smoker %>%
    ggplot(aes(rr, cols, colour = factor(value))) +
    geom_point() +
    scale_color_brewer(palette = "Set3")

  basin_grey = c("#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9", "#222222")

  smoker %>%
    ggplot(aes(rr, cols, colour = factor(value))) +
    geom_point() +
    scale_color_manual(values = basin_grey)



}


f09b <- function(x) {
  cave_width = nchar(x[[1]])
  cave_length = length(x)

  smokes_L = stringr::str_split_fixed(x, "", cave_width) %>% as.data.frame()
  smokes_L = sapply(smokes_L, as.numeric)

  smokes = array(NA, c(102, 102))
  smokes[2:101, 2:101] = smokes_L

  # basin_tracker
  basin_t = array(NA, c(102, 102))

  basin_num = 0

  for (k in seq(40)) {
    for (i in seq(2, cave_width+1)) {
      for (j in seq(2, cave_length+1)) {
        surr = c(basin_t[i-1,j], basin_t[i+1,j], basin_t[i,j-1], basin_t[i,j+1])
        if (smokes[i,j] == 9) {
          # if it is a 9 do nothing, keep as NA

        } else if (any(surr[!is.na(surr)])) {
          # if there is a basin number in any surrounding spot, adopt that number
          basin_t[i,j] = min(surr[!is.na(surr)])

        } else {
          # if we find no surrounding basin number,
          # add one to the basin and start a new basin counter
          basin_num = basin_num + 1
          basin_t[i,j] = basin_num

        }
      }
    }
  }

  basin_count = basin_t %>%
    as.data.frame() %>%
    mutate(rr = row_number()) %>%
    pivot_longer(-rr, "cols") %>%
    mutate(cols = parse_number(cols)) %>%
    count(value) %>%
    arrange(desc(n))

  # part two answer
  prod(basin_count$n[2:4])
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day09
#' @export
example_data_09 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
