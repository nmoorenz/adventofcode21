#' Day 22: Reactor Reboot
#'
#' [Reactor Reboot](https://adventofcode.com/2021/day/22)
#'
#' @name day22
#' @rdname day22
#' @details
#'
#' **Part One**
#'
#' Operating at these extreme ocean depths has overloaded the submarine\'s
#' reactor; it needs to be rebooted.
#'
#' The reactor core is made up of a large 3-dimensional grid made up
#' entirely of cubes, one cube per integer 3-dimensional coordinate
#' (`x,y,z`). Each cube can be either *on* or *off*; at the start of the
#' reboot process, they are all *off*. (Could it be an old model of a
#' reactor you\'ve seen [before](/2020/day/17)?)
#'
#' To reboot the reactor, you just need to set all of the cubes to either
#' *on* or *off* by following a list of *reboot steps* (your puzzle input).
#' Each step specifies a [cuboid](https://en.wikipedia.org/wiki/Cuboid)
#' (the set of all cubes that have coordinates which fall within ranges for
#' `x`, `y`, and `z`) and whether to turn all of the cubes in that cuboid
#' *on* or *off*.
#'
#' For example, given these reboot steps:
#'
#'     on x=10..12,y=10..12,z=10..12
#'     on x=11..13,y=11..13,z=11..13
#'     off x=9..11,y=9..11,z=9..11
#'     on x=10..10,y=10..10,z=10..10
#'
#' The first step (`on x=10..12,y=10..12,z=10..12`) turns *on* a 3x3x3
#' cuboid consisting of 27 cubes:
#'
#' -   `10,10,10`
#' -   `10,10,11`
#' -   `10,10,12`
#' -   `10,11,10`
#' -   `10,11,11`
#' -   `10,11,12`
#' -   `10,12,10`
#' -   `10,12,11`
#' -   `10,12,12`
#' -   `11,10,10`
#' -   `11,10,11`
#' -   `11,10,12`
#' -   `11,11,10`
#' -   `11,11,11`
#' -   `11,11,12`
#' -   `11,12,10`
#' -   `11,12,11`
#' -   `11,12,12`
#' -   `12,10,10`
#' -   `12,10,11`
#' -   `12,10,12`
#' -   `12,11,10`
#' -   `12,11,11`
#' -   `12,11,12`
#' -   `12,12,10`
#' -   `12,12,11`
#' -   `12,12,12`
#'
#' The second step (`on x=11..13,y=11..13,z=11..13`) turns *on* a 3x3x3
#' cuboid that overlaps with the first. As a result, only 19 additional
#' cubes turn on; the rest are already on from the previous step:
#'
#' -   `11,11,13`
#' -   `11,12,13`
#' -   `11,13,11`
#' -   `11,13,12`
#' -   `11,13,13`
#' -   `12,11,13`
#' -   `12,12,13`
#' -   `12,13,11`
#' -   `12,13,12`
#' -   `12,13,13`
#' -   `13,11,11`
#' -   `13,11,12`
#' -   `13,11,13`
#' -   `13,12,11`
#' -   `13,12,12`
#' -   `13,12,13`
#' -   `13,13,11`
#' -   `13,13,12`
#' -   `13,13,13`
#'
#' The third step (`off x=9..11,y=9..11,z=9..11`) turns *off* a 3x3x3
#' cuboid that overlaps partially with some cubes that are on, ultimately
#' turning off 8 cubes:
#'
#' -   `10,10,10`
#' -   `10,10,11`
#' -   `10,11,10`
#' -   `10,11,11`
#' -   `11,10,10`
#' -   `11,10,11`
#' -   `11,11,10`
#' -   `11,11,11`
#'
#' The final step (`on x=10..10,y=10..10,z=10..10`) turns *on* a single
#' cube, `10,10,10`. After this last step, `39` cubes are *on*.
#'
#' The initialization procedure only uses cubes that have `x`, `y`, and `z`
#' positions of at least `-50` and at most `50`. For now, ignore cubes
#' outside this region.
#'
#' Here is a larger example:
#'
#'     on x=-20..26,y=-36..17,z=-47..7
#'     on x=-20..33,y=-21..23,z=-26..28
#'     on x=-22..28,y=-29..23,z=-38..16
#'     on x=-46..7,y=-6..46,z=-50..-1
#'     on x=-49..1,y=-3..46,z=-24..28
#'     on x=2..47,y=-22..22,z=-23..27
#'     on x=-27..23,y=-28..26,z=-21..29
#'     on x=-39..5,y=-6..47,z=-3..44
#'     on x=-30..21,y=-8..43,z=-13..34
#'     on x=-22..26,y=-27..20,z=-29..19
#'     off x=-48..-32,y=26..41,z=-47..-37
#'     on x=-12..35,y=6..50,z=-50..-2
#'     off x=-48..-32,y=-32..-16,z=-15..-5
#'     on x=-18..26,y=-33..15,z=-7..46
#'     off x=-40..-22,y=-38..-28,z=23..41
#'     on x=-16..35,y=-41..10,z=-47..6
#'     off x=-32..-23,y=11..30,z=-14..3
#'     on x=-49..-5,y=-3..45,z=-29..18
#'     off x=18..30,y=-20..-8,z=-3..13
#'     on x=-41..9,y=-7..43,z=-33..15
#'     on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
#'     on x=967..23432,y=45373..81175,z=27513..53682
#'
#' The last two steps are fully outside the initialization procedure area;
#' all other steps are fully within it. After executing these steps in the
#' initialization procedure region, `590784` cubes are *on*.
#'
#' Execute the reboot steps. Afterward, considering only cubes in the
#' region `x=-50..50,y=-50..50,z=-50..50`, *how many cubes are on?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f22a(x)` returns .... For Part Two,
#'   `f22b(x)` returns ....
#' @export
#' @examples
#' f22a(example_data_22())
#' f22b()
f22a <- function(x) {

  cube = tibble(zz = x) %>%
    separate(zz,
             into = c("dd", "x_", "x1", "b_", "x2", "y_", "y1", "c_", "y2", "z_", "z1", "d_", "z2"),
             sep = "[, =\\.]") %>%
    select(!ends_with("_")) %>%
    mutate(across(x1:z2, as.integer),
           across(x1:z2, ~ . + 50))

  reboot = array(0, c(101, 101, 101))


  for (i in seq(20)) {
    if (cube$dd[i] == "on") {
      chng = 1
    } else {
      chng = 0
    }
    reboot[cube$x1[i]:cube$x2[i], cube$y1[i]:cube$y2[i], cube$z1[i]:cube$z2[i]] = chng
  }

  sum(reboot)
}


#' @rdname day22
#' @export
f22b <- function(x) {
  # create instructions
  cube = tibble(zz = x) %>%
    separate(zz,
             into = c("dd", "x_", "x1", "b_", "x2", "y_", "y1", "c_", "y2", "z_", "z1", "d_", "z2"),
             sep = "[, =\\.]") %>%
    select(!ends_with("_")) %>%
    mutate(across(x1:z2, as.integer))

  # reboot needs to be smarter than initialising the whole thing
  cube_min = cube %>%
    summarise(across(x1:z2, min))

  # add 100,000 to everything
  cube = cube %>% mutate(across(x1:z2, ~ . + 100000))

  # list of arrays
  reboot = vector("list", nrow(cube))

  # loop through instructions
  for (i in seq_len(nrow(cube))) {
    xx = abs(cube$x1[i] - cube$x2[i]) + 1
    yy = abs(cube$y1[i] - cube$y2[i]) + 1
    zz = abs(cube$z1[i] - cube$z2[i]) + 1

    if (cube$dd[i] == "on") {
      arr = array(1, c(xx, yy, zz),
                  list(cube$x1[i]:cube$x2[i], cube$y1[i]:cube$y2[i], cube$z1[i]:cube$z2[i]))

      reboot[[i]] = arr

    } else {
      chng = 0
    }
  }

  # answer
  sum(unlist(reboot))
}


f22_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day22
#' @export
example_data_22 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
