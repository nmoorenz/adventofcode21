#' Day 04: Giant Squid
#'
#' [Giant Squid](https://adventofcode.com/2021/day/4)
#'
#' @name day04
#' @rdname day04
#' @details
#'
#' **Part One**
#'
#' You\'re already almost 1.5km (almost a mile) below the surface of the
#' ocean, already so deep that you can\'t see any sunlight. What you *can*
#' see, however, is a giant squid that has attached itself to the outside
#' of your submarine.
#'
#' Maybe it wants to play
#' [bingo](https://en.wikipedia.org/wiki/Bingo_(American_version))?
#'
#' Bingo is played on a set of boards each consisting of a 5x5 grid of
#' numbers. Numbers are chosen at random, and the chosen number is *marked*
#' on all boards on which it appears. (Numbers may not appear on all
#' boards.) If all numbers in any row or any column of a board are marked,
#' that board *wins*. (Diagonals don\'t count.)
#'
#' The submarine has a *bingo subsystem* to help passengers (currently, you
#' and the giant squid) pass the time. It automatically generates a random
#' order in which to draw numbers and a random set of boards (your puzzle
#' input). For example:
#'
#'     7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
#'
#'     22 13 17 11  0
#'      8  2 23  4 24
#'     21  9 14 16  7
#'      6 10  3 18  5
#'      1 12 20 15 19
#'
#'      3 15  0  2 22
#'      9 18 13 17  5
#'     19  8  7 25 23
#'     20 11 10 24  4
#'     14 21 16 12  6
#'
#'     14 21 17 24  4
#'     10 16 15  9 19
#'     18  8 23 26 20
#'     22 11 13  6  5
#'      2  0 12  3  7
#'
#' After the first five numbers are drawn (`7`, `4`, `9`, `5`, and `11`),
#' there are no winners, but the boards are marked as follows (shown here
#' adjacent to each other to save space):
#'
#'     22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
#'      8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
#'     21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
#'      6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
#'      1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
#'
#' After the next six numbers are drawn (`17`, `23`, `2`, `0`, `14`, and
#' `21`), there are still no winners:
#'
#'     22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
#'      8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
#'     21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
#'      6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
#'      1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
#'
#' Finally, `24` is drawn:
#'
#'     22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
#'      8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
#'     21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
#'      6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
#'      1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
#'
#' At this point, the third board *wins* because it has at least one
#' complete row or column of marked numbers (in this case, the entire top
#' row is marked: `14 21 17 24  4`).
#'
#' The *score* of the winning board can now be calculated. Start by finding
#' the *sum of all unmarked numbers* on that board; in this case, the sum
#' is `188`. Then, multiply that sum by *the number that was just called*
#' when the board won, `24`, to get the final score, `188 * 24 = 4512`.
#'
#' To guarantee victory against the giant squid, figure out which board
#' will win first. *What will your final score be if you choose that
#' board?*
#'
#' **Part Two**
#' On the other hand, it might be wise to try a different strategy: [let
#' the giant squid
#' win]{title="That's 'cuz a submarine don't pull things' antennas out of their sockets when they lose. Giant squid are known to do that."}.
#'
#' You aren\'t sure how many bingo boards a giant squid could play at once,
#' so rather than waste time counting its arms, the safe thing to do is to
#' *figure out which board will win last* and choose that one. That way, no
#' matter which boards it picks, it will win for sure.
#'
#' In the above example, the second board is the last to win, which happens
#' after `13` is eventually called and its middle column is completely
#' marked. If you were to keep playing until this point, the second board
#' would have a sum of unmarked numbers equal to `148` for a final score of
#' `148 * 13 = 1924`.
#'
#' Figure out which board will win last. *Once it wins, what would its
#' final score be?*
#'
#' @param x bingo number calls, and a number of bingo cards
#' @return For Part One, `f04a(x)` returns the product of the latest
#' number to be called when one card has a bingo, and the uncalled numbers
#' on that bingo card. For Part Two, `f04b(x)` returns the last number
#' to create a winning bingo card, i.e. all other cards have won, and the
#' sum of the uncalled numbers on that last card.
#' @export
#' @examples
#' f04a(example_data_04())
#' f04b()
f04a <- function(x) {

  # bingo numbers, first row in text file
  bingo_numbers = as.numeric(unlist(strsplit(x[1], ",")))

  # bingo cards
  bingo_cards = create_long_card_record(x[2:length(x)])

  # "call" our bingo numbers but stop if we have a bingo
  for (j in 1:length(bingo_numbers)) {
    my_num = bingo_numbers[j]
    print(my_num)
    bingo_cards = bingo_cards %>%
      mutate(called = (called | value == my_num))

    do_we_have_bingo = check_cards_1(bingo_cards)

    if (do_we_have_bingo) break

  }

  # winning card numbers that haven't been called yet
  uncalled = bingo_cards %>%
    filter(called == FALSE & card_num == do_we_have_bingo) %>%
    summarise(win_card = sum(value))

  # part one answer
  uncalled$win_card[[1]] * my_num


}


#' @rdname day04
#' @export
f04b <- function(x) {
  # bingo numbers, first row in text file
  bingo_numbers = as.numeric(unlist(strsplit(x[1], ",")))

  # bingo cards
  bingo_cards = create_long_card_record(x[2:length(x)])

  bingo_cards = bingo_cards %>%
    mutate(bingo_done = FALSE)

  # "call" our bingo numbers but stop if we have a bingo
  for (j in 1:length(bingo_numbers)) {
  # for (j in 1:50) {
      my_num = bingo_numbers[j]
    print(my_num)
    bingo_cards = bingo_cards %>%
      mutate(called = (called | value == my_num))

    # check rows for bingo
    do_we_have_rows = check_rows(bingo_cards)
    # adjust record of which cards have bingo
    if (length(do_we_have_rows) == 1) {
      bingo_cards = bingo_cards %>%
        mutate(bingo_done = (bingo_done | do_we_have_rows == card_num))
    } else {
      for (x in do_we_have_rows) {
        bingo_cards = bingo_cards %>%
          mutate(bingo_done = (bingo_done | x == card_num))
      }
    }

    # check cols for bingo
    do_we_have_cols = check_cols(bingo_cards)
    # adjust record of which cards have bingo
    if (length(do_we_have_cols) == 1) {
      bingo_cards = bingo_cards %>%
        mutate(bingo_done = (bingo_done | do_we_have_cols == card_num))
    } else {
      for (x in do_we_have_cols) {
        bingo_cards = bingo_cards %>%
          mutate(bingo_done = (bingo_done | x == card_num))
      }
    }

    if (sum(bingo_cards$bingo_done) == 2500) break

  }

  # "winning" card numbers that haven't been called yet
  # winning for the second part is actually last to win
  if (do_we_have_cols) {
    last_card = do_we_have_cols
  } else {
    last_card = do_we_have_rows
  }
  uncalled = bingo_cards %>%
    filter(called == FALSE & card_num == last_card) %>%
    summarise(win_card = sum(value))

  # part two answer
  uncalled$win_card[[1]] * my_num


}


check_cards_1 <- function(cards) {
  # group by card and row
  # group by card and col

  row_bingo = cards %>%
    group_by(card_num, row_num) %>%
    summarise(bingo = sum(called), .groups = "drop") %>%
    filter(bingo == 5)

  if (nrow(row_bingo) == 1) return (row_bingo$card_num[[1]])

  col_bingo = cards %>%
    group_by(card_num, col_num) %>%
    summarise(bingo = sum(called), .groups = "drop") %>%
    filter(bingo == 5)

  if (nrow(col_bingo) == 1) return (col_bingo$card_num[[1]])

  return(FALSE)

}


check_rows <- function(cards) {
  # group by card and row

  row_bingo = cards %>%
    filter(bingo_done == FALSE) %>%
    group_by(card_num, row_num) %>%
    summarise(bingo = sum(called), .groups = "drop") %>%
    filter(bingo == 5)

  if (nrow(row_bingo) >= 1) return (row_bingo$card_num)

  return(FALSE)

}


check_cols <- function(cards) {
  # group by card and col

  col_bingo = cards %>%
    filter(bingo_done == FALSE) %>%
    group_by(card_num, col_num) %>%
    summarise(bingo = sum(called), .groups = "drop") %>%
    filter(bingo == 5)

  if (nrow(col_bingo) >= 1) return (col_bingo$card_num)

  return(FALSE)

}


create_long_card_record <- function(z) {
  # separate the columns
  # number the bingo cards
  # number the rows
  # remove na
  tibble(bb = z) %>%
    separate(bb, c("c1", "c2", "c3", "c4", "c5"), sep = c(3, 6, 9, 12), convert = TRUE) %>%
    mutate(row_na = is.na(c1),
           card_num = cumsum(row_na)) %>%
    group_by(card_num) %>%
    mutate(row_num = row_number() - 1) %>%
    ungroup() %>%
    filter(row_num != 0) %>%
    select(-row_na) %>%
    pivot_longer(c("c1", "c2", "c3", "c4", "c5"), names_to = "col_num") %>%
    mutate(col_num = parse_number(col_num),
           called = FALSE)
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day04
#' @export
example_data_04 <- function(example = 1) {
  l <- list(
    a = c("7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1",
  NA,
  "22 13 17 11  0",
  "8  2 23  4 24",
  "21  9 14 16  7",
  "6 10  3 18  5",
  "1 12 20 15 19",
  NA,
  "3 15  0  2 22",
  "9 18 13 17  5",
  "19  8  7 25 23",
  "20 11 10 24  4",
  "14 21 16 12  6",
  NA,
  "14 21 17 24  4",
  "10 16 15  9 19",
  "18  8 23 26 20",
  "22 11 13  6  5",
  "2  0 12  3  7")
  )
  l[[example]]
}
