#' @title Create a Monty Hall Game
#' @description Randomly arrange two goats and one car behind three doors.
#' @details No arguments are required. Vector positions represent doors 1 to 3.
#'   After an initial selection, the host reveals an unselected goat door.
#'   The contestant can then stay or switch to the other unopened door.
#' @return A character vector of length three containing two "goat" values
#'   and one "car" value.
#' @examples
#' game <- create_game()
#' game
#' stopifnot(length(game) == 3, sum(game == "goat") == 2,
#'           sum(game == "car") == 1)
#' @export
create_game <- function()
{
  prizes <- c("goat", "goat", "car")
  return(sample(prizes, size = 3, replace = FALSE))
}


#' @title Select an Initial Door
#' @description Randomly select one of the three doors.
#' @details No arguments are required. Each door has an equal chance of
#'   selection, independently of the prize locations.
#' @return A single integer between 1 and 3 identifying the selected door.
#' @examples
#' pick <- select_door()
#' pick
#' stopifnot(length(pick) == 1, pick %in% 1:3)
#' @export
select_door <- function()
{
  return(sample(1:3, size = 1))
}


#' @title Open an Unselected Goat Door
#' @description Reveal a goat without opening the contestant's selected door.
#' @details If the contestant selected the car, choose randomly between the
#'   two goat doors. Otherwise, open the only unselected goat door.
#' @param game A character vector of length three containing two "goat"
#'   values and one "car" value, in door order.
#' @param a.pick A single numeric door number, 1, 2, or 3, selected initially.
#' @return A single numeric door number identifying the opened goat door.
#' @examples
#' game <- c("goat", "car", "goat")
#' opened <- open_goat_door(game, a.pick = 2)
#' stopifnot(opened %in% c(1, 3), game[opened] == "goat")
#' stopifnot(open_goat_door(game, a.pick = 1) == 3)
#' stopifnot(open_goat_door(game, a.pick = 3) == 1)
#' @export
open_goat_door <- function(game, a.pick)
{
  possible.doors <- setdiff(which(game == "goat"), a.pick)
  
  if (length(possible.doors) == 1)
  {
    return(possible.doors)
  }
  
  return(sample(possible.doors, size = 1))
}


#' @title Stay with or Change the Selected Door
#' @description Determine the final door under the stay or switch strategy.
#' @details Staying keeps the initial selection. Switching chooses the only
#'   door that is neither the initial selection nor the opened goat door.
#' @param stay A single logical value: TRUE to stay (default), FALSE to switch.
#' @param opened.door A single numeric door number, 1, 2, or 3, identifying
#'   the opened goat door. It must differ from a.pick.
#' @param a.pick A single numeric door number, 1, 2, or 3, selected initially.
#' @return A single numeric door number identifying the final selection.
#' @examples
#' change_door(TRUE, opened.door = 3, a.pick = 1)
#' change_door(FALSE, opened.door = 3, a.pick = 1)
#' stopifnot(change_door(TRUE, 3, 1) == 1,
#'           change_door(FALSE, 3, 1) == 2)
#' @export
change_door <- function(stay = TRUE, opened.door, a.pick)
{
  if (stay)
  {
    return(a.pick)
  }
  
  return(setdiff(1:3, c(opened.door, a.pick)))
}


#' @title Determine Whether the Contestant Wins
#' @description Check the prize behind the final selected door.
#' @details Selecting the car wins; selecting either goat loses.
#' @param final.pick A single numeric door number, 1, 2, or 3, selected finally.
#' @param game A character vector of length three containing two "goat"
#'   values and one "car" value, in door order.
#' @return A single character value: "WIN" for the car or "LOSE" for a goat.
#' @examples
#' game <- c("goat", "car", "goat")
#' determine_winner(2, game)
#' determine_winner(1, game)
#' stopifnot(determine_winner(2, game) == "WIN",
#'           determine_winner(1, game) == "LOSE",
#'           determine_winner(3, game) == "LOSE")
#' @export
determine_winner <- function(final.pick, game)
{
  if (game[final.pick] == "car")
  {
    return("WIN")
  }
  
  return("LOSE")
}


#' @title Play One Game with Both Strategies
#' @description Play a three-door game and record stay and switch outcomes.
#' @details No arguments are required. Both strategies use the same game,
#'   initial selection, and opened door. Exactly one strategy wins each game.
#' @return A data frame with two rows and two character columns:
#'   strategy ("stay", "switch") and outcome ("WIN" or "LOSE").
#' @examples
#' result <- play_game()
#' result
#' stopifnot(identical(result$strategy, c("stay", "switch")),
#'           sum(result$outcome == "WIN") == 1,
#'           sum(result$outcome == "LOSE") == 1)
#' @export
play_game <- function()
{
  game <- create_game()
  pick <- select_door()
  opened <- open_goat_door(game, pick)
  
  stay.pick <- change_door(TRUE, opened, pick)
  switch.pick <- change_door(FALSE, opened, pick)
  
  return(data.frame(
    strategy = c("stay", "switch"),
    outcome = c(
      determine_winner(stay.pick, game),
      determine_winner(switch.pick, game)
    ),
    stringsAsFactors = FALSE
  ))
}


#' @title Simulate Multiple Monty Hall Games
#' @description Repeat the game and compare outcomes for staying and switching.
#' @details Each game evaluates both strategies. The function prints a table
#'   of outcome proportions within each strategy, rounded to two decimals.
#'   Simulations approach win rates of one-third for staying and two-thirds
#'   for switching as the number of games increases.
#' @param n A single positive whole number giving the number of games.
#'   Defaults to 100.
#' @return A data frame with 2 * n rows and two character columns:
#'   strategy ("stay" or "switch") and outcome ("WIN" or "LOSE").
#'   Each consecutive pair of rows represents one game.
#' @examples
#' results <- play_n_games(10)
#' head(results)
#' stopifnot(nrow(results) == 20,
#'           sum(results$strategy == "stay") == 10,
#'           sum(results$strategy == "switch") == 10,
#'           sum(results$outcome == "WIN") == 10)
#' @export
play_n_games <- function(n = 100)
{
  if (!is.numeric(n) || length(n) != 1L || is.na(n) ||
      !is.finite(n) || n < 1 || n != floor(n))
  {
    stop("n must be a single positive whole number.")
  }
  
  results.list <- vector("list", n)
  
  for (i in seq_len(n))
  {
    results.list[[i]] <- play_game()
  }
  
  results.df <- dplyr::bind_rows(results.list)
  
  print(round(
    prop.table(table(results.df), margin = 1),
    2
  ))
  
  return(results.df)
}