#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
} 



#' @title
#' Inidicate which door number was selected by contestant. 
#' 
#' @description
#' select_door()generates the door number that was chosen by the contestant.
#' 
#' @details
#' The contestant makes their first selection and they have to choose one of the three doors where there are two goats and one car hiding.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a one numerical number indicating the chosen door.
#' 
#' @examples
#' select_door()
#' 
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Host Opens Goat Door.
#' 
#' @description
#' open_goat_door() generates door with goat behind it to be opened by the host.
#' 
#' @details
#' The host will open a goat door and a door different than the one that was chosen by the contestant.So if the contestant has chose initially a car door then the host can now choose randomly any other door, but if the contestant has initially chose a door where there is a goat behind it, then now the host must choose the other goat door so the host will have in this case one option door to be choosed. 
#' 
#' @param  ... initial door selected by the contestant
#' 
#' @return The function returns a one numerical number indicating which door is slected by the host.
#' 
#' @examples
#' open_goat_door()
#' 
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  # if contestant selected car,
  # randomly select one of two goats 
  if( game[ a.pick ] == "car" )
  { 
    goat.doors <- doors[ game != "car" ] 
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  { 
    opened.door <- doors[ game != "car" & doors != a.pick ] 
  }
  return( opened.door ) # number between 1 and 3
}



#' @title
#' Change Doors.
#' 
#' @description
#' change_door()generates a number of the door that the contestant choses if he made a decision to switch doors.
#'
#' @details
#' The contestant is given the option to change from their initial selection to the other door that is still closed. 
#' 
#' @param ... a logical vector whether the contestant chooses to stay or change his initial selection.
#' 
#' @return The function returns a door number which is between one and three.
#' 
#' @examples
#' change_door()
#' 
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3) 
  
  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ] 
  }
  
  return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine If Contestant Has Won.
#' 
#' @description
#' determine_winner() generates the result which is whether the contestant has won or lost.
#' 
#' @details
#' If the contestant final choice was a door with car then he is a winner and if his choice was a door with a goat behind it then he is a loser.
#' 
#' @param ... the door picked by the contestant.
#' 
#' @return the result wich is winner or loser.
#' 
#' @examples
#' determine_winner()
#' 
#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}





#' @title
#' Wrap the game steps into a single function.
#' 
#' @description
#' play_game() wraps the game steps into a single function which includes within it all the required steps. 
#' 
#' @details
#' This is a wrap function that includes within it all other functions and by the end we can know depending on whether the contestant has chose to switch or stay whther he is a winner or loser.
#' 
#' @param ... numeric and logical vectors of the game steps.
#' 
#' @return the result of the game which is whether the contestant has won or lose.
#' 
#' @examples
#' play_game()
#' 
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )
  
  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )
  
  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Build a loop Function. 
#' 
#' @description
#' play_n_games() generates the result if the game has been palyed for n tims and here for instance n is 100.
#' 
#' @details
#' This is a loop function designed to build a simulation that plays the game for 100 times.
#' 
#' @param ... iterator values
#' 
#' @return the fuction returns the result of the the decisions taken whether to switch or stay.
#' 
#' @examples
#' play_n_games()
#' 
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1
  
  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )
  
  table( results.df ) %>% 
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>% 
    print()
  
  return( results.df )
  
}
