######## Functions for Baseball Model ##########
library(baseballr)


# Functions
games <- function(date){
  get_game_pks_mlb(date, level_ids = 1)
}
away_sp <- function(df) {
  output <- vector("numeric", length(df))
  for (i in seq_along(df)) {
    output[i] <- get_probables_mlb(df[[i]])[1,3]
  }
  output
}
home_sp <- function(df) {
  output <- vector("numeric", length(df))
  for (i in seq_along(df)) {
    output[i] <- get_probables_mlb(df[[i]])[2,3]
  }
  output
}
result_5in <- function(gameid){
  get_pbp_mlb(gameid) %>%
    select('game_pk', 'result.awayScore', 'result.homeScore', 'about.halfInning', 'about.inning', 'matchup.pitcher.fullName', 'home_team', 'away_team') %>% 
    mutate(count = row_number()) %>% 
    filter(about.inning == 5) %>% 
    filter(count == min(count)) 
}
result_1in <- function(gameid){
  get_pbp_mlb(gameid) %>%
    select('game_pk', 'result.awayScore', 'result.homeScore', 'about.halfInning', 'about.inning', 'matchup.pitcher.fullName', 'home_team', 'away_team') %>% 
    mutate(count = row_number()) %>% 
    filter(about.inning == 1) %>% 
    filter(count == min(count)) 
}
lineup <- function(df) {
  output <- list()
  for (i in seq_along(df)) {
    output[[i]] <- get_batting_orders(df[[i]])
  }
  output
}





