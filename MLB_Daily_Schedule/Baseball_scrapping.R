
library(baseballr)

# DATE
date <- Sys.Date()

# Function's
## Today's Game ID
games <- function(date){
  get_game_pks_mlb(date, level_ids = 1)
}
## Today's Starting Pitcher
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
## Today's Lineup
lineup <- function(df) {
  output <- list()
  for (i in seq_along(df)) {
    output[[i]] <- get_batting_orders(df[[i]])
  }
  output
}
# 5 inning results
results <- function(df) {
  output <- list()
  for (i in seq_along(df)) {
    output[[i]] <- get_pbp_mlb(df[[i]]) %>% 
      select('game_pk', 'result.awayScore', 'result.homeScore', 'about.halfInning', 'about.inning', 'matchup.pitcher.fullName', 'home_team', 'away_team') %>% 
      mutate(count = row_number()) %>% 
      filter(about.inning == 5) %>% 
      group_by(game_pk) %>% 
      filter(count == min(count))
  }
  output
}
# Pulling in Today's Game Information
## Today's games 
var_keep <- c("game_pk","gameType", "officialDate", "dayNight", "scheduledInnings",
              "seriesGameNumber","status.detailedState","teams.away.score",
              "teams.away.leagueRecord.pct", "teams.away.team.id",
              "teams.away.team.name","teams.home.score", "teams.home.leagueRecord.pct",
              "teams.home.team.id", "teams.home.team.name", "venue.id", "venue.name")
todays_games <- games(date)
todays_games <- todays_games[, var_keep]
## Today's Schedule
todays_schedule <- data.frame(
  Date = todays_games$officialDate,
  GameID = todays_games$game_pk,
  HomeTeam = todays_games$teams.home.team.name,
  AwayTeam = todays_games$teams.away.team.name,
  HomeSP = unlist(home_sp(todays_games$game_pk)),
  AwaySP = unlist(away_sp(todays_games$game_pk)),
  HomeScore = todays_games$teams.home.score,
  AwayScore = todays_games$teams.away.score
)
## Today's Lineup's
todays_lineup <- lineup(todays_games$game_pk)

# GameID
gameid <- todays_games$game_pk
# Starting Pitcher names
Starting_Pitchers <- vector("numeric", length(gameid))
for (i in seq_along(gameid)) {
  Starting_Pitchers[i] <- get_probables_mlb(gameid[[i]])[3]
}


############# Tomorrow #############
tomorrow <- Sys.Date()+1
tomorrows_games <- games(tomorrow)



