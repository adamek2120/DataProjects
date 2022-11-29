############################# SIMPLY RUN ALL ######################
path_rdat <- "S:/jadam/Coding/R/Sport_Models/Baseball/"
setwd(path_rdat)
load('SP_5in_wt.Rdata')

# Load Packages
library(baseballr)

# Yesterdays DATE
date <- Sys.Date() - 1
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
# Pulling in Yesterday's Game Information
## Yesterday's games 
var_keep <- c("game_pk","gameType", "officialDate", "dayNight", "scheduledInnings",
              "seriesGameNumber","status.detailedState","teams.away.score",
              "teams.away.leagueRecord.pct", "teams.away.team.id",
              "teams.away.team.name","teams.home.score", "teams.home.leagueRecord.pct",
              "teams.home.team.id", "teams.home.team.name", "venue.id", "venue.name")
yesterday_games <- games(date)
yesterday_games <- yesterday_games[, var_keep]

# Yesterday's 5 inning results
result_5in <- function(gameid){
  get_pbp_mlb(gameid) %>%
    select('game_pk', 'result.awayScore', 'result.homeScore', 'about.halfInning', 'about.inning', 'matchup.pitcher.fullName', 'home_team', 'away_team') %>% 
    mutate(count = row_number()) %>% 
    filter(about.inning == 5) %>% 
    filter(count == min(count)) 
}

yesterday_results <- results(yesterday_games$game_pk)
yesterday_results <- as.data.frame(matrix(unlist(yesterday_results),nrow=length(yesterday_results),byrow=TRUE))
col_names <- c('game_pk', 'AwayScore', 'HomeScore', 'about.halfInning', 'about.inning', 'matchup.pitcher.fullName', 'HomeTeam', 'AwayTeam', 'count')
colnames(yesterday_results) <- col_names

## Yesterday's Results
yesterday_schedule <- data.frame(
  Date = rep(date, nrow(yesterday_results)),
  GameID = yesterday_games$game_pk,
  HomeTeam = yesterday_results$HomeTeam,
  AwayTeam = yesterday_results$AwayTeam,
  HomeSP = home_sp(yesterday_games$game_pk),
  AwaySP = away_sp(yesterday_games$game_pk),
  HomeScore = yesterday_results$HomeScore,
  AwayScore = yesterday_results$AwayScore
)


# Linear Model Prediction #####
##Creates the Databases
df.home <- data.frame(
  GameID = yesterday_schedule$GameID,
  HomeSP = yesterday_schedule$HomeSP
)
df.home <- merge(df.home, SP_5in_wt, by.x = "HomeSP", by.y="Name", all.x = TRUE)
df.away <- data.frame(
  GameID = yesterday_schedule$GameID,
  AwaySP = yesterday_schedule$AwaySP
)
df.away <- merge(df.away, SP_5in_wt, by.x = "AwaySP", by.y="Name", all.x = TRUE)
## Merges and changes names
Linear_ERA_Prediction <- merge(df.home, df.away, by = 'GameID')
Linear_ERA_Prediction <- Linear_ERA_Prediction[, c(1,2,4,3,5)]
colnames(Linear_ERA_Prediction)[4] <- 'H_Pred_era'  
colnames(Linear_ERA_Prediction)[5] <- 'A_Pred_era'
## CLeans environment
rm(df.home, df.away)
## MAKES PREDICTIONS BASED ON DIFFERENCES IN ERA
Linear_ERA_Prediction$Decision <- ifelse(Linear_ERA_Prediction$H_Pred_era < Linear_ERA_Prediction$A_Pred_era, 'Home', 'Away')
Linear_ERA_Prediction$Decision_1.0 <- ifelse(Linear_ERA_Prediction$H_Pred_era - Linear_ERA_Prediction$A_Pred_era > 1.00, 'Away', 
                                         ifelse(Linear_ERA_Prediction$A_Pred_era - Linear_ERA_Prediction$H_Pred_era > 1.00, 'Home', 'NO BET'))
Linear_ERA_Prediction$Decision_1.25 <- ifelse(Linear_ERA_Prediction$H_Pred_era - Linear_ERA_Prediction$A_Pred_era > 1.25, 'Away', 
                                             ifelse(Linear_ERA_Prediction$A_Pred_era - Linear_ERA_Prediction$H_Pred_era > 1.25, 'Home', 'NO BET'))
Linear_ERA_Prediction$Decision_1.5 <- ifelse(Linear_ERA_Prediction$H_Pred_era - Linear_ERA_Prediction$A_Pred_era > 1.5, 'Away', 
                                             ifelse(Linear_ERA_Prediction$A_Pred_era - Linear_ERA_Prediction$H_Pred_era > 1.5, 'Home', 'NO BET'))
# Results and Predictions
yesterday_prediction <- yesterday_schedule %>% 
  left_join(Linear_ERA_Prediction, by = c('GameID', 'HomeSP', 'AwaySP')) %>% 
  mutate(Results = ifelse(HomeScore > AwayScore, 'Home', ifelse(HomeScore < AwayScore, 'Away', 'Tie'))) %>% 
  select(GameID, starts_with('Decision'), Results)

# Save results to folder
filename <- paste0('S:/jadam/Coding/R/Sport_Models/Baseball/2021_results/Half_Game_Results/Results_', date, '.csv')
filenameR <- paste0('S:/jadam/Coding/R/Sport_Models/Baseball/2021_results/Half_Game_Results/Results_', date, '.Rdata')
write_csv(yesterday_prediction, filename)
save(yesterday_prediction, file = filenameR) 


#############################################
#                                           #
#         1_FirstInn_Model                  #
#                                           #
#                                           #
#############################################

# First inning results
results <- function(df) {
  output <- list()
  for (i in seq_along(df)) {
    output[[i]] <- get_pbp_mlb(df[[i]]) %>% 
      select('game_pk', 'result.awayScore', 'result.homeScore', 'about.halfInning', 'about.inning', 'matchup.pitcher.fullName', 'home_team', 'away_team') %>% 
      mutate(count = row_number()) %>% 
      filter(about.inning == 1) %>% 
      group_by(game_pk) %>% 
      filter(count == min(count))
  }
  output
}

yesterday_results <- results(yesterday_games$game_pk)
yesterday_results <- as.data.frame(matrix(unlist(yesterday_results),nrow=length(yesterday_results),byrow=TRUE))
col_names <- c('game_pk', 'AwayScore', 'HomeScore', 'about.halfInning', 'about.inning', 'matchup.pitcher.fullName', 'HomeTeam', 'AwayTeam', 'count')
colnames(yesterday_results) <- col_names



