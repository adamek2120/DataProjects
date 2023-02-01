######################################
#                                    #
#   Scrape up to date season stats   #
#                                    #
######################################

library(baseballr)

# Date Range
openingDay <- '2021-04-01'
today <- Sys.Date()

# Pull in up-to-date Batting and Pitching stats
batting <- daily_batter_bref(openingDay, today)
pitching <- daily_pitcher_bref(openingDay, today)



