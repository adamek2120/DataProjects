########################
#
# Change lines on 32, 36)
#
#
#



# Creating object with the address
tr_url <- "https://www.teamrankings.com/mlb/team-stats/"
tr <- read_html(tr_url)

# Getting links
tr_links <- tr %>% 
  html_nodes("a") %>% 
  html_attr("href")
head(tr_links,10)


mlb_links <- tr_links[str_detect(tr_links,"mlb/stat")]
head(mlb_links)


mlb_links <- mlb_links[!str_detect(mlb_links, "/mlb/stats/")]

# Puts links in dataframe
df <- tibble(stat_links = mlb_links)

# Find per first inning stats
df <- df %>% 
  mutate(is_per_first = str_detect(stat_links, "first-5-innings-runs-per-game")) %>% 
  filter(is_per_first == TRUE)

df1 <- df %>% 
  mutate(is_opp = str_detect(stat_links, "opponent-first-5-innings-runs-per-game")) %>% 
  filter(is_opp == TRUE)

# Fixing the links
df <- df %>% 
  mutate(url = paste0('https://www.teamrankings.com', stat_links, '?date=2018-10-28'))
df1 <- df1 %>% 
  mutate(url = paste0('https://www.teamrankings.com', stat_links, '?date=2018-10-28'))
df1 %>% 
  head() %>% 
  knitr::kable()

# Downloadinbg the web page
get_page <- function(url){
  page <- read_html(url)
  Sys.sleep(sample(seq(.25,2.5,.25),1))
  page
}

page_data <- map(df$url, get_page)
page_data1 <- map(df1$url, get_page)
tr_data <- map(page_data, html_table)
tr_data1 <- map(page_data1, html_table)
# Binding all the tables together
tr_data <- pluck(tr_data, 1) %>% 
  map2_df(df$stat_links, 
          ~as_tibble(.x) %>% 
            mutate(stat = .y)) %>% 
  set_names(c(
    'rank',
    'team',
    'current_seas',
    'last_3',
    'last_1',
    'home',
    'away',
    'last_seas',
    'stat'
  ))
tr_data1 <- pluck(tr_data1, 1) %>% 
  map2_df(df1$stat_links, 
          ~as_tibble(.x) %>% 
            mutate(stat = .y)) %>% 
  set_names(c(
    'rank',
    'team',
    'current_seas',
    'last_3',
    'last_1',
    'home',
    'away',
    'last_seas',
    'stat'
  ))

# Create two datasets for team stat and Opp stat
team_data <- tr_data %>% 
  filter(stat == '/mlb/stat/first-5-innings-runs-per-game') %>% 
  select(-last_3, -last_1, -stat)
opp_data <- tr_data1 %>% 
  select(-last_3, -last_1, -stat)

# Created a table associating each team name in TR with the team name from baseballr to substitute for it:
team_lkup <- c(Cleveland="Cleveland Indians", Washington="Washington Nationals", Miami="Miami Marlins", Atlanta="Atlanta Braves", `Chi Cubs` ="Chicago Cubs", `St. Louis` ="St. Louis Cardinals",
               Pittsburgh = "Pittsburgh Pirates", Cincinnati = 'Cincinnati Reds', Houston = 'Houston Astros', Milwaukee ='Milwaukee Brewers', `San Diego` = 'San Diego Padres', Arizona = 'Arizona Diamondbacks',
               Colorado = 'Colorado Rockies', `LA Dodgers` = 'Los Angeles Dodgers', `NY Mets` = 'New York Mets', Philadelphia = 'Philadelphia Phillies', `Kansas City` = 'Kansas City Royals', `Chi Sox` = 'Chicago White Sox',
               Minnesota = 'Minnesota Twins', Detroit = 'Detroit Tigers', Oakland = 'Oakland Athletics', Texas = 'Texas Rangers', `LA Angels` = 'Los Angeles Angels', Seattle = 'Seattle Mariners', 
               Baltimore = 'Baltimore Orioles', Boston = 'Boston Red Sox', `NY Yankees` = 'New York Yankees', Toronto = 'Toronto Blue Jays', `Tampa Bay` = 'Tampa Bay Rays', `SF Giants` = 'San Francisco Giants')
team_data$Team <- as.character(team_lkup[team_data$team])
opp_data$Team <- as.character(team_lkup[opp_data$team])

team_data <- team_data[, c(7,3:6)]
opp_data <- opp_data[, c(7,3:6)]

# Turning percentage column to numeric
team_data$current_seas <- as.numeric(sub("%", "", team_data$current_seas,fixed=TRUE))/100
team_data$home <- as.numeric(sub("%", "", team_data$home,fixed=TRUE))/100
team_data$away <- as.numeric(sub("%", "", team_data$away,fixed=TRUE))/100
team_data$last_seas <- as.numeric(sub("%", "", team_data$last_seas,fixed=TRUE))/100

# Leaves on taem_data and opp_data
rm(df, df1, page_data, page_data1, tr, tr_data, tr_data1, mlb_links, tr_links, get_page, tr_url, team_lkup)




