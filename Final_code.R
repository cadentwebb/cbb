#Libraries necessary
library(tidyverse)
library(caret)
library(dplyr)
library(gbm)
library(ranger)

#Read in available data

#Historic box score data
boxscore <- read.csv("player_half_boxscore_history.csv", quote = "", stringsAsFactors = F)
head(boxscore)

#High school rankings
hs_rank <- read.csv("HS_Rankings (1).csv", stringsAsFactors = F)


#Merge Confrences onto hs rankings
hs_rank <- left_join(hs_rank, conferences, by = c("college" = "team_market"))
hs_rank$conference_name <- ifelse(hs_rank$college == "NC State", "Atlantic Coast",
                                  hs_rank$conference_name )
hs_rank$conference_name <- ifelse(hs_rank$college == "Miami", "Atlantic Coast",
                                  hs_rank$conference_name )
hs_rank$conference_name <- ifelse(hs_rank$college == "USF", "American Athletic",
                                  hs_rank$conference_name )
hs_rank$conference_name <- ifelse(hs_rank$college == "Loyola (Chi)", "Missouri Valley",
                                  hs_rank$conference_name )
hs_rank$conference_name <- ifelse(hs_rank$college == "Saint Joseph's", "Atlantic 10",
                                  hs_rank$conference_name )
hs_rank$conference_name <- ifelse(hs_rank$college == "VCU", "Atlantic 10",
                                  hs_rank$conference_name )
hs_rank$conference_name <- ifelse(hs_rank$college == "Wisconsin-Green Bay", "Horizon",
                                  hs_rank$conference_name )
hs_rank$conference_name <- ifelse(hs_rank$college == "Middle Tennessee State", "Conference USA",
                                  hs_rank$conference_name )
hs_rank$conference_name <- ifelse(hs_rank$college == "College of Charleston", "Colonial",
                                  hs_rank$conference_name )
hs_rank$conference_name <- ifelse(hs_rank$college == "American", "Patriot League",
                                  hs_rank$conference_name )
hs_rank$conference_name <- ifelse(hs_rank$college == "Nebraska Omaha", "Summit League",
                                  hs_rank$conference_name )
hs_rank$conference_name <- ifelse(hs_rank$college == "Saint Joseph's", "Atlantic 10",
                                  hs_rank$conference_name )
hs_rank$conference_name <- ifelse(hs_rank$college == "St. Mary's", "West Coast",
                                  hs_rank$conference_name )
hs_rank$conference_name <- ifelse(hs_rank$college == "Long Island", "Northeast",
                                  hs_rank$conference_name )
hs_rank$conference_name <- ifelse(hs_rank$college == "FIU", "Conference USA",
                                  hs_rank$conference_name )
hs_rank$conference_name <- ifelse(hs_rank$college == "UNC Wilmington", "Colonial",
                                  hs_rank$conference_name )
hs_rank$conference_name <- ifelse(hs_rank$college == "Bryant", "Northeast",
                                  hs_rank$conference_name )
hs_rank$conference_name <- ifelse(hs_rank$college == "Louisiana", "Sun Belt",
                                  hs_rank$conference_name )
hs_rank$conference_name <- ifelse(hs_rank$college == "UT Martin", "Ohio Valley",
                                  hs_rank$conference_name )
hs_rank$conference_name <- ifelse(hs_rank$college == "Saint Peter's", "Metro Atlantic Athletic",
                                  hs_rank$conference_name )
hs_rank$conference_name <- ifelse(hs_rank$college == "Wisconsin-Milwaukee", "Horizon",
                                  hs_rank$conference_name )
hs_rank$conference_name <- ifelse(hs_rank$college == "Loyola Maryland", "Patriot League",
                                  hs_rank$conference_name )
hs_rank$conference_name <- ifelse(hs_rank$college == "South Carolina Upstate", "Big South",
                                  hs_rank$conference_name )
hs_rank$conference_name <- ifelse(hs_rank$college == "SIU Edwardsville", "Ohio Valley",
                                  hs_rank$conference_name )



#Used to make sure all conferences are good
#conf_stuff <- hs_rank %>%
#  filter(is.na(conference_name)) %>%
#  filter(college != "Professional" & college != "") %>%
#  select(college, conference_name)


#Rename column
hs_rank <- hs_rank %>% 
  rename(
    full_name = name)


#Get unique names from high school 
un_hs <- hs_rank %>%
  distinct(full_name, .keep_all = TRUE)

#Group by half

#Remove players with same name but different player id
unique_names <- boxscore %>%
  select(full_name, player_id) %>%
  distinct(player_id, .keep_all = TRUE) %>%
  distinct(full_name)

names_list <- unique_names$full_name

#Keep only unique names 
new_box <- boxscore[boxscore$full_name %in% names_list, ]


#merge on name
combined <- merge(new_box, un_hs, by = "full_name")

#sum(is.na(combined$ranking))

#combine game halfs

#One hot encode positions
combined$pg <- ifelse(combined$position.y == " PG ",1,0)
combined$sg <- ifelse(combined$position.y == " SG ",1,0)
combined$sf <- ifelse(combined$position.y == " SF ",1,0)
combined$pf <- ifelse(combined$position.y == " PF ",1,0)
combined$c <- ifelse(combined$position.y == " C ",1,0)




#Get height in inches
height_1 <- hs_rank$height[2]

testing <- unlist(strsplit(as.character(height_1), "[-]" ))
as.numeric(testing[2])

combined$inches <- sapply(strsplit(as.character(combined$height),"[-]"),
                          function(x){12*as.numeric(x[1]) + as.numeric(x[2])})

#Drop height/conference NAs
combined <- combined %>%
  filter(!is.na(inches)) %>% 
  filter(!is.na(conference_name))

#Get per game data

#Get unique players, games

all_players <- unique(combined$player_id)
all_games <- unique(combined$game_id)

#initialize the new dataframe we will use 

per_game_df <- data.frame(full_name = character(), player_id = character(), team_id = character(), 
                          game_id =  character(), points = integer(), rebounds = integer(), 
                          ranking = double(), assists = integer(), conference_name = character(), pg = integer(), sg = integer(), 
                          sf = integer(), pf = integer(), c = integer(), inches = integer())

for(game in all_games){
  #Separate by game
  this_game <- combined[(combined$game_id == game),]
  #Get list of players in each game
  players_in_game <- unique(this_game$player_id)
  for (player in players_in_game) {
    #Isolate player
    this_player <- this_game[(this_game$player_id == player),]
    #Get the 2 halves of each game
    sep_periods <- this_player %>% 
      distinct(period_id, .keep_all = TRUE)
    #Convert assists to numeric so you can sum it
    sep_periods$assists <- as.numeric(sep_periods$assists)
    #Add totals to dataframe
    per_game_df <- add_row(per_game_df, tibble_row(full_name = sep_periods$full_name[1], 
                                                   player_id = sep_periods$player_id[1], team_id = sep_periods$team_id.x[1], 
                                                   game_id =  sep_periods$game_id[1], 
                                                   points = sum(sep_periods$points), rebounds = sum(sep_periods$rebounds), 
                                                   ranking = sep_periods$ranking[1], assists = sum(sep_periods$assists), 
                                                   conference_name = sep_periods$conference_name[1], 
                                                   pg = sep_periods$pg[1], sg = sep_periods$sg[1], 
                                                   sf = sep_periods$sf[1], pf = sep_periods$pf[1], 
                                                   c = sep_periods$c[1], inches = sep_periods$inches[1]))
    
  }
  
}


library(caret)

#Split into train/test

per_game_df$conference_name <- as.factor(per_game_df$conference_name)
point_index <- createDataPartition(per_game_df$points, p = 0.7, list = FALSE)
train_data <- per_game_df[point_index, ]
test_data  <- per_game_df[-point_index, ]



#Random forest model Points


points_rf_model <- model <- ranger(
  formula         = points ~ conference_name + ranking + pg + sg + sf + pf + c + inches,
  data            = per_game_df, 
  num.trees       = 500,
  mtry            = 6,
  min.node.size   = 5,
  sample.fraction = 0.632,
  seed            = 123,
  respect.unordered.factors = TRUE
)

#Save final model
#saveRDS(points_rf_model, "points_rf_mod.RDS")

#See how predictions do
rf_preds <- predict(points_rf_model, test_data)

#Look at RMSE
RMSE(rf_preds$predictions, test_data$points)

importance(rf_model)
rf_model




#Usage by half
#season usage
#Get season stats

#combine halves

#Get usage percentage
#Get the team data for usage 
#Must use all boxscore data not just filtered

tm_all_players <- unique(boxscore$player_id)
tm_all_games <- unique(boxscore$game_id)

#Use total minutes
#boxscore$minutes <- as.numeric(as.difftime(boxscore$minutes,format="%M:%S",units="mins"))
boxscore$total_minutes <- as.numeric(as.difftime(boxscore$total_minutes,format="%M:%S",units="mins"))

#Create df to get necessary data 
tm_per_game_df <- data.frame(full_name = character(), player_id = character(), team_id = character(), 
                             game_id =  character(), points = integer(), rebounds = integer(), assists = integer(), minutes = double(),
                             fg_att = integer(), ft_att = integer(),  to = integer())

#Loop to get players from each game
#Similar to above but with box score (unfiltered) data

for(game in tm_all_games){
  #Separate by game
  this_game <- boxscore[(boxscore$game_id == game),]
  #Get list of players in each game
  players_in_game <- unique(this_game$player_id)
  for (player in players_in_game) {
    #Isolate player
    this_player <- this_game[(this_game$player_id == player),]
    #Get the 2 halves of each game
    sep_periods <- this_player %>% 
      distinct(period_id, .keep_all = TRUE)
    #Convert assists to numeric so you can sum it
    sep_periods$assists <- as.numeric(sep_periods$assists)
    #Add totals to dataframe
    tm_per_game_df <- add_row(tm_per_game_df, tibble_row(full_name = sep_periods$full_name[1], 
                                                         player_id = sep_periods$player_id[1], team_id = sep_periods$team_id[1], 
                                                         game_id =  sep_periods$game_id[1], 
                                                         points = sum(sep_periods$points), rebounds = sum(sep_periods$rebounds), 
                                                         assists = sum(sep_periods$assists),
                                                         minutes = sep_periods$total_minutes[1], fg_att = sum(sep_periods$field_goals_att),
                                                         ft_att = sum(sep_periods$free_throws_att), to = sum(sep_periods$turnovers)))
    
  }
  #Use to see progress
  # if (cnt == 500 | cnt == 1000 |cnt == 1500 |cnt == 2000 |cnt == 2500 |cnt == 3000 |cnt == 3500 | cnt == 4000 | cnt == 4500 | cnt == 5000) {
  #   print(cnt)
  # }
  # cnt <- cnt + 1
}


#Get team statistics in this loop 
new_all_games <- unique(tm_per_game_df$game_id)

tm_stats <- data.frame(team_id = character(), game_id =  character(), tm_points = integer(),
                       tm_rebounds = integer(), tm_assists = integer(), tm_fg_att = integer(), tm_ft_att = integer(),
                       tm_to = integer(), tm_mp = integer())

#Separate into games
for (game in new_all_games) {
  this_game <- tm_per_game_df[(tm_per_game_df$game_id == game),]
  tms <- unique(this_game$team_id)
  #Get totals for each team in each game
  for (team in tms) {
    this_team_game <- this_game[(this_game$team_id == team),]
    tm_stats <- add_row(tm_stats, tibble_row(team_id = team, game_id =  game, tm_points = sum(this_team_game$points),
                                             tm_rebounds = sum(this_team_game$rebounds), tm_assists = sum(this_team_game$assists),
                                             tm_fg_att = sum(this_team_game$fg_att), tm_ft_att = sum(this_team_game$ft_att),
                                             tm_to = sum(this_team_game$to), tm_mp = sum(this_team_game$minutes)))
    
    
  }
}

#Combine team stats with per game player data
#Team totals are their own column
with_tm_totals <- merge(per_game_df, tm_stats, by=c("game_id", "team_id"))

##Include season
years_and_id <- combined %>%
  select(game_id, season) %>%
  distinct(game_id, .keep_all = TRUE)
with_tm_totals_yr <- left_join(with_tm_totals, years_and_id, by="game_id")

#Now get usage

years <- unique(with_tm_totals_yr$season)

#Give usage its own df. Will give a player's usage for each season
usg_df <- data.frame(player_id = character(), team_id = character(), season = integer(), usage = double())

#Go through each year
for(yr in years){
  season <- with_tm_totals_yr[(with_tm_totals_yr$season == yr),]
  teams <- unique(with_tm_totals_yr$team_id)
  
  #Go through each team
  for(team in teams) {
    select_team <- season[(season$team_id == team),]
    players <- unique(select_team$player_id)
    sep_game <- select_team %>% distinct(game_id, .keep_all = TRUE)
    
    #Go through each player 
    for(plr in players){
      select_player <- select_team[(select_team$player_id == plr),]
      #Denominator calculation
      usg_denom <- (sum(sum(sep_game$tm_fg_att),0.44*sum(sep_game$tm_ft_att),
                        sum(sep_game$tm_to))) * 5 * (sum(select_player$minutes))
      usg_numer <- 100*(sum(sum(as.numeric(select_player[,'assists'])),sum(select_player[,'to']), 
                            sum(select_player[,'fg_att']),0.44*sum(select_player[,'ft_att'])))*(sum(sep_game$tm_mp))
      #Numerator calculation
      usage <- usg_numer / usg_denom
      ##Add to df
      usg_df <- add_row(usg_df, tibble_row(player_id = select_player$player_id[1], team_id = select_player$team_id[1],
                                           season = as.numeric(select_player$season[1]), usage = usage))
    }
  }
}

#Get rid of infinities and NAs
usg_df$usage <- ifelse(is.na(usg_df$usage) | usg_df$usage == "Inf",0,usg_df$usage)

just_usg <- usg_df %>%
  select(player_id, usage)

w_usage <- left_join(usg_df,with_tm_totals_yr)

#Get rid of 0 usage
w_usage <- w_usage %>% 
  filter(usage != 0)

#unique(w_usage[c("player_id", "season")])

#Keep only unique player/season combinations (ie Colbey Ross in 2018, Ross in 2019, etc.)
filtered_usg <- w_usage[!duplicated(w_usage[c(1,3)]),]

#Usage model
usage_index <- createDataPartition(filtered_usg$usage, p = 0.7, list = FALSE)
train_data <- filtered_usg[usage_index, ]
test_data  <- filtered_usg[-usage_index, ]



usage_model <- model <- ranger(
  formula         = usage ~ conference_name + ranking + pg + sg + sf + pf + c + inches,
  data            = filtered_usg,
  num.trees       = 600,
  mtry            = 4,
  min.node.size   = 4,
  sample.fraction = 0.8,
  seed            = 123
)
usage_model

saveRDS(usage_model, "usage_rf_mod.RDS")

#Rebounds model
rebounds_model <- model <- ranger(
  formula         = rebounds ~ conference_name + ranking + pg + sg + sf + pf + c + inches,
  data            = per_game_df,
  num.trees       = 500,
  mtry            = 5,
  min.node.size   = 4,
  sample.fraction = 0.7,
  seed            = 123
)
# 
rebounds_preds <- predict(rebounds_model, test_data)

RMSE(rebounds_preds$predictions, test_data$rebounds)





#Rebounds model


rebounds_model <- model <- ranger(
  formula         = rebounds ~ conference_name + ranking + pg + sg + sf + pf + c + inches,
  data            = per_game_df,
  num.trees       = 500,
  mtry            = 5,
  min.node.size   = 4,
  sample.fraction = 0.7,
  seed            = 123,
  respect.unordered.factors = TRUE
)
# 
rebounds_preds <- predict(rebounds_model, test_data)

RMSE(rebounds_preds$predictions, test_data$rebounds)
#Save model
saveRDS(rebounds_model, "rebounds_rf_mod.RDS")

#Assists predictions
assist_index <- createDataPartition(combined$assists, p = 0.7, list = FALSE)
train_data <- combined[assist_index, ]
test_data  <- combined[-assist_index, ]
train_data$assists <- as.numeric(train_data$assists)
test_data$assists <- as.numeric(test_data$assists)

per_game_df$assists <- as.numeric(per_game_df$assists)

train_data$assists <- as.numeric(train_data$assists)

assist_rf_model <- model <- ranger(
  formula         = assists ~ conference_name + ranking + pg + sg + sf + pf + c + inches,
  data            = per_game_df, 
  num.trees       = 500,
  mtry            = 5,
  min.node.size   = 5,
  sample.fraction = 0.7,
  seed            = 123,
  respect.unordered.factors = TRUE
)

saveRDS(assist_rf_model, "assist_rf_mod.RDS")

assist_rf_preds <- predict(assist_rf_model, test_data)

RMSE(assist_rf_preds$predictions, test_data$assists)

#Save model as rds files 
#save rds function
#Terrell Brown switched teams. Transfers dont always line up. 
#Some data for players is missing.