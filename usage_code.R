library(tidyverse)
library(dplyr)
library(chron)


get_player_data <- function(player_select, season_select){
  #Specify player and season to be selected
  player_var = player_select
  season_var = season_select
  
  ### Create Player data aggregated by season, half ###
  #Join player data with teams on team_id, game_id
  #joined_p <- joined[(joined$full_name == player_var) & (joined$season.x == season_var)
  #                   & (joined$played == "True"),]
  
  if (season_var == "2019"){
    newp <- players[(players$full_name == player_var) & (players$played == "True"),]
  }
  else {
    newp <- players_2020[(players_2020$full_name == player_var),]
  }
  
  ### Get Opposing Team's Relevant Data
  #Grab gameID's
  #all_ids = select_team$game_id
  #select_games = teams[teams$game_id %in% all_ids,]
  
  #Grab all game_id's and then get field goal attempts where game_id != team_id
  #opp_games = select_games[select_games$team_id!=select_team$team_id[1],]
  
  return(newp)
}

get_opp_games <- function(p_df,team_df){
  ### Get Opposing Team's Relevant Data
  #Grab gameID's
  season_var = p_df$season[1]
  select_team <- team_df[(team_df$team_id==p_df$team_id[1]) & (team_df$season == season_var),]
  
  all_ids = select_team$game_id
  select_games = teams[teams$game_id %in% all_ids,]
  
  #Grab all game_id's and then get field goal attempts where game_id != team_id
  opp_games = select_games[select_games$team_id!=select_team$team_id[1],]
  return(opp_games)
}

get_select_team <- function(p_df) {
  season_var = p_df$season[1]
  select_team <- teams[(teams$team_id==p_df$team_id[1]) & (teams$season == season_var),]
  return(select_team)
}

#Calculate minutes
get_team_minutes <- function(team_df,p_df){
  #Subset by team and grab team possessions overall mean per half and min
  season_var = p_df$season[1]
  select_team <- team_df[(team_df$team_id==p_df$team_id[1]) & (team_df$season == season_var),]
  #team_pos <- sum(select_team$possessions_est / 2)
  #avg_team_pos_half <- mean(select_team$possessions_est / 2)
  
  team_min = as.numeric(as.difftime(select_team$minutes,format="%H:%M:%S",units="mins"))
  return(team_min)
}

get_player_mins <- function(p_df){
  if(p_df$season[1] != 2020){
    player_min = as.numeric(as.difftime(p_df$total_minutes,format="%M:%S",units="mins"))
  } else {
    player_min = as.numeric(as.difftime(p_df$minutes,format="%M:%S",units="mins"))
  }
  return(player_min)
}

### GET USAGE DATA ###
get_usage <- function(p_df){
  #NUMERATOR OF USAGE
  player_possess <- 100*(sum(sum(p_df[,'assists']),sum(p_df[,'turnovers']),
                             sum(p_df[,'field_goals_att']),0.44*sum(p_df[,'free_throws_att'])))*sum(get_team_minutes(teams,p_df))
  
  team_id = p_df$team_id[1]
  season_select = p_df[1,"season"]
  
  #DENOMINATOR OF USAGE
  select_team <- teams[(teams$team_id==team_id) & (teams$season == season_select),]
  team_possess <- (sum(sum(select_team$field_goals_att),0.44*sum(select_team$free_throws_att),
                       sum(select_team$turnovers))) * 5 * sum(get_player_mins(p_df))
  
  usage_perc <- player_possess / team_possess
  return(usage_perc)
}