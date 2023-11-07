library(tidyverse)
library(baseballr)
library(glue)
library(grid)
library(gridExtra)
library(teamcolors)

get_game_image <- function(game_id){
  #### RETREIVE INFORMATION FROM MLB.COM ####
  test_game <- mlb_pbp(game_id)
  test_info <- mlb_game_info(game_id)
  test_content <- mlb_game_linescore(game_id)
  test_wp <- mlb_game_wp(game_id)
  test_orders <- mlb_batting_orders(game_id, type = "all")
  
  #### CREATE A BOX SCORE ####
  box_score <- data.frame(num = test_content$num, 
                          away_runs = test_content$away_runs, 
                          home_runs = test_content$home_runs) %>%
    gather(key = "variable", value = "value", -num) %>%
    spread(key = num, value = value) %>%
    column_to_rownames(var = "variable") %>% replace(is.na(.), "X") %>%
    mutate(R = c(sum(ifelse(is.na(test_content$away_runs), 0, test_content$away_runs))
                 ,sum(ifelse(is.na(test_content$home_runs), 0, test_content$home_runs))),
           H = c(sum(ifelse(is.na(test_content$away_hits), 0, test_content$away_hits))
                 ,sum(ifelse(is.na(test_content$home_hits), 0, test_content$home_hits))),
           E = c(sum(ifelse(is.na(test_content$away_errors), 0, test_content$away_errors))
                 ,sum(ifelse(is.na(test_content$home_errors), 0, test_content$home_errors)))) %>%
    `rownames<-`(c(unique(test_content$away_team_abbreviation), unique(test_content$home_team_abbreviation)))
  
  #### CREATE THE AWAY AND HOME BATTING BOX SCORES ####
  #### AWAY BOX SCORE ####
  away_ids <- test_orders %>% 
    filter(team == "away") %>% 
    select(id, fullName) %>% 
    rename(player = fullName)
  
  away_stats_list <- list()
  for (away_id in away_ids$id) {
    away_stats <- mlb_player_game_stats(away_id, game_id) %>% 
      filter(group == "hitting") %>%
      select(AB = 'at_bats',
             R = 'runs',
             H = 'hits',
             RBI = 'rbi',
             BB = 'base_on_balls',
             SO = 'strike_outs',
             SB = 'stolen_bases',
             LOB = 'left_on_base') %>%
      mutate(id = away_id)
    away_stats_list[[as.character(away_id)]] <- away_stats
  }
  
  away_stats_df <- bind_rows(away_stats_list)
  
  away_full_box <- left_join(away_stats_df, away_ids, by = "id") %>%
    select(player, AB, R, H, RBI, BB, SO, SB, LOB)
  
  #### AWAY PITCHERS ####
  away_pitching <- test_game %>%
    filter(about.isTopInning == FALSE) %>%
    arrange(atBatIndex) %>%
    select(id = matchup.pitcher.id,
           player = matchup.pitcher.fullName) %>% 
    distinct()
  
  away_pitching_stats_list <- list()
  for (pitcher in away_pitching$id) {
    away_pitching_stats <- mlb_player_game_stats(pitcher, game_id) %>% 
      filter(group == "pitching") %>%
      select(IP = 'innings_pitched',
             H = 'hits',
             R = 'runs',
             ER = 'earned_runs',
             BB = 'base_on_balls',
             SO = 'strike_outs',
             HR = 'home_runs') %>%
      mutate(id = pitcher)
    away_pitching_stats_list[[as.character(pitcher)]] <- away_pitching_stats
  }
  
  away_pitching_stats_df <- bind_rows(away_pitching_stats_list)
  
  away_full_pitching_box <- left_join(away_pitching_stats_df, 
                                      away_pitching, by = "id") %>%
    select(player, IP, H, R, ER, BB, SO, HR)
  
  #### HOME BOX SCORE ####
  home_ids <- test_orders %>% 
    filter(team == "home") %>% 
    select(id, fullName) %>% 
    rename(player = fullName)
  
  home_stats_list <- list()
  for (home_id in home_ids$id) {
    home_stats <- mlb_player_game_stats(home_id, game_id) %>% 
      filter(group == "hitting") %>%
      select(AB = 'at_bats',
             R = 'runs',
             H = 'hits',
             RBI = 'rbi',
             BB = 'base_on_balls',
             SO = 'strike_outs',
             SB = 'stolen_bases',
             LOB = 'left_on_base') %>%
      mutate(id = home_id)
    home_stats_list[[as.character(home_id)]] <- home_stats
  }
  
  home_stats_df <- bind_rows(home_stats_list)
  
  home_full_box <- left_join(home_stats_df, home_ids, by = "id") %>%
    select(player, AB, R, H, RBI, BB, SO, SB, LOB)
  
  #### HOME PITCHERS ####
  home_pitching <- test_game %>%
    filter(about.isTopInning == TRUE) %>%
    arrange(atBatIndex) %>%
    select(id = matchup.pitcher.id,
           player = matchup.pitcher.fullName) %>% 
    distinct()
  
  home_pitching_stats_list <- list()
  for (pitcher in home_pitching$id) {
    home_pitching_stats <- mlb_player_game_stats(pitcher, game_id) %>% 
      filter(group == "pitching") %>%
      select(IP = 'innings_pitched',
             H = 'hits',
             R = 'runs',
             ER = 'earned_runs',
             BB = 'base_on_balls',
             SO = 'strike_outs',
             HR = 'home_runs') %>%
      mutate(id = pitcher)
    home_pitching_stats_list[[as.character(pitcher)]] <- home_pitching_stats
  }
  
  home_pitching_stats_df <- bind_rows(home_pitching_stats_list)
  
  home_full_pitching_box <- left_join(home_pitching_stats_df, 
                                      home_pitching, by = "id") %>%
    select(player, IP, H, R, ER, BB, SO, HR)
  
  #### WIN PROBABILITY ####
  
  #### CREATE A WIN PROBABILITY TABLE ####
  wp_ref <- test_wp %>% 
    arrange(desc(abs(home_team_win_probability_added))) %>% 
    slice(1:3) %>% 
    mutate(wp = ifelse(home_team_win_probability_added >= 0, 
                       home_team_win_probability_added, 
                       -home_team_win_probability_added))
  
  
  test_game %>% 
    filter(test_game$about.atBatIndex == 
             as.numeric(order(-abs(test_wp$home_team_win_probability_added))[3])) %>%
    slice(1) %>%
    mutate(result = paste(result.awayScore, result.homeScore, sep = "-")) %>%
    pull(result)
  
  find_player_at_bat <- function(ab_index){
    (test_game %>% 
       mutate(atBatIndex = as.integer(atBatIndex)) %>%
       select(atBatIndex, matchup.batter.fullName) %>%
       unique() %>% 
       filter(atBatIndex == ab_index) %>% 
       pull(matchup.batter.fullName))
  }
  
  find_player_team <- function(player){
    test_game %>% filter(matchup.batter.fullName == player) %>%
      pull(batting_team) %>% unique()
  }
  
  
  find_player_at_bat <- function(ab_index){
    (test_game %>% 
       mutate(atBatIndex = as.integer(atBatIndex)) %>%
       select(atBatIndex, matchup.batter.fullName) %>%
       unique() %>% 
       filter(atBatIndex == ab_index) %>% 
       pull(matchup.batter.fullName))
  }
  
  find_player_pitching <- function(ab_index){
    (test_game %>% 
       mutate(atBatIndex = as.integer(atBatIndex)) %>%
       select(atBatIndex, matchup.pitcher.fullName) %>%
       unique() %>% 
       filter(atBatIndex == ab_index) %>% 
       pull(matchup.pitcher.fullName))
  }
  
  find_player_team <- function(player){
    test_game %>% filter(matchup.batter.fullName == player) %>%
      pull(batting_team) %>% unique()
  }
  
  find_pitcher_team <- function(player){
    test_game %>% filter(matchup.pitcher.fullName == player) %>%
      pull(fielding_team) %>% unique()
  }
  
  test_wp %>% 
    group_by(at_bat_index) %>%
    mutate(
      player_at_bat = (find_player_at_bat(at_bat_index)),
      player_pitching = find_player_pitching(at_bat_index)) %>%
    mutate(player_team = find_player_team(player_at_bat),
           pitcher_team = find_pitcher_team(player_pitching),
           away_team = unique(test_game$away_team),
           home_team = unique(test_game$home_team)) -> win_prob_ref
  
  
  batters_wp <- win_prob_ref %>% group_by(player_at_bat) %>%
    mutate(total_win_probability = sum(home_team_win_probability_added)) %>%
    mutate(win_prob_adjusted = if_else(player_team == home_team,
                                       total_win_probability,
                                       -total_win_probability)) %>%
    select(Player = player_at_bat, Team = player_team, WPA = win_prob_adjusted) %>%
    unique() %>%
    arrange(desc(WPA))
  
  pitchers_wp <- win_prob_ref %>% 
    group_by(player_pitching) %>%
    mutate(total_win_probability = sum(home_team_win_probability_added)) %>%
    mutate(win_prob_adjusted = if_else(pitcher_team == home_team,
                                       total_win_probability,
                                       -total_win_probability)) %>%
    select(Player = player_pitching, Team = pitcher_team, WPA = win_prob_adjusted) %>%
    unique() %>%
    arrange(desc(WPA))
  
  total_win_prob <- rbind(batters_wp, pitchers_wp) %>% 
    arrange(desc(WPA)) %>%
    filter(WPA >= .$WPA[10]) -> win_prob_table
  
  
  
  #### GET TEAM COLORS FOR FORMATTING ####
  mlb_colors <- read.csv("mlb_colors.csv") %>% 
    mutate(full_name = glue("{location} {team}"))
  
  away_primary <- test_game %>% 
    slice(1) %>% 
    pull(away_team) %>%
    {mlb_colors[mlb_colors$full_name == ., "primary_color"]} %>%
    as.character()
  
  home_primary <- test_game %>% 
    slice(1) %>% 
    pull(home_team) %>%
    {mlb_colors[mlb_colors$full_name == ., "primary_color"]} %>%
    as.character()
  
  away_secondary <- test_game %>% 
    slice(1) %>% 
    pull(away_team) %>%
    {mlb_colors[mlb_colors$full_name == ., "secondary_color"]} %>%
    as.character()
  
  home_secondary <- test_game %>% 
    slice(1) %>% 
    pull(home_team) %>%
    {mlb_colors[mlb_colors$full_name == ., "secondary_color"]} %>%
    as.character()
  #### CREATE THE PNG IMAGE ####
  
  width <- 1824
  height <- 1024
  
  #### CREATE PNG ####
  
  png(glue("{test_game$home_level_name} {test_info$game_date}.png"), width = width, height = height, bg = "white")
  
  plot(0, type = "n", xlim = c(0, width), ylim = c(0, height), xaxt = "n", yaxt = "n", bty = "n")
  
  text(x = 2300, y = 20, 
       labels = glue("Date: {test_info$game_date}
                 Venue: {test_info$venue_name}
                 Temperature: {test_info$temperature}°
                 Attendance: {test_info$attendance}
                 Time of Game: {test_info$elapsed_time}"), 
       pos = 4, offset = -50, cex = 2, 
       col = "black", font = 2)
  
  text(x = 912, y = 1000, labels = glue("{test_content$away_team_name} ({unique(test_content$away_team_record_league_record_wins)}-{unique(test_content$away_team_record_league_record_losses)}) vs {test_content$home_team_name} ({unique(test_content$home_team_record_league_record_wins)}-{unique(test_content$home_team_record_league_record_losses)})"),
       cex = 5, col = "black", font = 2)
  
  text(x = 285, y = 925, labels = "Away Batting",
       cex = 3, col = "black", font = 2)
  
  text(x = 285, y = 450, labels = "Away Pitching",
       cex = 3, col = "black", font = 2)
  
  text(x = 1525, y = 925, labels = "Home Batting",
       cex = 3, col = "black", font = 2)
  
  text(x = 1525, y = 450, labels = "Home Pitching",
       cex = 3, col = "black", font = 2)
  
  text(x = 912, y = 500, labels = "WPA Leaders",
       cex = 3, col = "black", font = 2)
  
  text(x = 912, y = 750, labels = "Box Score",
       cex = 4, col = "black", font = 2)
  
  text(x = 1, y = 1, labels = "@WillCave
Version 1.3",
       cex = 2, col = "black", font = 2)
  
  vp <- viewport(x = 0.5, y = 0.6, width = 2, height = 2)
  pushViewport(vp)
  
  theme <- ttheme_default(core = list(fg_params = list(cex = 3)),
                          rowhead = list(fg_params = list(cex = 3)),
                          colhead = list(fg_params = list(cex = 3)))
  
  grid.table(box_score, theme = theme)
  upViewport()
  
  away_box_theme <- ttheme_default(
    core=list(bg_params=list(fill="#FFFFFF"), 
              fg_params=list(fontface="bold", cex = 1.5)),
    colhead=list(bg_params=list(fill=away_primary), 
                 fg_params=list(col=away_secondary, fontface="bold", cex = 1.5)),
    rowhead=list(fg_params=list(col=away_secondary, fontface="bold")),
    cell = list(border=unit(c(1,1), "pt"), 
                padding=unit(c(5,5), "pt"))
  )
  
  vp1 <- viewport(x = 0.2, y = 0.65, width = 2, height = 2)
  pushViewport(vp1)
  
  grid.table(away_full_box, theme=away_box_theme)
  
  upViewport()
  
  home_box_theme <- ttheme_default(
    core=list(bg_params=list(fill="#FFFFFF"), 
              fg_params=list(fontface="bold", cex = 1.5)),
    colhead=list(bg_params=list(fill=home_primary), 
                 fg_params=list(col=home_secondary, fontface="bold", cex = 1.5)),
    rowhead=list(fg_params=list(col=home_secondary, fontface="bold")),
    cell = list(border=unit(c(1,1), "pt"), 
                padding=unit(c(5,5), "pt"))
  )
  
  vp2 <- viewport(x = 0.8, y = 0.65, width = 2, height = 2)
  pushViewport(vp2)
  
  grid.table(home_full_box, theme = home_box_theme)
  
  upViewport()
  
  
  vp3 <- viewport(x = 0.2, y = 0.35, width = 2, height = 2)
  pushViewport(vp3)
  
  grid.table(away_full_pitching_box, theme = away_box_theme)
  
  upViewport() 
  
  vp4 <- viewport(x = 0.8, y = 0.35, width = 2, height = 2)
  pushViewport(vp4)
  
  grid.table(home_full_pitching_box, theme = home_box_theme)
  
  upViewport() 
  
  vp5 <- viewport(x = 0.5, y = 0.3, width = 2, height = 2)
  pushViewport(vp5)
  
  wptheme <- ttheme_default(core = list(fg_params = list(cex = 2)),
                            rowhead = list(fg_params = list(cex = 2)),
                            colhead = list(fg_params = list(cex = 2)))
  grid.table(win_prob_table, theme = wptheme)
  
  upViewport() 
  
  dev.off()
  
}

get_game_image("Your Game PK")