#get_preseason_priors.R
library(tidyverse)
library(RcppRoll)
library(cfbfastR)
library(lubridate)
library(nnet)
library(splines)
library(rstanarm)
library(randomForest)
library(glmnet)
library(broom.mixed)
library(googledrive)
library(googlesheets4)
source("r/googledrive_csv.R")
options(tibble.width = Inf)
options(mc.cores = parallel::detectCores())
current_year <- 2023
run_historical <- TRUE
historical_seasons <- 10

#drive_auth()
# options(gargle_oauth_email = "*@sabinanalytics.com")
drive_auth()
gs4_auth(token = drive_token())

cfb_teams <- cfbd_team_info(only_fbs = FALSE)

# Read in & Adjust 247 Team Talent ----------------------------------------


cfb_247_talent <- cfbd_team_talent() %>% mutate(talent = ifelse(talent == 0, NA_real_, talent)) %>% distinct()

#join on fbs/fcs schools to make sure no one is missing
cfb_247_talent <- cfb_teams %>% 
  filter(classification %in% c("fbs", "fcs")) %>% 
  pull(school) %>% 
  expand_grid(team = ., 
              season = unique(cfb_247_talent$year)) %>% 
  left_join(., 
            cfb_247_talent,
            by = c("season" = "year", "team" = "school")) %>% 
  left_join(cfb_teams %>% 
              dplyr::select(team = school, 
                            team_division = classification, 
                            team_conference = conference),
            by = c("team")) %>% 
  mutate(season_team = paste(season, team, sep = '_') ) %>% 
  arrange(team, season) %>% 
  group_by(season, team_conference) %>% 
  mutate(conf_average_talent = mean(talent, na.rm = TRUE)) %>% 
  group_by(team) %>% 
  mutate(team_average_talent = mean(talent, na.rm = TRUE),
         team_last_talent = lag(talent)) %>% 
  ungroup() %>% ### replace missing talent values with an average of the conference average, the teams last value and teams average
  mutate(non_missing_values = 0 + (!is.na(conf_average_talent)) + (!is.na(team_average_talent)) + (!is.na(team_last_talent)),
         missing_replacement = (replace_na(conf_average_talent, 0) + replace_na(team_average_talent,0) + replace_na(team_last_talent, 0)) / non_missing_values,
         talent = coalesce(talent, missing_replacement)
           )

## if still missing values, take the minimum for their division (fbs or fcs)
cfb_247_talent <- cfb_247_talent %>% 
  group_by(season, team_division) %>% 
  mutate(talent = coalesce(talent, min(talent, na.rm = TRUE))) %>% 
  ungroup()
  
#now add in the log talent vs fbs z-score

cfb_247_talent <- cfb_247_talent %>% 
  group_by(season) %>% 
  mutate(talent_vs_fbs_avg = talent - mean(talent[team_division == 'fbs'],na.rm = TRUE),
         talent_vs_fbs_avg_z = talent_vs_fbs_avg / sd(talent_vs_fbs_avg[team_division == 'fbs'], na.rm = TRUE),
         log_talent = log(talent),
         avg_fbs_log_talent = mean(log_talent[team_division == 'fbs'], na.rm = TRUE),
         log_talent_vs_fbs_avg = log_talent - avg_fbs_log_talent,
         log_talent_vs_fbs_z = log_talent_vs_fbs_avg/sd(log_talent_vs_fbs_avg)
         ) %>% 
  ungroup()


# Read in Roster & Position Mapping from Google Drive -----------------------------------------



## read in position mapping file (manually curated from roster_data object)
position_mapping_file <- googledrive::drive_get("College Football/Position Data/position_mapping")
position_mapping <- googlesheets4::read_sheet(position_mapping_file, sheet = "position_mapping")

### read in team roster information
roster_file <- googledrive::drive_get("College Football/Player Data/all_season_rosters")
roster_data <- googlesheets4::read_sheet(roster_file, sheet = "complete_rosters")


# 
# ## read saved off adjusted stats for each team/season
# team_game_dribble <- gs4_find("advanced_game_stats")
# team_game_summary <- sheet_read(team_game_dribble, sheet = "advanced_game_stats")
# 

# Read in Adjusted Stats (no prior) ---------------------------------------


team_effects_workbook_name <- "team_effects"
model_params_workbook_name <- "model_parameters"
team_effects_workbook_dribble <- googledrive::drive_ls(path = "College Football/Team Data/No Prior Season Adjusted Stats/") %>% filter(name == team_effects_workbook_name)
model_params_workbook_dribble <- googledrive::drive_ls(path = "College Football/Team Data/No Prior Season Adjusted Stats/") %>% filter(name == model_params_workbook_name)

team_effects_tbl_wide <- read_sheet(team_effects_workbook_dribble, sheet = team_effects_workbook_name)
fixed_effects_tbl <- read_sheet(model_params_workbook_dribble, sheet = model_params_workbook_name)

#very select few duplication rows here (maybe it is from a join on the 'load_drive_epa.R' script)
team_effects_tbl_wide <- team_effects_tbl_wide %>% mutate(season = as.numeric(season)) %>% distinct()


# Read in Returning Players & Usage ---------------------------------------



# temp <- cfbd_game_box_advanced(400547640)
#player usage/returning production
player_returning <- player_usage <- NULL
for(s in (current_year - historical_seasons + 1):current_year){
  player_usage <- cfbd_player_usage(year = s) %>% bind_rows(player_usage, .)
  
  player_returning <- cfbd_player_returning(year = s) %>% bind_rows(player_returning, .)
}


#will model epa_drive, drives_game, plays_drive
stats_to_model <- team_effects_tbl_wide$stat %>% unique

## aggregate each season's roster recruiting and transfer data per team (by position)
# roster_data %>% count(position)

#look at distribution of missing players vs stars to have a good idea of what rating/star a player unevaluated is
roster_data <- roster_data %>% #teams data is current, doesn't reflect past conference affiliation
  left_join(cfb_teams %>%
              filter(!is.na(classification)) %>%
              dplyr::select(team = school, classification, conference),
            by = c("team"))


# Overall Recruiting ------------------------------------------------------


# recruiting_summary <- roster_data  %>% 
#   group_by(season, team, classification, conference) %>% 
#   summarize(pct_missing_star = mean(is.na(stars)),
#             avg_rating = mean(rating, na.rm = TRUE),
#             avg_stars = mean(stars, na.rm = TRUE),
#             n_players = n()
#             ) %>% 
#   ungroup()
# 
# recruiting_summary %>% 
#   filter(season >= 2014) %>% 
#   filter(classification == 'fbs') %>% 
#   ggplot(aes(x = avg_stars, y = pct_missing_star)) +#avg_rating
#   geom_point(aes(size = n_players, col = conference)) + 
#   geom_smooth(aes(weight = n_players)) + 
#   theme_bw()
# recruiting_summary %>% 
#   filter(season >= 2014) %>% 
#   filter(classification == 'fbs') %>% 
#   ggplot(aes(x = avg_rating, y = avg_stars)) +#avg_rating
#   geom_point(aes(size = n_players, col = conference)) + 
#   geom_smooth(aes(weight = n_players)) + 
#   theme_bw()

## prior for missing recruiting ranking will be 0.811 rating and 2.66 stars, which is about the average G-5 recruit (see below)
missing_recruit_replacement_level <- roster_data %>% 
  filter(season >= (current_year - 5)) %>% 
  filter(classification == 'fbs') %>%
  filter(!conference %in% c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC"),
         team != "Notre Dame") %>% 
  group_by(season, team, classification, conference) %>% 
  summarize(pct_missing_star = mean(is.na(stars)),
            avg_rating = mean(rating, na.rm = TRUE),
            avg_stars = mean(stars, na.rm = TRUE),
            n_players = n()
  ) %>% 
  ungroup() %>% 
  summarize(pct_missing_star = weighted.mean(pct_missing_star, w = n_players, na.rm = TRUE),
            avg_rating = weighted.mean(avg_rating, w = n_players, na.rm = TRUE),
            avg_stars = weighted.mean(avg_stars, w = n_players, na.rm = TRUE),
            n_players = mean(n_players)
  )

#replace positions with transfer positions (if available)
roster_data <- roster_data %>% 
  mutate(position = ifelse(!is.na(transfer_position) & season >= transfer_season, transfer_position, position) )

##map all roster positions
roster_data <- roster_data %>% 
  mutate(position = tolower(position)) %>% 
  left_join(position_mapping %>% 
              mutate(position = tolower(position)) %>% 
              dplyr::select(position,
                            off_def_st,
                            unit,
                            position_mapped),
            by = c("position")
  )
## if position is missing still, they are assumed to be a special teamer
roster_data <- roster_data %>% 
  mutate_at(vars(off_def_st, unit, position_mapped), ~ coalesce(., 'st'))

## fill in missing ratings with replacement level
roster_data <- roster_data %>% 
  mutate(rating = coalesce(rating, missing_recruit_replacement_level$avg_rating),
         stars = coalesce(stars, missing_recruit_replacement_level$avg_stars),
  )


## replace recruiting info with transfer info (if available) & add a manual account for which season it is for a player
roster_data <- roster_data %>% 
  mutate(rating = ifelse(!is.na(transfer_rating) & season >= transfer_season, transfer_rating, rating),
         stars = ifelse(!is.na(transfer_stars) & season >= transfer_season, transfer_stars, stars)) %>% 
  group_by(athlete_id) %>% 
  mutate(athlete_seasons = 1:n(),
         athlete_seasons = ifelse(is.na(athlete_id), 1, athlete_seasons) ) %>% 
  ungroup()

## calculate overall team recruiting/transfer ratings as well as avg by unit and weak-link of unit
team_unit_recruiting_summary <- roster_data %>% 
  group_by(season, team, classification, conference, off_def_st, unit) %>% 
  mutate(rating_rank = row_number(desc(rating)),
         rating_rank_adj = ifelse(rating_rank > 5, 6, rating_rank) ) %>% 
  group_by(season, team, classification, conference, off_def_st, unit, rating_rank_adj) %>% 
  summarize(avg_stars = mean(stars),
            avg_rating = mean(rating),
            n_players = n(),
            avg_experience = mean(athlete_seasons)
  ) %>% 
  group_by(season, team, classification, conference, off_def_st, unit) %>% 
  mutate(avg_unit_rating = weighted.mean(avg_rating, w = n_players),
         avg_unit_stars = weighted.mean(avg_stars, w = n_players),
         avg_unit_experience = weighted.mean(avg_experience, w = n_players)
  ) %>% 
  group_by(season, team, classification, conference, off_def_st) %>% 
  mutate(avg_off_def_st_rating = weighted.mean(avg_rating, w = n_players),
         avg_off_def_st_stars = weighted.mean(avg_stars, w = n_players),
         avg_off_def_st_experience = weighted.mean(avg_experience, w = n_players) 
  ) %>% 
  ungroup()
#look at weak link of recruiting 


team_unit_recruiting_summary_wide <- team_unit_recruiting_summary %>% 
  filter((unit == "db" & rating_rank_adj == 4) | 
           (unit == "dl" & rating_rank_adj == 4) | 
           (unit == "lb" & rating_rank_adj == 3) | 
           (unit == "ol" & rating_rank_adj == 5) | 
           (unit == "qb" & rating_rank_adj == 1) | 
           (unit == "rb" & rating_rank_adj == 2) | 
           (unit == "sp" & rating_rank_adj == 1) | 
           (unit == "st" & rating_rank_adj == 1) | 
           (unit == "wr" & rating_rank_adj == 3)
  ) %>% 
  dplyr::select(-n_players,
                -rating_rank_adj,
                -off_def_st) %>% 
  pivot_wider(values_from = c("avg_stars", "avg_rating", "avg_experience",
                              "avg_unit_rating", "avg_unit_stars",
                              "avg_unit_experience", "avg_off_def_st_rating",
                              "avg_off_def_st_stars", "avg_off_def_st_experience"),
              names_from = "unit") %>% 
  rename_at(vars(starts_with("avg_stars_")), ~str_replace(.,"avg_", "wk_link_")) %>% 
  rename_at(vars(starts_with("avg_rating_")), ~str_replace(.,"avg_", "wk_link_")) %>% 
  rename_at(vars(starts_with("avg_experience_")), ~str_replace(.,"avg_", "wk_link_"))


#replace missing values to minimum (i.e. didn't have a recruit at that level)

# returning production ----------------------------------------------------
player_usage_adjusted <- player_usage %>% 
  left_join(team_effects_tbl_wide %>% 
              filter(stat%in% c("epa_drive", "success_rate")) %>% 
              dplyr::select(team,
                            season,
                            off_effect,
                            def_effect,
                            phase,
                            stat) %>% 
              pivot_wider(values_from = c("def_effect", "off_effect"),
                          names_from = c("stat", "phase")),
            by = c("team", "season")
            ) %>% 
  arrange(season, athlete_id) %>% 
  mutate(usg_adj_epa_pass = usg_pass*off_effect_epa_drive_los,
         usg_adj_epa_rush = usg_rush*off_effect_epa_drive_los,
         usg_adj_epa_ovr = usg_overall*off_effect_epa_drive_los,
  )
  
### need to cycle through these and get a new one for each season being the "Max season"
roster_seasons <- roster_data %>% count(season) %>% filter(season >= current_year - historical_seasons + 1) %>% pull(season)
player_usage_adjusted_wmean <- player_usage_adjusted_max <- NULL
epa_usg_unit_wmean <- epa_usg_unit_max <- NULL

for(s in roster_seasons){
  player_usage_adjusted_max <- player_usage_adjusted %>% 
    filter(season <= s) %>% 
    group_by(athlete_id, name, position) %>% 
    mutate(season_max = max(season)) %>% 
    summarize_at(vars(usg_overall:last_col()), ~ max(., na.rm  = TRUE)) %>% 
    filter(season_max == s) %>% 
    ungroup() %>% 
    bind_rows(player_usage_adjusted_max, .)
  
  
  player_usage_adjusted_wmean <- player_usage_adjusted %>% 
    filter(season <= s) %>% 
    group_by(athlete_id, name, position) %>% 
    mutate(season_max = max(season)) %>% 
    summarize_at(vars(usg_overall:last_col()), ~ weighted.mean(., w = ((1:n())/2)^2, na.rm  = TRUE)) %>% 
    filter(season_max == s) %>% #get rid of values for players accounted for in previous seasons
    ungroup() %>% 
    bind_rows(player_usage_adjusted_wmean, .)
  
  #take current roster and apply both methods to a new returning production number
  #go through each season and take most recent value for each player on roster, then aggregate to roster x unit level
  
  epa_usg_unit_wmean <- roster_data %>% 
    filter(season == s) %>% 
    inner_join(player_usage_adjusted_wmean %>% 
                 filter(season_max <= s) %>% 
                 group_by(athlete_id) %>% 
                 filter(season_max == max(season_max)) %>% 
                 dplyr::select(athlete_id,
                               starts_with("usg_adj_")) %>% 
                 rename_at(vars(starts_with("usg_adj_")), ~paste0(., "_wmean")),
               by = c("athlete_id")
    ) %>% 
    group_by(season, team, classification, off_def_st, unit) %>% 
    summarize_at(vars(starts_with("usg_adj_")), sum) %>% 
    ungroup() %>% 
    bind_rows(epa_usg_unit_wmean, .)
  
  epa_usg_unit_max <- roster_data %>% 
    filter(season == s) %>% 
    inner_join(player_usage_adjusted_max %>% 
                 filter(season_max <= s) %>% 
                 group_by(athlete_id) %>% 
                 filter(season_max == max(season_max)) %>% 
                 dplyr::select(athlete_id,
                               starts_with("usg_adj_")) %>% 
                 rename_at(vars(starts_with("usg_adj_")), ~paste0(., "_max")),
               by = c("athlete_id")
    ) %>% 
    group_by(season, team, classification, off_def_st, unit) %>% 
    summarize_at(vars(starts_with("usg_adj_")), sum) %>% 
    ungroup() %>% 
    bind_rows(epa_usg_unit_max, .)
  
  cat("Finished team epa x usage metrics by unit for season ", s, "\r")
}



# Previous Years Success --------------------------------------------------
returning_production_variables <- epa_usg_unit_wmean %>% ## maybe only show offense right now --> collect all of these into a prior model prediction
  filter(unit %in% c("qb", "rb", "wr")) %>% 
  pivot_wider(values_from = starts_with("usg_adj"), names_from = 'unit') %>% 
  mutate_at(vars(starts_with("usg_adj_")), ~coalesce(., 0)) %>% 
  left_join(., ## now add in the max value for each team's unit
            epa_usg_unit_max %>% ## maybe only show offense right now --> collect all of these into a prior model prediction
              filter(unit %in% c("qb", "rb", "wr")) %>% 
              pivot_wider(values_from = starts_with("usg_adj"), names_from = 'unit') %>% 
              mutate_at(vars(starts_with("usg_adj_")), ~coalesce(., 0)),
            by = c("team", "season", "classification", "off_def_st")
            ) %>% 
  left_join(player_returning %>% 
              dplyr::select(team, 
                            season,
                            total_ppa:last_col()
                            ),
            by = c("season", "team")
  ) %>% 
  # group_by(season) %>% 
  mutate_if(is.numeric, ~coalesce(., mean(., na.rm = TRUE))) %>% 
  ungroup()

#incorporate some recruiting by units -- fill in missing values in a smart way
team_unit_recruiting_summary_wide <- team_unit_recruiting_summary_wide %>% 
  filter(season >= 2013,
         !is.na(classification),
         tolower(classification) %in% c('fcs', 'fbs')) %>% 
  group_by(classification) %>%
  mutate_at(vars(wk_link_stars_db:last_col()), ~coalesce(., min(., na.rm = TRUE))) %>% 
  ungroup()

## do I want an overall offense/defense recruiting variable?
# or just have a "weak-link" variable for each unit, instead of all the big ones?

# Calculate Recent Adj EPA values & Principle Components of Advanced Stats ------------------------------
# team_unit_recruiting_summary_wide
# returning_production_variables

adjusted_team_epa_drive <- team_effects_tbl_wide %>% 
  filter(stat == "epa_play", phase == "los") %>% 
  dplyr::select(season,
                team,
                team_division,
                off_epa_play_effect = off_effect,
                def_epa_play_effect = def_effect,
                off_epa_play_effect_sd = off_effect_sd,
                def_epa_play_effect_sd = def_effect_sd,
  )


#can I create a PCA of each team's offense and defense each year? might be best to differentiate
for(phase_use in c("los", "st")){
  for(off_def_use in c("off", "def")){
    
    temp_team_effects_tbl_wide <- team_effects_tbl_wide %>% 
      filter(phase == phase_use) %>% 
      dplyr::select(stat,
                    season,
                    team,
                    team_division,
                    contains(off_def_use)
                    ) %>% 
      pivot_wider(values_from = contains("effect"),
                  names_from = c("stat")
      ) %>% 
      group_by(team_division) %>% 
      mutate_at(vars(contains("effect")), ~coalesce(., mean(., na.rm = TRUE))) %>% 
      ungroup() %>% 
      filter(complete.cases(.))
    pc <- prcomp(temp_team_effects_tbl_wide[,-c(1:3)],
                 center = FALSE,
                 scale. = TRUE)
    # attributes(pc)
    #get the loadings and then take the first (3) PC's? or that account for at least 80% of the variation
    pc_avg <- pc$x[,1:2]*pc$scale[1:2]
    temp_team_effects_tbl_wide[,paste("pc",off_def_use,phase_use, sep = "_")] <- unlist(pc_avg[,1])
    
    
    adjusted_team_epa_drive <- adjusted_team_epa_drive %>% 
      left_join(temp_team_effects_tbl_wide %>% 
                  dplyr::select(season,
                                team,
                                starts_with("pc_")),
                by = c("season", "team"))
  }
}
## add last seasons off/def effect as a variable as well as last 3 years
adjusted_team_epa_drive <- adjusted_team_epa_drive %>% 
  arrange(team, season) %>% 
  group_by(team) %>% 
  mutate(pc_off_los_last_ssn = lag(pc_off_los),
         pc_def_los_last_ssn = lag(pc_def_los),
         pc_off_st_last_ssn = lag(pc_off_st),
         pc_def_st_last_ssn = lag(pc_def_st),
         off_epa_effect_last_3_ssn = lag(off_epa_play_effect),
         def_epa_effect_last_3_ssn = lag(def_epa_play_effect),
         off_epa_effect_last_3_ssn = (lag(off_epa_play_effect) + lag(off_epa_play_effect, 2) + lag(off_epa_play_effect, 3, default = 0)) / 3,
         def_epa_effect_last_3_ssn = (lag(def_epa_play_effect) + lag(def_epa_play_effect, 2) + lag(def_epa_play_effect, 3, default = 0)) / 3,
  ) %>% 
  ungroup()
# rolling mean (it is inclusive of last value -- unwanted)
# roll_mean(c(1:10), n = 3, fill = NA, align = "right")

epa_drive_prior_data <- adjusted_team_epa_drive %>% 
  arrange(team, season) %>% 
  group_by(team) %>%
  dplyr::select(season,
                team,
                team_division,
                starts_with("pc_"),
                ends_with("last_3_ssn")
                ) %>% 
  ungroup() %>% 
  left_join(team_unit_recruiting_summary_wide %>% 
              dplyr::select(-classification, -conference), 
            by = c("season", "team")) %>% 
  left_join(returning_production_variables %>% 
              dplyr::select(-classification, -off_def_st), 
            by = c("season", "team")) %>% 
  mutate_at(vars(contains("_"), usage, -team_division), ~ifelse(. == -Inf, min(.[.>-Inf], na.rm = TRUE), .) ) %>%  #if there are -Inf then replace with next smallest value 
  mutate_at(vars(contains("_"), usage, -team_division), ~coalesce(., quantile(.[. >-Inf], 0.05, na.rm = TRUE)) ) #if there are -Inf then replace with next smallest value 

#construct data for upcoming season too
epa_drive_prior_data_predictive <- epa_drive_prior_data %>% 
  mutate(season = season + 1)

### join on the stats we are modeling then run prior models for them
# stats_to_model## need to fix the adjusted epa/play as well - blowout adjustments etc. 
# team_effects_tbl_wide


# Define Data Sets Used in Prior Models -----------------------------------


## run multiple models for each of several different variables and ensemble
#define possible data points for offense and defense models
modeling_data <- team_effects_tbl_wide %>% 
  dplyr::select(-contains("talent")) %>% 
  left_join(cfb_247_talent %>% 
              dplyr::select(season_team,
                            talent,
                            talent_vs_fbs_avg_z,
                            log_talent_vs_fbs_z),
            by = c("season_team")) %>%
  group_by(team) %>% #fill in 2014 where no teams have talent values
  mutate_at(vars(contains("talent")), ~ coalesce(., mean(., na.rm = TRUE)) ) %>% 
  ungroup() %>% #anyone else gets the minimum
  mutate_at(vars(contains("talent")), ~coalesce(., min(., na.rm = TRUE)) ) %>% 
  left_join(epa_drive_prior_data %>% 
              dplyr::select(-team_division,
                           -pc_off_los,#these include current season, so can't include
                           -pc_def_los,
                           -pc_off_st,
                           -pc_def_st), by = c("season", "team")) %>% 
  filter(team_division %in% c("fbs", "fcs"))

## calculate quantiles of off/def epa vs the last 3 seasons to know how to distribute talent
modeling_data <- modeling_data %>% 
  group_by(season) %>% 
  mutate(off_epa_last_3_percentile = min_rank(off_epa_effect_last_3_ssn) / n(),
         def_epa_last_3_percentile = min_rank(def_epa_effect_last_3_ssn) / n(),
         recent_off_over_def_percent = off_epa_last_3_percentile / (off_epa_last_3_percentile + def_epa_last_3_percentile)
  ) %>% 
  ungroup()
         

off_los_modeling_data <-  modeling_data %>% 
  filter(phase == 'los') %>% 
  dplyr::select(stat,
                phase,
                season,
                team,
                team_division,
                off_effect,
                pc_off_los_last_ssn,
                off_epa_effect_last_3_ssn,
                # recent_off_over_def_percent,
                talent_vs_fbs_avg_z,
                # log_talent_vs_fbs_z,
                #qb
                # wk_link_rating_qb,
                # avg_unit_experience_qb,
                usg_adj_epa_pass_wmean_qb,
                # usg_adj_epa_rush_wmean_qb,
                #rb
                # avg_unit_rating_rb,
                avg_unit_experience_rb,
                usg_adj_epa_rush_wmean_rb,
                #wr
                # wk_link_rating_wr,
                # avg_unit_rating_wr,
                # avg_unit_experience_wr,
                usg_adj_epa_pass_wmean_wr,
                #ol
                # wk_link_rating_ol,
                # avg_unit_rating_ol,
                avg_unit_experience_ol,
                # 
                # ends_with("_rb"),
                # ends_with("_wr"),
                # ends_with("_ol"),
                # total_passing_ppa,
                # total_receiving_ppa,
                # total_rushing_ppa,
                # percent_ppa,
                # percent_passing_ppa,
                # percent_receiving_ppa,
                # percent_rushing_ppa,
                # usage,
                # passing_usage,
                # receiving_usage,
                # rushing_usage
  ) %>% 
  filter(complete.cases(.))
def_los_modeling_data <-  modeling_data %>% 
  filter(phase == 'los') %>% 
  dplyr::select(stat,
                phase,
                season,
                team,
                team_division,
                def_effect,
                pc_def_los_last_ssn,
                def_epa_effect_last_3_ssn,
                recent_off_over_def_percent,
                talent_vs_fbs_avg_z,
                # log_talent_vs_fbs_z,
                #db
                # wk_link_rating_db,
                # avg_unit_rating_db,
                # avg_unit_experience_db,
                #lb
                # wk_link_rating_lb,
                # avg_unit_rating_lb,
                # avg_unit_experience_lb,
                #dl
                # wk_link_rating_dl,
                # avg_unit_rating_dl,
                # avg_unit_experience_dl,
                # ends_with("_db"),
                # ends_with("_lb"),
                # ends_with("_dl"),
                # 
                # usage,
                # percent_ppa
  ) %>% 
  filter(complete.cases(.))


off_st_modeling_data <-  modeling_data %>% 
  filter(phase == 'st') %>% 
  dplyr::select(stat,
                phase,
                season,
                team,
                team_division,
                off_effect,
                # pc_off_st_last_ssn,
                recent_off_over_def_percent,
                talent_vs_fbs_avg_z,
                # log_talent_vs_fbs_z,
                avg_unit_rating_sp,
                avg_unit_experience_sp,
                # usage,
                # percent_ppa
  ) %>% 
  filter(complete.cases(.))


def_st_modeling_data <-  modeling_data %>% 
  filter(phase == 'st') %>% 
  dplyr::select(stat,
                phase,
                season,
                team,
                team_division,
                def_effect,
                pc_def_st_last_ssn,
                recent_off_over_def_percent,
                talent_vs_fbs_avg_z,
                # log_talent_vs_fbs_z,
                # avg_unit_rating_sp,
                # avg_unit_experience_sp,
                # usage,
                # percent_ppa
  ) %>% 
  filter(complete.cases(.))

adjusted_modeling_stats <- modeling_data %>% count(stat, phase)

## just do a linear model for each  (not much better than lasso)
get_split_lm_model <- function(data_split, use_off_def){
  
  if(use_off_def == "off"){
    
    models <- map(data_split,
                   function(sub_data) {
                     lm(off_effect ~ (.),
                        data = sub_data %>% 
                          dplyr::select(-stat,
                                        -phase,
                                        -season,
                                        -team)
                        )
                   })
  }else if(use_off_def == "def"){
    
    models <- map(data_split,
                  function(sub_data){
                  lm(def_effect ~ (.),
                     data = sub_data %>% 
                       dplyr::select(-stat,
                                     -phase,
                                     -season,
                                     -team)
                  )
                  })
  }else{
    stop("Must enter either 'off' or 'def' for use_off_def")
  }
  
  return(models)
  
  
}
get_split_model_coef <- function(input_models){
  
  return_df <- map(input_models, tidy) %>%
    list_rbind(names_to = "stat")
  
  return(return_df)
}
get_split_model_pred <- function(data_split, input_models){
 
  
  split_tbl_predictions <- map2(input_models,
                        data_split,
                        function(models_input, new_data) {
                          new_data$prior_mean <- predict(models_input, newdata = new_data)
                          return(new_data)
                        }
  )
  
  return_df <-list_rbind(split_tbl_predictions, names_to = "stat")
  
  return(return_df)
}
#offense los model
off_los_modeling_data_split <- split(off_los_modeling_data, off_los_modeling_data$stat)
# off_los_modeling_lambda_split <- best_lambda_tbl %>% 
#   filter(off_def == "off", phase == "los") %>% 
#   dplyr::select(stat,lambda) %>% 
#   split(., .$stat)
  
#defense los model
def_los_modeling_data_split <- split(def_los_modeling_data, def_los_modeling_data$stat)
# def_los_modeling_lambda_split <- best_lambda_tbl %>% 
#   filter(off_def == "def", phase == "los") %>% 
#   dplyr::select(stat,lambda) %>% 
#   split(., .$stat)

#offense st model
off_st_modeling_data_split <- split(off_st_modeling_data, off_st_modeling_data$stat)
# off_st_modeling_lambda_split <- best_lambda_tbl %>% 
#   filter(off_def == "off", phase == "st") %>% 
#   dplyr::select(stat,lambda) %>% 
#   split(., .$stat)

#defense st model
def_st_modeling_data_split <- split(def_st_modeling_data, def_st_modeling_data$stat)
# def_st_modeling_lambda_split <- best_lambda_tbl %>% 
#   filter(off_def == "def", phase == "st") %>% 
#   dplyr::select(stat,lambda) %>% 
#   split(., .$stat)

## save off model parameters & prior predictions

off_los_model <- get_split_lm_model(off_los_modeling_data_split, use_off_def = 'off')
off_los_model_params <-  get_split_model_coef(off_los_model) %>% mutate(phase = 'los', off_def = 'off')
def_los_model <- get_split_lm_model(def_los_modeling_data_split, use_off_def = 'def')
def_los_model_params <- get_split_model_coef(def_los_model) %>% mutate(phase = 'los', off_def = 'def')
off_st_model <- get_split_lm_model(off_st_modeling_data_split, use_off_def = 'off') 
off_st_model_params <- get_split_model_coef(off_st_model) %>% mutate(phase = 'st', off_def = 'off')
def_st_model <- get_split_lm_model(def_st_modeling_data_split, use_off_def = 'def')
def_st_model_params <- get_split_model_coef(def_st_model) %>% mutate(phase = 'st', off_def = 'def')

#get prior predictions for each data point
off_los_priors <- get_split_model_pred(off_los_modeling_data_split, off_los_model)
def_los_priors <- get_split_model_pred(def_los_modeling_data_split, def_los_model)
off_st_priors <- get_split_model_pred(off_st_modeling_data_split, off_st_model)
def_st_priors <- get_split_model_pred(def_st_modeling_data_split, def_st_model)


#combine all model parameters into one data frame
all_model_params <- bind_rows(off_los_model_params,
                              def_los_model_params,
                              off_st_model_params,
                              def_st_model_params)

for(use_phase in c("los", "st")){
  
  for(use_side in c("off", "def")){
    
    #save off predictive metric
    #create all_season_rosters if it doesn't exist yet
    worksheet_name <- paste("priors", use_side, use_phase, sep = '_')
    pred_stat_dribble <- gs4_find(worksheet_name)
    if(nrow(pred_stat_dribble) == 0){
      gs4_create(worksheet_name)
      
      #get dribble
      pred_stat_dribble <- gs4_find(worksheet_name)
      #move it to correct folder
      googledrive::drive_mv(file = pred_stat_dribble,
                            path = "College Football/Team Data/Preseason Priors/",
                            overwrite = TRUE)
      
      #search for it again
      pred_stat_dribble <- gs4_find(worksheet_name)
      
    }
    
    tbl_to_write_name <- paste(use_side, use_phase, "priors", sep = '_')
    tbl_to_write <- get(tbl_to_write_name)
    sheet_write(tbl_to_write, ss = pred_stat_dribble, sheet = worksheet_name)
    
    cat("Finished Priors for ", tbl_to_write_name, "\n")
  }
  
}
#save off all the model parameters in one file

worksheet_name <- 'all_prior_model_params'

#just run the first time
# gs4_create(worksheet_name)
# all_params_dribble <- gs4_find(worksheet_name)
# #move it to correct folder
# googledrive::drive_mv(file = all_params_dribble,
#                       path = "College Football/Team Data/Preseason Priors/",
#                       overwrite = TRUE)
#search for it again
all_params_dribble <- drive_ls("College Football/Team Data/Preseason Priors/") %>% 
  filter(name == worksheet_name)

sheet_write(all_model_params, ss = all_params_dribble, sheet = worksheet_name)




  
