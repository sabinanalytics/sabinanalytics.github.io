# # You can install using the pacman package using the following code:
# if (!requireNamespace('pacman', quietly = TRUE)){
#   install.packages('pacman')
# }
# pacman::p_load_current_gh("sportsdataverse/recruitR")

library(tidyverse)
library(recruitR)
library(stringdist)#for string distance algorithms
library(lubridate)
library(googledrive)
library(googlesheets4)
options(tibble.width = Inf)
source("r/haversine_distance.R")
# # Google sheets authentification -----------------------------------------------
drive_auth()
gs4_auth(token = drive_token())

# options(gargle_oauth_email = "*@sabinanalytics.com")
# options(gargle_oauth_cache = ".secrets")
# drive_auth(cache = ".secrets", email = "paul@sabinanalytics.com")
# gs4_auth(token = drive_token())

# AL_OTs <- cfbd_recruiting_player(2020, recruit_type = 'HighSchool', state='AL', position ='OT')
current_year <- 2023
run_historical <- TRUE
historical_seasons <- 15


all_recruits <- all_transfers <- all_rosters <- NULL

for(s in (current_year  - historical_seasons + 1):current_year){
  
  all_recruits_highschool <- cfbd_recruiting_player(s, recruit_type = 'HighSchool')
  all_recruits_juco <- cfbd_recruiting_player(s, recruit_type = 'JUCO')
  all_recruits_prepschool <- cfbd_recruiting_player(s, recruit_type = 'PrepSchool')

  all_recruits <- bind_rows(all_recruits_highschool,
                            all_recruits_juco,
                            all_recruits_prepschool) %>%
    bind_rows(all_recruits, .)

  rm(list = c("all_recruits_highschool", "all_recruits_juco", "all_recruits_prepschool"))

  all_transfers <- cfbd_recruiting_transfer_portal(s) %>%
    as_tibble() %>% 
    bind_rows(all_transfers, .)
  
  all_rosters <- cfbd_team_roster(year = s) %>% 
    unnest(recruit_ids, keep_empty = TRUE) %>% 
    mutate(season = s) %>% 
    bind_rows(all_rosters, .)
  
  cat("finished recruiting and rosters for ", s, " \n")
}


## try match all current rosters with recruiting information

##adjust variables that will help record linkage so names match etc.
all_rosters <- all_rosters %>% 
  mutate_at(vars(first_name, last_name, team, home_city, home_state, home_country), str_trim) %>% 
  mutate(home_lat_lon = paste0(home_latitude, ", ", home_longitude)) %>% 
  mutate_at(vars(home_latitude, home_longitude), as.numeric) %>% 
  mutate(full_name = paste(first_name, last_name, sep = " ") ) %>% 
  rename(ncaa_year = year)


all_recruits <- all_recruits %>% 
  mutate_at(vars(recruit_type, name, school, committed_to, position, city, state_province, country), str_trim) %>% 
  mutate(home_lat_lon = paste0(hometown_info_latitude, ", ", hometown_info_longitude)) %>% 
  mutate_at(vars(hometown_info_latitude, hometown_info_longitude), as.numeric) %>% 
  mutate(recruit_ids = as.integer(id)) %>% 
  rename(full_name = name,
         last_school = school,
         team = committed_to,
         home_city = city,
         home_state = state_province, 
         home_country = country,
         home_latitude = hometown_info_latitude,
         home_longitude = hometown_info_longitude,
         home_county_fips = hometown_info_fips_code,
         class_year =  year
         )

## for recruits & rosters, group by the recruit athlete_id and fill in missing data
all_recruits <- all_recruits %>% 
  group_by(athlete_id) %>% 
  fill(team, .direction = 'updown') %>% 
  fill(weight, .direction = 'updown') %>% 
  fill(height, .direction = 'updown') %>% 
  fill(position, .direction = 'updown') %>% 
  fill(home_city, .direction = 'updown') %>% 
  fill(home_state, .direction = 'updown') %>% 
  fill(home_country, .direction = 'updown') %>% 
  fill(home_country, .direction = 'updown') %>% 
  fill(home_latitude, .direction = 'updown') %>% 
  fill(home_longitude, .direction = 'updown') %>% 
  fill(home_county_fips, .direction = 'updown') %>% 
  fill(recruit_ids, .direction = 'updown') %>% 
  ungroup()

all_rosters <- all_rosters %>% 
  group_by(athlete_id) %>% 
  fill(team, .direction = 'updown') %>% 
  fill(weight, .direction = 'updown') %>% 
  fill(height, .direction = 'updown') %>% 
  fill(position, .direction = 'updown') %>% 
  fill(home_city, .direction = 'updown') %>% 
  fill(home_state, .direction = 'updown') %>% 
  fill(home_country, .direction = 'updown') %>% 
  fill(home_country, .direction = 'updown') %>% 
  fill(home_latitude, .direction = 'updown') %>% 
  fill(home_longitude, .direction = 'updown') %>% 
  fill(home_county_fips, .direction = 'updown') %>% 
  fill(recruit_ids, .direction = 'updown') %>% 
  ungroup()

#give them full names
all_transfers <- all_transfers %>% mutate(full_name = paste(first_name, last_name, sep = " "))

# all_rosters## full_name, weight, height, home_city, home_state, home_latitude, home_longitude
# all_recruits## full_name, position, height, weight, city, state_provience, country, hometown_info_latitude, hometown_info_longitude

#give a row id for each dataset
all_rosters <- all_rosters %>% mutate(row_id = dense_rank(athlete_id)) %>% arrange(row_id)
all_recruits <- all_recruits %>% mutate(row_id = dense_rank(athlete_id)) %>% arrange(row_id)


#caclulate mean and sd for some standard variables
height_mean <- mean(all_rosters$height, na.rm = TRUE)
height_sd <- sd(all_rosters$height, na.rm = TRUE)
weight_mean <- mean(all_rosters$weight, na.rm = TRUE)
weight_sd <- sd(all_rosters$weight, na.rm = TRUE)

recruit_roster_matches <- all_rosters %>% 
  left_join(all_recruits %>% 
               dplyr::select(recruit_ids,
                             class_year,
                             ranking,
                             stars,
                             rating,
                             recruit_row_id = row_id,
                             full_name_2 = full_name,
                             team_2 = team,
                             height_2 = height,
                             weight_2 = weight,
                             position_2 = position,
                             athlete_id_2 = athlete_id,
                             home_city_2 = home_city,
                             home_latitude_2 = home_latitude,
                             home_longitude_2 = home_longitude,
                             home_county_fips_2 = home_county_fips,
                             home_state_2 = home_state,
                             home_country_2 = home_country,
                             home_lat_lon_2 = home_lat_lon
                             ),
             by = c("recruit_ids")) %>% 
  mutate(full_name_dist = stringdist::stringdist(a = full_name, b = full_name_2, method = "jw"),
         team_dist = stringdist::stringdist(a = team, b = team_2, method = "jw"),
         height_dist = abs(((height - height_mean)/height_sd) - ((height_2 - height_mean)/height_sd) ),
         weight_dist = abs(((weight - weight_mean)/weight_sd) - ((weight_2 - weight_mean)/weight_sd) ),
         position_dist = stringdist::stringdist(a = position, b = position_2, method = "jw"),
         athlete_id_dist = athlete_id != athlete_id_2,
         home_city_dist = stringdist::stringdist(a = home_city, b = home_city_2, method = "jw"),
         home_dist = haversine_distance(home_latitude, home_longitude, home_latitude_2, home_longitude_2),
         home_state_dist = home_state != home_state_2
  )

roster_no_recruit <- recruit_roster_matches %>% filter(is.na(full_name_2))

recruit_no_roster <- all_recruits %>% 
  anti_join(all_rosters,
            by = c("recruit_ids"))

moments_df <- function(x){
  tibble(val = c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))#,
         # stat = c("mean", "sd")
         )
}

exact_matches_distance_summary <- recruit_roster_matches %>% 
  # dplyr::select(ends_with("_dist")) %>%
  reframe(
    across(ends_with("_dist"), moments_df, .unpack = TRUE)
  ) %>% 
  mutate(stat = c("mean", "sd")) %>% 
  pivot_longer(cols = ends_with("_val"),
               names_to = "col_name",
               values_to = "value") %>% 
  mutate(col_name = str_remove(col_name, "_val"))

full_name_dist_match_mean <- exact_matches_distance_summary %>% filter(col_name == "full_name_dist", stat == "mean") %>% pull
full_name_dist_match_sd <- exact_matches_distance_summary %>% filter(col_name == "full_name_dist", stat == "sd") %>% pull
team_name_dist_match_mean <- exact_matches_distance_summary %>% filter(col_name == "team_dist", stat == "mean") %>% pull
team_name_dist_match_sd <- exact_matches_distance_summary %>% filter(col_name == "team_dist", stat == "sd") %>% pull
height_dist_match_mean <- exact_matches_distance_summary %>% filter(col_name == "height_dist", stat == "mean") %>% pull
height_dist_match_sd <- exact_matches_distance_summary %>% filter(col_name == "height_dist", stat == "sd") %>% pull
weight_dist_match_mean <- exact_matches_distance_summary %>% filter(col_name == "weight_dist", stat == "mean") %>% pull
weight_dist_match_sd <- exact_matches_distance_summary %>% filter(col_name == "weight_dist", stat == "sd") %>% pull
position_dist_match_mean <- exact_matches_distance_summary %>% filter(col_name == "position_dist", stat == "mean") %>% pull
position_dist_match_sd <- exact_matches_distance_summary %>% filter(col_name == "position_dist", stat == "sd") %>% pull
home_dist_match_mean <- exact_matches_distance_summary %>% filter(col_name == "home_dist", stat == "mean") %>% pull
home_dist_match_sd <- exact_matches_distance_summary %>% filter(col_name == "home_dist", stat == "sd") %>% pull



##go through one by one the ones that aren't matched and find best one
distinct_no_recruit <- roster_no_recruit %>% 
  distinct(full_name, row_id) %>% 
  mutate(athlete_id = NA_character_,
         recruit_ids = NA_real_,
         class_year = NA_real_,
         ranking = NA_real_,
         stars = NA_real_,
         rating = NA_real_,
         recruit_row_id = NA_real_)

for(i in 1:nrow(distinct_no_recruit)){
  
  roster_temp <- roster_no_recruit %>% 
    filter(row_id == distinct_no_recruit$row_id[i])
  
  possible_recruit_matches <- recruit_no_roster %>% #don't show recruits who are already matched based on ID
    mutate(full_name_dist = stringdist::stringdist(a = full_name, b = distinct_no_recruit$full_name[i], method = "jw")) %>% 
    # find all names within 4 std deviations of what the true matches name distances are and only go through those
    filter(full_name_dist <= full_name_dist_match_mean + 3*full_name_dist_match_sd) %>% 
    filter(class_year <= min(roster_temp$season, na.rm = TRUE))
  
  # a couple other minimum filters
  
  possible_recruit_matches_2 <- roster_temp %>% 
    arrange(desc(season)) %>%
    slice(1) %>% 
    dplyr::select(athlete_id:row_id) %>% 
    dplyr::select(-recruit_ids) %>% 
    bind_cols(., 
              possible_recruit_matches %>% 
                dplyr::select(recruit_ids,
                              class_year,
                              ranking,
                              stars,
                              rating,
                              recruit_row_id = row_id,
                              full_name_2 = full_name,
                              team_2 = team,
                              height_2 = height,
                              weight_2 = weight,
                              position_2 = position,
                              athlete_id_2 = athlete_id,
                              home_city_2 = home_city,
                              home_latitude_2 = home_latitude,
                              home_longitude_2 = home_longitude,
                              home_county_fips_2 = home_county_fips,
                              home_state_2 = home_state,
                              home_country_2 = home_country,
                              home_lat_lon_2 = home_lat_lon
                )
              ) %>% 
    mutate(full_name_dist = stringdist::stringdist(a = full_name, b = full_name_2, method = "jw"),
           team_dist = stringdist::stringdist(a = team, b = team_2, method = "jw"),
           height_dist = abs(((height - height_mean)/height_sd) - ((height_2 - height_mean)/height_sd) ),
           weight_dist = abs(((weight - weight_mean)/weight_sd) - ((weight_2 - weight_mean)/weight_sd) ),
           position_dist = stringdist::stringdist(a = position, b = position_2, method = "jw"),
           athlete_id_dist = athlete_id != athlete_id_2,
           home_city_dist = stringdist::stringdist(a = home_city, b = home_city_2, method = "jw"),
           home_dist = haversine_distance(home_latitude, home_longitude, home_latitude_2, home_longitude_2),
           home_state_dist = home_state != home_state_2
    )
  #check to see if both distance from hometown and height and weight are within the thresholds of 3 sd
  possible_recruit_matches_2 <- possible_recruit_matches_2 %>% 
    filter(is.na(home_dist) | home_dist <= home_dist_match_mean + 3*home_dist_match_sd) %>% 
    filter(is.na(position_dist) | position_dist <= position_dist_match_mean + 3*position_dist_match_sd)  %>% 
    filter(is.na(weight_dist) | weight_dist <= weight_dist_match_mean + 3*weight_dist_match_sd)  %>% 
    filter(is.na(height_dist) | height_dist <= height_dist_match_mean + 3*height_dist_match_sd) 
  
  if(nrow(possible_recruit_matches_2) > 0){
    selected_recruit_match <- possible_recruit_matches_2 %>% 
      arrange(athlete_id_dist,
              full_name_dist,
              home_dist,
              height_dist,
              weight_dist,
              position_dist,
              home_state_dist
              ) %>% 
      slice(1)
    
    ## fill in data from the selected recruit to the non-matching roster values
    distinct_no_recruit$athlete_id[i] <- selected_recruit_match$athlete_id_2
    distinct_no_recruit$recruit_ids[i] <- selected_recruit_match$recruit_ids
    distinct_no_recruit$class_year[i] <- selected_recruit_match$class_year
    distinct_no_recruit$ranking[i] <- selected_recruit_match$ranking
    distinct_no_recruit$stars[i] <- selected_recruit_match$stars
    distinct_no_recruit$rating[i] <- selected_recruit_match$rating
    distinct_no_recruit$recruit_row_id[i] <- selected_recruit_match$recruit_row_id

  }
  cat(" Finished matching number ", i, " of ", nrow(distinct_no_recruit), ' \r')
}

## add new recruiting data to roster_no_recruit
filled_in_missing_recruits <- roster_no_recruit %>% 
  dplyr::select(-full_name_2,
                -athlete_id,
                -recruit_ids,
                -class_year,
                -ranking,
                -stars,
                -rating,
                -recruit_row_id) %>% 
  left_join(distinct_no_recruit %>% 
              rename(full_name_2 = full_name), by = c("row_id")) %>% 
  mutate(full_name = coalesce(full_name, full_name_2)) %>% 
  dplyr::select(-full_name_2)


filled_in_rosters <- recruit_roster_matches %>% 
  filter(!is.na(full_name_2)) %>% 
  bind_rows(., filled_in_missing_recruits) %>% 
  arrange(row_id, season)


### add in transfers to filled in rosters
# filled_in_rosters %>% filter(full_name == "Caleb Williams", athlete_id == 4431611)

#someone who transfered twice, go through each situation to see if handled correctly
which(all_transfers$full_name == "Jack Abraham")

matched_transfers <- NULL
for(i in 1:nrow(all_transfers)){
  
  transfer_temp <- all_transfers %>% slice(i)
  
  possible_transfer_matches <- filled_in_rosters %>% #don't show recruits who are already matched based on ID
    mutate(full_name_dist = stringdist::stringdist(a = full_name, b = transfer_temp$full_name, method = "jw")) %>% 
    # find all names within 4 std deviations of what the true matches name distances are and only go through those
    filter(full_name_dist <= full_name_dist_match_mean + 3*full_name_dist_match_sd) %>% 
    filter(season >= transfer_temp$season)
  
  # a couple other minimum filters
  
  possible_transfer_matches_2 <- possible_transfer_matches %>% 
    arrange(desc(season)) %>%
    # slice(1) %>% 
    dplyr::select(athlete_id:team_2) %>% 
    # dplyr::select(-recruit_ids) %>% 
    bind_cols(., 
              transfer_temp %>% 
                dplyr::select(transfer_season = season,
                              transfer_full_name = full_name,
                              transfer_destination = destination,
                              transfer_origin = origin,
                              transfer_position = position,
                              transfer_eligibility = eligibility,
                              transfer_stars = stars,
                              transfer_rating = rating,
                              transfer_date
                )
    ) %>% 
    mutate(full_name_dist = stringdist::stringdist(a = full_name, b = transfer_full_name, method = "jw"),
           team_dist = stringdist::stringdist(a = team, b = transfer_destination, method = "jw"),
           team_2_dist = stringdist::stringdist(a = team_2, b = transfer_origin, method = "jw"),
           position_dist = stringdist::stringdist(a = position, b = transfer_position, method = "jw")
           )
  #check to see if both distance from hometown and height and weight are within the thresholds of 3 sd
  possible_transfer_matches_2 <- possible_transfer_matches_2 %>% 
    filter(is.na(full_name_dist) | full_name_dist <= full_name_dist_match_mean + 3*full_name_dist_match_sd) %>% 
    filter(is.na(team_dist) | team_dist <= team_name_dist_match_mean + 3*team_name_dist_match_sd)  %>% 
    # filter(is.na(team_2_dist) | team_2_dist <= team_name_dist_match_mean + 3*team_name_dist_match_sd)  %>% 
    filter(is.na(position_dist) | position_dist <= position_dist_match_mean + 3*position_dist_match_sd) 
  
  if(nrow(possible_transfer_matches_2) > 0){
    selected_transfer_matches <- possible_transfer_matches_2 %>% 
      arrange(full_name_dist, team_dist, position_dist, team_2_dist) 
    ## only take rows where the player equals the most likely player
    selected_transfer_matches <- selected_transfer_matches %>% 
      filter(athlete_id == selected_transfer_matches$athlete_id[1]) %>% 
      dplyr::select(row_id,
                    season,
                    starts_with("transfer_")
                    ) %>%
      distinct()
    
    matched_transfers <- matched_transfers %>% bind_rows(., selected_transfer_matches)
  }
  cat(" Finished transfer number ", i, " of ", nrow(all_transfers), ' \r')
}


matched_transfers <- matched_transfers %>% 
  mutate(transfer_rating = as.numeric(transfer_rating))

#getting rid of duplicate player x season transfers caused by either multiple transfers by same player or other oddities
matched_transfers <- matched_transfers %>% 
  group_by(row_id, season) %>% 
  # filter(n() > 1) %>%
  # arrange(row_id, season) %>% 
  # ungroup() %>% 
  arrange(row_id, season, desc(transfer_stars), desc(transfer_rating), desc(transfer_date)) %>% 
  slice(1) %>% 
  ungroup()

## add matched transfers to the rosters data
filled_in_rosters <- filled_in_rosters %>% 
  left_join(matched_transfers, 
            by = c("season", "row_id"))
#pair down what columns we will be writing
filled_in_rosters_write <- filled_in_rosters %>% 
  mutate(height = coalesce(height, height_2),
         weight = coalesce(weight, weight_2),
         position = coalesce(position, position_2),
         home_city = coalesce(home_city, home_city_2),
         home_state = coalesce(home_state, home_state_2),
         home_country = coalesce(home_country, home_country_2),
         home_latitude = coalesce(home_latitude, home_latitude_2),
         home_longitude = coalesce(home_longitude, home_longitude_2),
         home_country = coalesce(home_country, home_country_2) 
         ) %>% 
  dplyr::select(season,
                athlete_id:headshot_url, 
                class_year:rating,
                starts_with("transfer")
                )


## save to my google drive
# googledrive_csv(df = filled_in_rosters, 
#                 path_name = "College Football/Player Data/all_season_rosters")
# gs4_create("College Football/Player Data/all_season_rosters")

#create all_season_rosters if it doesn't exist yet
all_season_roster_dribble <- gs4_find("all_season_rosters")
if(nrow(all_season_roster_dribble) == 0){
  gs4_create("all_season_rosters")
  
  #get dribble
  all_season_roster_dribble <- gs4_find("all_season_rosters")
  #move it to correct folder
  googledrive::drive_mv(file = all_season_roster_dribble,
                        path = "College Football/Player Data/",
                        overwrite = TRUE)
  
  #search for it again
  all_season_roster_dribble <- gs4_find("all_season_rosters")
  
}

sheet_write(filled_in_rosters_write, ss = all_season_roster_dribble, sheet = "complete_rosters")


# googledrive::drive_find("College Football/Player Data")
