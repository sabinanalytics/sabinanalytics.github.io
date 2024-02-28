library(parallel)
library(tidyverse)
library(hoopR)
library(ggridges)
library(gt)
library(gtExtras)
library(viridis)
library(rstan)
library(tidybayes)
library(rstanarm)
library(aws.s3)
library(broom.mixed)
options(mc.cores = detectCores())
options(tibble.width = Inf)
bucket_name <- "s3://sagemaker-studio-m35e50pwfmm/"


start_season <- 2018
end_season <- 2023
pbp <- NULL
for(s in start_season:end_season){
  pbp <- load_mbb_pbp(s) |> 
    select(
      tidyselect::any_of(
        c("game_id",
        "game_date_time",
        "period_number",
        "game_half",
        "start_half_seconds_remaining",
        "team_id",
        "season",
        "away_team_id",
        "home_team_id",
        "away_team_name",
        "home_team_name",
        "home_team_spread",
        "game_spread",
        "home_favorite",
        "type_id",
        "type_text",
        "text",
        "coordinate_x",
        "coordinate_y",
        "scoring_play",
        "home_score",
        "away_score")
      )
    ) |> 
    mutate(across(contains("_id"), as.numeric) ) |> 
    mutate(across(contains("scor"), as.numeric) ) |> 
    # filter(coordinate_x == 25, coordinate_y == 0) |> 
    filter(type_id == 540) |> #freethrows
    mutate(make_ft = scoring_play) |> 
    bind_rows(pbp)
  cat("finished season ", s, "\n")
}

mcbb_games <- hoopR::load_mbb_schedule(start_season:end_season)

#denote who is shooting the free throws
ft_half_summary <- pbp |> 
  mutate(first_half = period_number == 1) |> 
  group_by_at(vars(game_id,
                   game_date_time,
                   first_half,
                   team_id:home_favorite)
              ) |> 
  summarize(fta = n(),
            ftm = sum(make_ft)
            ) |> 
  mutate(ftp = ftm / fta) |> 
  left_join(mcbb_games |> 
              distinct(game_id, 
                       neutral_site,
                       home_logo,
                       away_logo,
                       attendance),
            by = "game_id")

ft_half_summary <- ft_half_summary |> 
  mutate(ft_visiting_team = ifelse(team_id == away_team_id & !neutral_site, 1, 0),
         ft_team = ifelse(team_id == home_team_id, 
                          paste(home_team_name, season, sep = '-'),
                          paste(away_team_name, season, sep = '-')
                          ),
         half_chr = ifelse(first_half, "first", "second")
         ) |>
  ungroup()

ft_half_summary_wide <- ft_half_summary |> 
  select(-first_half) |> 
  pivot_wider(names_from = half_chr,
              values_from = c("fta", "ftm", "ftp"),
              values_fill = 0)

#define team names
ft_team_tbl <- ft_half_summary_wide |> 
  count(ft_team) |> 
  mutate(adj_ft_team = ifelse(n >= 20, ft_team, "replacement_team"))

home_team_name_tbl <- ft_half_summary_wide |> 
  count(home_team_name) |> 
  mutate(adj_home_team_name = ifelse(n >= 15, home_team_name, "replacement_team"))

ft_half_summary_wide <- ft_half_summary_wide |> 
  left_join(ft_team_tbl |> 
              select(-n), 
            by = "ft_team") |> 
  left_join(home_team_name_tbl |> 
              select(-n), 
            by = "home_team_name") 

# cbind(ftm, fta) ~ 1 + (1 | ft_team) + ft_visiting_team:(1 | home_team_name) + neutral_site

first_half_model <- stan_glmer(cbind(ftm_first, fta_first) ~ 1 +
                                 (1 | adj_ft_team) + 
                                 ft_visiting_team:(1 | adj_home_team_name) +
                                 neutral_site,
                               data = ft_half_summary_wide,
                               family = binomial,
                               prior = laplace(autoscale = TRUE),
                               iter = 800,
                               chains = 4)
# first_half_model |> 
# s3write_using(FUN = write_rds,
#               bucket = bucket_name,
#               object = "College Basketball/ft_model_first_half.rds")

second_half_model <- stan_glmer(cbind(ftm_second, fta_second) ~ 1 +
                                 (1 | adj_ft_team) + 
                                 ft_visiting_team:(1 | adj_home_team_name) +
                                 neutral_site,
                               data = ft_half_summary_wide,
                               family = binomial,
                               prior = laplace(autoscale = TRUE),
                               iter = 800,
                               chains = 4)

# second_half_model |> 
#   s3write_using(FUN = write_rds,
#                 bucket = bucket_name,
#                 object = "College Basketball/ft_model_second_half.rds")

## read in aws model results
first_half_model <- 
  s3read_using(FUN = read_rds,
                bucket = bucket_name,
                object = "College Basketball/ft_model_first_half.rds")

second_half_model <- 
  s3read_using(FUN = read_rds,
                bucket = bucket_name,
                object = "College Basketball/ft_model_second_half.rds")


broom.mixed::tidy(first_half_model)
broom.mixed::tidy(second_half_model)

first_half_model_ranef <- ranef(first_half_model)
second_half_model_ranef <- ranef(second_half_model)

#avg home court effects
home_team_ft_effect <- tibble(
  home_team = row.names(first_half_model_ranef$adj_home_team_name),
  first_half_effect = first_half_model_ranef$adj_home_team_name[,1]
  ) |> 
  left_join(
    tibble(
      home_team = row.names(second_half_model_ranef$adj_home_team_name),
      second_half_effect = second_half_model_ranef$adj_home_team_name[,1]
      ),
    by = c("home_team")
    ) |> 
  mutate(avg_effect = (first_half_effect + second_half_effect) / 2,
         diff_effect = second_half_effect - first_half_effect
         )

#more negative effect means the home court affected the team more
home_team_ft_effect |> 
  arrange(avg_effect)
  
#more negative difference means the second half affected more than the first
home_team_ft_effect |> 
  arrange(diff_effect)


### For simpler method: calculate home team ft% for each season
## model only away games the difference in ft% vs home

ft_half_summary_wide <- ft_half_summary_wide |> 
  mutate(fta_game = fta_second + fta_first,
         ftm_game = ftm_second + ftm_first)

#ft percentage in non-away games
team_season_home_ft_pct <- ft_half_summary_wide |> 
  filter(ft_visiting_team == 0) |> 
  group_by(adj_ft_team) |> 
  summarize(tot_fta = sum(fta_game),
            tot_ftm = sum(ftm_game),
            games = n(),
            avg_fta = mean(fta_game),
            avg_ftm = mean(ftm_game),
            avg_ftp = sum(ftm_game) / sum(fta_game)
  )

away_team_game_summary <- ft_half_summary_wide |> 
  filter(ft_visiting_team == 1) |> 
  left_join(team_season_home_ft_pct |> 
              rename_at(
                vars(tot_fta:last_col()), 
                ~paste0("home_court_", .x)
                ),
            by = c("adj_ft_team")
            ) |> 
  mutate(ftp_game = ftm_game / fta_game,
         diff_ftp_from_home = ftp_game - home_court_avg_ftp)


diff_ftp_model <- stan_lmer(diff_ftp_from_home ~ 1 + (1 | adj_home_team_name),#the home court team 
                                data = away_team_game_summary,
                                prior = normal(),
                                weights = fta_game,
                                iter = 800,
                                chains = 4)


summary(diff_ftp_model)
#avg home court effects
ranef_diff_ftp_model <- ranef(diff_ftp_model)
home_team_ft_effect <- tibble(
  home_team = row.names(ranef_diff_ftp_model$adj_home_team_name),
  ft_effect = ranef_diff_ftp_model$adj_home_team_name[,1]
) 