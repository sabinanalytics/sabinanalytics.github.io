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
first_half_model |> 
s3write_using(FUN = write_rds,
              bucket = bucket_name,
              object = "College Basketball/ft_model_first_half.rds")

second_half_model <- stan_glmer(cbind(ftm_second, fta_second) ~ 1 +
                                 (1 | adj_ft_team) + 
                                 ft_visiting_team:(1 | adj_home_team_name) +
                                 neutral_site,
                               data = ft_half_summary_wide,
                               family = binomial,
                               prior = laplace(autoscale = TRUE),
                               iter = 800,
                               chains = 4)

second_half_model |> 
  s3write_using(FUN = write_rds,
                bucket = bucket_name,
                object = "College Basketball/ft_model_second_half.rds")