#convert_team_abbreviation.R
library(dplyr)

# Create a tibble with the team conversion information
team_conversion <- tibble(
  old_abbreviation = c("STL", "SD", "OAK"),
  new_abbreviation = c("LA", "LAC", "LV"),
  move_season = c(2016, 2017, 2020)
)

# Function to convert team abbreviations in both directions
convert_team_abbreviation <- function(data, team_conversion, col_name = "team") {
  
  data <- data %>%
    rename(team_rename = all_of(col_name))
  
  # Forward conversion: old abbreviation to new abbreviation if season >= move_season
  data <- data %>%
    left_join(team_conversion, by = c("team_rename" = "old_abbreviation")) %>%
    mutate(
      team_rename = if_else(season >= move_season & !is.na(new_abbreviation), new_abbreviation, team_rename),
      move_season = NULL, new_abbreviation = NULL
    )
  
  # Reverse conversion: new abbreviation to old abbreviation if season < move_season
  data <- data %>%
    left_join(team_conversion, by = c("team_rename" = "new_abbreviation")) %>%
    mutate(
      team_rename = if_else(season < move_season & !is.na(old_abbreviation), old_abbreviation, team_rename),
      move_season = NULL, old_abbreviation = NULL
    )
  
  # Rename the column back
  data <- data %>%
    rename_at(vars(team_rename), ~ str_replace(.x, "team_rename", col_name ) )
  
  return(data)
}

# # Example data
# data <- tibble(
#   team = c('STL', 'STL', 'SD', 'SD', 'OAK', 'OAK', 'LA', 'LAC', 'LV'),
#   season = c(2015, 2016, 2016, 2017, 2019, 2020, 2015, 2016, 2019)
# )
# 
# # Apply the conversion function
# converted_data <- convert_team_abbreviation(data, team_conversion)

# Print the converted data
# print(converted_data)