library(data.table)
library(codexdata)
library(codex)
current <- "CAMS21"
seed_number <- 2018 - 11 - 10

is_standard_match <- function(victor, victory, map, deck1, deck2) {
  (is.element(victory, c("normal", "")) | is.na(victory)) &
    is.na(map) &
    !is.na(victor) &
    !endsWith(deck1, "F20") &
    !endsWith(deck2, "F20")
}

normal_matches <- matches[
  tournament != "Casual" &
    recorder == "charnel_mouse" &
    is_standard_match(victor, victory, map, deck1, deck2)
  ]
basic_data <- prepare_match_data_for_modelling(normal_matches, codexdata::starters, codexdata::nicknames, mean = FALSE)
mean_data <- prepare_match_data_for_modelling(normal_matches, codexdata::starters, codexdata::nicknames, mean = TRUE)
if (!any(normal_matches$tournament == current)) {
  current_matches <- normal_matches
  current_mean_data <- mean_data
}else{
  current_start_date <- min(normal_matches[tournament == current, min(start)], Sys.Date())
  current_matches <- normal_matches[end < current_start_date]
  current_mean_data <- prepare_match_data_for_modelling(current_matches,
                                                        codexdata::starters, codexdata::nicknames, mean = TRUE)
}
setindex(normal_matches, NULL)

metal_matches <- matches[tournament != "Casual" &
                                      is_standard_match(victor, victory, map, deck1, deck2)]
metal_basic_data <- prepare_match_data_for_modelling(metal_matches, codexdata::starters, codexdata::nicknames, mean = FALSE)
metal_mean_data <- prepare_match_data_for_modelling(metal_matches, codexdata::starters, codexdata::nicknames, mean = TRUE)

if (!any(metal_matches$tournament == current)) {
  current_metal_matches <- metal_matches
  current_metal_mean_data <- metal_mean_data
}else{
  current_metal_start_date <- min(metal_matches[tournament == current, min(start)], Sys.Date())
  current_metal_matches <- metal_matches[end < current_start_date]
  current_metal_mean_data <- prepare_match_data_for_modelling(current_metal_matches,
                                                              codexdata::starters, codexdata::nicknames, mean = TRUE)
}
full_metal_matches <- matches[is_standard_match(victor, victory, map, deck1, deck2)]
full_metal_basic_data <- prepare_match_data_for_modelling(full_metal_matches, codexdata::starters, codexdata::nicknames, mean = FALSE)
full_metal_mean_data <- prepare_match_data_for_modelling(full_metal_matches, codexdata::starters, codexdata::nicknames, mean = TRUE)
if (!any(full_metal_matches$tournament == current)) {
  current_full_metal_matches <- full_metal_matches
  current_full_metal_mean_data <- full_metal_mean_data
}else{
  current_full_metal_start_date <- min(full_metal_matches[tournament == current, min(start)], Sys.Date())
  current_full_metal_matches <- full_metal_matches[end < current_start_date]
  current_full_metal_mean_data <- prepare_match_data_for_modelling(current_full_metal_matches,
                                                                   codexdata::starters, codexdata::nicknames, mean = TRUE)
}
