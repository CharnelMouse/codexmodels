current <- "XCAPS21"
library(data.table)

current_matches <- codexdata::matches[tournament == current]
fwrite(current_matches, "results/website_current_matches.csv")

tidy_current_vs_split <- readRDS("results/tidy_current_vs_split.rds")

website_current_vs_split <- tidy_current_vs_split[c("tidy_results", "vs_array")]
website_current_vs_split$tidy_results <-
  website_current_vs_split$tidy_results[c("sd_player", "player")]

saveRDS(website_current_vs_split, "results/website_current_vs_split.rds")


tidy_vs_split <- readRDS("results/tidy_vs_split.rds")

website_vs_split <- tidy_vs_split[c("tidy_results", "vs_array")]
website_vs_split$tidy_results <- website_vs_split$tidy_results[
  c(
    "matchup",
    "sd_player",
    "sd_starter_vs_starter",
    "sd_starter_vs_spec",
    "sd_spec_vs_spec",
    "player"
  )
]

saveRDS(website_vs_split, "results/website_vs_split.rds")
