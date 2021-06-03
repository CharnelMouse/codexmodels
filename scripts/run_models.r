library(rstan)
library(cmdstanr)
source("scripts/configure_rstan.r")
source("scripts/load_data.r")

# simple_deck_pars <- c(
#   "turn", "matchup",
#   "sd_player", "sd_player_turn", "sd_deck", "sd_deck_turn",
#   "player", "player_turn",
#   "deck_turn", "deck",
#   "log_lik"
# )

simple_deck_inputs <- c(
  "M",
  "P",
  "D",
  "first_player",
  "second_player",
  "first_deck",
  "second_deck",
  "w"
)

# split_deck_pars <- c(
#   "turn", "matchup",
#   "sd_player", "sd_player_turn",
#   "sd_starter", "sd_starter_turn",
#   "sd_spec", "sd_spec_turn",
#   "player", "player_turn",
#   "starter", "starter_turn",
#   "spec", "spec_turn",
#   "log_lik"
# )

split_deck_inputs <- c(
  "M",
  "P",
  "St",
  "Sp",
  "first_player",
  "second_player",
  "first_starter",
  "second_starter",
  "first_specs1",
  "first_specs2",
  "first_specs3",
  "second_specs1",
  "second_specs2",
  "second_specs3",
  "w"
)

inter_deck_inputs <- c(
  "M",
  "P",
  "St",
  "Sp",
  "first_player",
  "second_player",
  "first_starter",
  "second_starter",
  "first_specs1",
  "first_specs2",
  "first_specs3",
  "second_specs1",
  "second_specs2",
  "second_specs3",
  "w"
)

# inter_deck_pars <- c(
#   "turn", "matchup",
#   "sd_player", "sd_player_turn",
#   "sd_starter", "sd_starter_turn",
#   "sd_spec", "sd_spec_turn",
#   "sd_starter_spec",
#   "player", "player_turn",
#   "starter", "starter_turn",
#   "spec", "spec_turn",
#   "starter_spec",
#   "log_lik"
# )

full_inter_deck_inputs <- c(
  "M",
  "P",
  "St",
  "Sp",
  "SpSp",
  "first_player",
  "second_player",
  "first_starter",
  "second_starter",
  "first_specs1",
  "first_specs2",
  "first_specs3",
  "second_specs1",
  "second_specs2",
  "second_specs3",
  "first_starter_specs1",
  "first_starter_specs2",
  "first_starter_specs3",
  "second_starter_specs1",
  "second_starter_specs2",
  "second_starter_specs3",
  "first_spec_specs1",
  "first_spec_specs2",
  "first_spec_specs3",
  "second_spec_specs1",
  "second_spec_specs2",
  "second_spec_specs3",
  "w"
)

# full_inter_deck_pars <- c(
#   "turn",
#   "matchup",
#   "sd_player", "sd_player_turn", "sd_starter", "sd_starter_turn",
#   "sd_spec", "sd_spec_turn", "sd_starter_spec", "sd_spec_spec",
#   "player", "player_turn",
#   "starter", "starter_turn",
#   "spec", "spec_turn",
#   "starter_spec",
#   "spec_spec",
#   "log_lik"
# )

vs_split_inputs <- c(
  "M",
  "P",
  "St",
  "Sp",
  "first_player",
  "second_player",
  "first_starter",
  "second_starter",
  "first_specs1",
  "first_specs2",
  "first_specs3",
  "second_specs1",
  "second_specs2",
  "second_specs3",
  "w"
)

# vs_split_pars <- c(
#   "matchup",
#   "sd_player", "sd_starter_vs_starter", "sd_starter_vs_spec", "sd_spec_vs_spec",
#   "player",
#   "starter_vs_starter", "starter_vs_spec", "spec_vs_starter", "spec_vs_spec",
#   "log_lik"
# )

vs_split_gamma_inputs <- c(
  "M",
  "P",
  "St",
  "Sp",
  "first_player",
  "second_player",
  "first_starter",
  "second_starter",
  "first_specs1",
  "first_specs2",
  "first_specs3",
  "second_specs1",
  "second_specs2",
  "second_specs3",
  "w"
)

# vs_split_gamma_pars <- c(
#   "matchup",
#   "var_player", "var_starter_vs_starter", "var_starter_vs_spec", "var_spec_vs_spec",
#   "player",
#   "starter_vs_starter", "starter_vs_spec", "spec_vs_starter", "spec_vs_spec",
#   "log_lik"
# )

model_names <- c(
  "simple_deck",
  "split_deck",
  "inter_deck",
  "full_inter_deck",
  "vs_split",
  "vs_negative",
  "vs_split_gamma"
)

models <- setNames(
  lapply(
    model_names,
    function(model_name) {
      cmdstan_model(paste0("models/", model_name, ".stan"))
    }
  ),
  model_names
)

run <- function(model_name, data, inputs, results_name, delta, tree) {
  model <- models[[model_name]]
  res <- if (!delta && !tree)
    model$sample(
      seed = seed_number,
      data = data[inputs]
    )
  else {
    model$sample(
      seed = seed_number,
      data = data[inputs],
      adapt_delta = if (delta) delta,
      max_treedepth = if (tree) tree
    )
  }
  res$save_object(paste0("results/", results_name, ".rds"))
  NULL
}

Reduce(
  function(x, y) do.call(run, y),
  list(
    list("simple_deck"    , basic_data                  , simple_deck_inputs    , "simple_deck"                , FALSE, FALSE),
    list("split_deck"     , mean_data                   , split_deck_inputs     , "split_deck"                 , FALSE, FALSE),
    list("inter_deck"     , mean_data                   , inter_deck_inputs     , "inter_deck"                 , 0.85 , FALSE),
    list("full_inter_deck", mean_data                   , full_inter_deck_inputs, "full_inter_deck"            , 0.95 , FALSE),
    list("vs_split"       , mean_data                   , vs_split_inputs       , "vs_split"                   , FALSE, 12),
    list("vs_split"       , current_mean_data           , vs_split_inputs       , "current_vs_split"           , FALSE, 12),
    list("vs_split"       , metal_mean_data             , vs_split_inputs       , "vs_split_metal"             , FALSE, 12),
    list("vs_split"       , current_metal_mean_data     , vs_split_inputs       , "current_vs_split_metal"     , FALSE, 12),
    list("vs_split"       , full_metal_mean_data        , vs_split_inputs       , "vs_split_full_metal"        , FALSE, 12),
    list("vs_split"       , current_full_metal_mean_data, vs_split_inputs       , "current_vs_split_full_metal", FALSE, 12),
    list("vs_negative"    , mean_data                   , vs_split_inputs       , "vs_split_negative"          , FALSE, 12),
    list("vs_split_gamma" , mean_data                   , vs_split_gamma_inputs , "vs_split_gamma"             , FALSE, 12)
  ),
  init = NULL
)
