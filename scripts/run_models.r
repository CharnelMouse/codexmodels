library(rstan)
source("scripts/configure_rstan.r")
source("scripts/load_data.r")

simple_deck_pars <- c(
  "turn", "matchup",
  "sd_player", "sd_player_turn", "sd_deck", "sd_deck_turn",
  "player", "player_turn",
  "deck_turn", "deck",
  "log_lik"
)

split_deck_pars <- c(
  "turn", "matchup",
  "sd_player", "sd_player_turn",
  "sd_starter", "sd_starter_turn",
  "sd_spec", "sd_spec_turn",
  "player", "player_turn",
  "starter", "starter_turn",
  "spec", "spec_turn",
  "log_lik"
)

inter_deck_pars <- c(
  "turn", "matchup",
  "sd_player", "sd_player_turn",
  "sd_starter", "sd_starter_turn",
  "sd_spec", "sd_spec_turn",
  "sd_starter_spec",
  "player", "player_turn",
  "starter", "starter_turn",
  "spec", "spec_turn",
  "starter_spec",
  "log_lik"
)

full_inter_deck_pars <- c(
  "turn",
  "matchup",
  "sd_player", "sd_player_turn", "sd_starter", "sd_starter_turn",
  "sd_spec", "sd_spec_turn", "sd_starter_spec", "sd_spec_spec",
  "player", "player_turn",
  "starter", "starter_turn",
  "spec", "spec_turn",
  "starter_spec",
  "spec_spec",
  "log_lik"
)

vs_split_pars <- c(
  "matchup",
  "sd_player", "sd_starter_vs_starter", "sd_starter_vs_spec", "sd_spec_vs_spec",
  "player",
  "starter_vs_starter", "starter_vs_spec", "spec_vs_starter", "spec_vs_spec",
  "log_lik"
)

vs_split_gamma_pars <- c(
  "matchup",
  "var_player", "var_starter_vs_starter", "var_starter_vs_spec", "var_spec_vs_spec",
  "player",
  "starter_vs_starter", "starter_vs_spec", "spec_vs_starter", "spec_vs_spec",
  "log_lik"
)

run <- function(model_name, data, pars, results_name, delta, tree) {
  res <- if (!delta && !tree)
    stan(
      paste0("models/", model_name, ".stan"),
      seed = seed_number,
      data = data,
      pars = pars,
      cores = 1
    )
  else {
    con <- c(adapt_delta = if (delta) list(delta),
             max_treedepth = if (tree) list(tree))
    stan(
      paste0("models/", model_name, ".stan"),
      seed = seed_number,
      data = data,
      pars = pars,
      control = con,
      cores = 1
    )
  }
  saveRDS(res, paste0("results/", results_name, ".rds"))
  NULL
}

Reduce(
  function(x, y) do.call(run, y),
  list(
    list("simple_deck"    , basic_data                  , simple_deck_pars    , "simple_deck"                , FALSE, FALSE),
    list("split_deck"     , mean_data                   , split_deck_pars     , "split_deck"                 , FALSE, FALSE),
    list("inter_deck"     , mean_data                   , inter_deck_pars     , "inter_deck"                 , 0.85 , FALSE),
    list("full_inter_deck", mean_data                   , full_inter_deck_pars, "full_inter_deck"            , 0.95 , FALSE),
    list("vs_split"       , mean_data                   , vs_split_pars       , "vs_split"                   , FALSE, 12),
    list("vs_split"       , current_mean_data           , vs_split_pars       , "current_vs_split"           , FALSE, 12),
    list("vs_split"       , metal_mean_data             , vs_split_pars       , "vs_split_metal"             , FALSE, 12),
    list("vs_split"       , current_metal_mean_data     , vs_split_pars       , "current_vs_split_metal"     , FALSE, 12),
    list("vs_split"       , full_metal_mean_data        , vs_split_pars       , "vs_split_full_metal"        , FALSE, 12),
    list("vs_split"       , current_full_metal_mean_data, vs_split_pars       , "current_vs_split_full_metal", FALSE, 12),
    list("vs_negative"    , mean_data                   , vs_split_pars       , "vs_split_negative"          , FALSE, 12),
    list("vs_split_gamma" , mean_data                   , vs_split_gamma_pars , "vs_split_gamma"             , FALSE, 12)
  ),
  init = NULL
)
