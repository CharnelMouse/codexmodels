library(codex)
source("scripts/configure_rstan.r")
source("scripts/load_data.r")

n_eff <- function(results) {
  UseMethod("n_eff", results)
}

n_eff.stanfit <- function(x) {
  as.data.table(
    rstan::summary(x)$summary,
    keep.rownames = "parameter"
  )[parameter != "lp_",
    c("parameter", "n_eff")]
}

n_eff.CmdStanMCMC <- function(x) {
  as.data.table(
    x$summary(n_eff = posterior::ess_basic)
  )[variable != "lp_",
    .(parameter = variable, n_eff)
  ]
}

tidy_save <- function(name, data, matches, vs_array, n_eff, log_lik) {
  x <- readRDS(paste0("results/", name, ".rds"))
  tidy <- get_tidy_model_results(x, data, matches, vs_array, log_lik)
  saveRDS(tidy, paste0("results/tidy_", name, ".rds"))
  if (n_eff) {
    neff_info <- n_eff(x)
    fwrite(neff_info, paste0("results/", name, "_n_eff.csv"))
  }
  unlink(paste0("results/", name, ".rds"))
  NULL
}

Reduce(
  function(x, y) do.call(tidy_save, y),
  list(
    list("simple_deck"                , basic_data                  , normal_matches            , FALSE, FALSE, TRUE),
    list("split_deck"                 , mean_data                   , normal_matches            , FALSE, FALSE, TRUE),
    list("inter_deck"                 , mean_data                   , normal_matches            , FALSE, FALSE, TRUE),
    list("full_inter_deck"            , mean_data                   , normal_matches            , FALSE, FALSE, TRUE),
    list("vs_split"                   , mean_data                   , normal_matches            , TRUE , TRUE , TRUE),
    list("current_vs_split"           , current_mean_data           , current_matches           , TRUE , FALSE, TRUE),
    list("vs_split_metal"             , metal_mean_data             , metal_matches             , TRUE , FALSE, TRUE),
    list("current_vs_split_metal"     , current_metal_mean_data     , current_metal_matches     , TRUE , FALSE, TRUE),
    list("vs_split_full_metal"        , full_metal_mean_data        , full_metal_matches        , TRUE , FALSE, TRUE),
    list("current_vs_split_full_metal", current_full_metal_mean_data, current_full_metal_matches, TRUE , FALSE, TRUE),
    list("vs_split_negative"          , mean_data                   , normal_matches            , TRUE , FALSE, TRUE)
  ),
  init = NULL
)
