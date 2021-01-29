library(codex)
library(stringr)
library(data.table)
library(parallel)

n.cores <- detectCores()
current_results <- readRDS("results/tidy_vs_split.rds")

# expensive
multicolour_nash_array <- get_nash_array_from_sim(current_results, "multi", parallel = n.cores)
saveRDS(multicolour_nash_array, "results/multicolour_nash_array_vs_split.rds")
multicolour_calibrated_nash <- nash_mean(multicolour_nash_array)
saveRDS(multicolour_calibrated_nash, "results/multicolour_nash_vs_split.rds")

# expensive
counter_example <- counter("Miracle Grow", current_results$vs_array)
nms <- counter_example$Deck
cl <- makeCluster(n.cores)
counters <- rbindlist(
  parLapply(cl, setNames(nms, nms), counter, current_results$vs_array),
  idcol = "Opponent"
)
stopCluster(cl)
counters[, c("Opponent", "Deck") := lapply(.SD, factor, levels = sort(nms)),
         .SDcols = c("Opponent", "Deck")]
saveRDS(counters, "results/multicolour_counters.rds")
