# Script for drawing deck matchups for "random decks, model's choice":
# http://forums.sirlingames.com/t/tournament-format-random-decks-models-choice/7706

library(data.table)
library(codex)
library(magrittr)
sim <- readRDS("results/tidy_vs_split.rds")
nash <- readRDS("results/multicolour_nash_vs_split.rds")

# 1. Deathstrike Deathmatch (Nash: pick by probability best)

# 2. One hand tied (Nash: pick by probability weakest)

# same as codex::reformat_used_nash, without removing zero-weight decks,
# and reversing weight sort
cols <- colnames(nash)
shaped <- melt(
  nash,
  id.vars = "Player", measure.vars = cols[-(1:2)],
  variable.name = "Deck", value.name = "Matchup") %>%
  dcast(Deck ~ Player, value.var = "Matchup"
  )
shaped$Deck <- as.character(shaped$Deck)
shaped <- shaped[order(Both, Deck)]

cat(paste(shaped[order(P1), head(Deck, 10)], collapse = "\n"))
cat(paste(shaped[order(P2), head(Deck, 10)], collapse = "\n"))

# The rest is exploring weightings.
# change to weights proportional to 1 - Nash weight
decks_by_worst <- copy(shaped)[
  ,
  c("Both", "P1", "P2") := lapply(.SD, function(x) (1 - x)/(.N - 1)),
  .SDcols = c("Both", "P1", "P2")
][]
# Result is near-uniform, since SD -> SD/sqrt(.N - 1)

decks_by_worst2 <- copy(shaped)[
  order(Both, Deck)
][
  ,
  c("Both", "P1", "P2") := lapply(.SD, function(x) (max(x) - x)/(.N*max(x) - 1)),
  .SDcols = c("Both", "P1", "P2")
][]

plot(cumsum(shaped$Both), type = "l", ylab = "Cumulative weight")
lines(1:3084, cumsum(decks_by_worst$Both), lty = "dashed")
lines(1:3084, cumsum(decks_by_worst2$Both), lty = "dotted")
lapply(
  list(
    nash = shaped$Both,
    negnash = decks_by_worst$Both,
    negnash2 = decks_by_worst2$Both
  ),
  function(x) Position(function(y) y <= 0.75, cumsum(x), right = TRUE)
)

# Above is for Both, but we could use P1 and P2 instead with no extra effort

# 3. Midori's Balanced Bouts (choose deck pairings expected to be balanced)
# This could mean two different things:
# Weight by chance to be most balanced
# Weight by mean fairness

matchups <- readRDS("results/multicolour_mean_matchup_vs_split.rds")
sorted_matchups <- melt(
  as.data.table(
    matchups,
    keep.rownames = "P1"
  ),
  id.vars = "P1",
  variable.name = "P2",
  value.name = "matchup",
  variable.factor = FALSE
)[
  ,
  fairness := 1 - 2*abs(matchup - 0.5)
][
  order(-fairness)
][]
head(sorted_matchups, 20)
sorted_matchups[fairness >= 0.999999, .N]
matchup_pool <- head(sorted_matchups, 35)

matchup_pool[, .(deck = c(P1, P2))][, .N, by = "deck"][order(-N)]
pool_components <- matchup_pool[
  , .(deck = c(P1, P2))
][, codexdata::components(deck, codexdata::starters, codexdata::nicknames)]
pool_components[, table(starter)]
pool_components[, table(c(spec1, spec2, spec3))]

# 4. Midori's Final Balanced Buster (as 3., but account for player skill levels)

# 2.5 As for 2., but choose balanced pairings as in 3.

example_P1 <- shaped[order(P1), head(Deck, 10)]
example_P1_matchups <- sorted_matchups[
  .(P1 = example_P1),
  on = "P1",
  mult = "first"
]
example_P2 <- shaped[order(P2), head(Deck, 10)]
example_P2_matchups <- sorted_matchups[
  .(P2 = example_P2),
  on = "P2",
  mult = "first"
]

example_P1_matchups
example_P2_matchups
