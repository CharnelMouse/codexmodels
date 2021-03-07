library(data.table)
library(codexdata)
library(codex)
library(ggplot2)
library(gridExtra)
theme_codex <- theme_dark() +
  theme(panel.border = element_rect(fill = NA, colour = "grey13"),
        plot.background = element_rect(fill = "grey13", colour = "grey13"),
        axis.ticks = element_line(colour = "white"),
        strip.background = element_rect(fill = "grey13", colour = NA),
        legend.background = element_rect(fill = "grey13", colour = NA),
        text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"))
theme_set(theme_codex)

starts <- matches[
  recorder == "charnel_mouse" & tournament != "MMM1",
  .(start = min(start)),
  by = "tournament"
]
draft_picks <- matches[
  recorder == "charnel_mouse" & tournament == "XCAPS19",
  .(tournament, deck = c(deck1, deck2))
][!is.na(deck)]
# Remove RACE2 since we only have last few matches, and Metalize tournament
excluded_tournaments <- c(
  "RACE2",
  "Casual",
  "Monocolour-only, first Moscow offline event with floor rules enforcement",
  "Igrokon Moscow vs. Petersburg showmatches",
  "Moscow Igrokon qualifier",
  "Petersburg Igrokon qualifier"
)
picks <- rbind(
  decks[!is.element(tournament, excluded_tournaments)],
  draft_picks, fill = TRUE
)[, c(.(tournament = tournament), components(deck, starters, nicknames))]
draft_players <- matches[
  is.element(tournament, c("XCAPS19", "LLL2")),
  .(tournament, player = c(player1, player2))
][, .(player = unique(player)), by = "tournament"]
player_counts <- unique(rbind(
  decks[, c("tournament", "player")], draft_players
))[, .(count = .N), by = "tournament"]
starter_picks <- picks[
  , .(count = .N), by = c("tournament", "starter")
][CJ(tournament, starter, unique = TRUE), on = c("tournament", "starter")
][, count := ifelse(is.na(count), 0, count)
][!endsWith(starter, "F20")]
# means taken with each tournament having equal weight, not weighted by number of entrants
mean_starter_counts <- starter_picks[
  , .(starter, count = count/sum(count)), by = "tournament"
][!is.nan(count), .(count = mean(count)), by = "starter"
][order(-count)
][, .(starter = factor(starter, starter), count)]
spec_picks <- picks[
  , .(tournament, spec = c(spec1, spec2, spec3))
][, .(count = .N), by = c("tournament", "spec")
][CJ(tournament, spec, unique = TRUE),
  on = c("tournament", "spec")
][, count := ifelse(is.na(count), 0, count)
][!endsWith(spec, "F20")]
mean_spec_counts <- spec_picks[
  , .(spec, count = count/sum(count)), by = "tournament"
][!is.nan(count), .(count = mean(count)), by = "spec"
][order(-count)
][, .(spec = factor(spec, spec), count)]
starter_colours <- setNames(
  c(
    "#000000",
    "#162899",
    "#207d26",
    "#a39387",
    "#5a177e",
    "#8c201b",
    "#f8f8f8"
  ),
  sort(unique(starters$starter[!endsWith(starters$starter, "F20")]))
)
spec_colours <- setNames(
  c(
    "#ef86f5",
    "#26dee1",
    "#fa7d7e",
    "#f06065",
    "#f550f6",
    "#4c9dff",
    "#d7d330",
    "#b8966e",
    "#8eefe6",
    "#f09156",
    "#c47cff",
    "#13c863",
    "#fff84f",
    "#7c7e7d",
    "#b89269",
    "#ecb46d",
    "#f67a2c",
    "#ebeb73",
    "#a9a9a9",
    "#31a3fd"
  ),
  sort(unique(starters$spec[!endsWith(starters$starter, "F20")]))
)
start_fill <- ggplot(
  starter_picks,
  aes(x = factor(tournament, starts$tournament), y = count, fill = starter)
) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = starter_colours) +
  xlab("tournament") +
  ylab("proportion")
start_line <- ggplot(
  copy(starter_picks[
    , .(
      starter = factor(
        starter,
        levels = levels(mean_starter_counts$starter)),
      count = count/sum(count)
    ),
    by = "tournament"
  ])[!is.nan(count)],
  aes(
    x = factor(tournament, starts$tournament),
    y = count,
    group = starter,
    colour = "black"
  )
) +
  geom_line() +
  geom_hline(aes(yintercept = count, colour = "grey"), mean_starter_counts) +
  xlab("tournament") +
  ylab("proportion") +
  scale_colour_identity() +
  facet_wrap(. ~ starter, nrow = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
spec_fill <- ggplot(
  spec_picks,
  aes(
    x = factor(tournament, starts$tournament),
    y = count,
    fill = factor(spec, starters[order(starter)][, spec])
  )
) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(name = "spec", values = spec_colours) +
  xlab("tournament") +
  ylab("proportion")
spec_line <- ggplot(
  spec_picks[
    ,
    .(spec = factor(spec, levels = levels(mean_spec_counts$spec)),
      count = count / sum(count)), by = "tournament"
  ][!is.nan(count)],
  aes(
    x = factor(tournament, starts$tournament),
    y = count,
    group = spec,
    colour = "black"
  )
) +
  geom_line() +
  geom_hline(
    aes(yintercept = count, colour = "grey"),
    mean_spec_counts
  ) +
  xlab("tournament") +
  ylab("proportion") +
  scale_colour_identity() +
  facet_wrap(. ~ spec) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

player_line <- ggplot(
  copy(
    player_counts[
      !is.element(tournament, excluded_tournaments)
    ])[,
       tournament := factor(tournament, starts$tournament)],
  aes(x = tournament, y = count, group = 1)
) +
  geom_line() +
  ylab("players") +
  ylim(0, max(player_counts$count)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

deck_picks <- rbind(
  decks[!is.element(tournament, excluded_tournaments)],
  draft_picks, fill = TRUE
)[, .(count = .N), by = c("tournament", "deck")
][CJ(tournament, deck, unique = TRUE), on = c("tournament", "deck")
][, count := ifelse(is.na(count), 0, count)]
total_deck_counts <- deck_picks[
  , .(count = sum(count)), by = "deck"
][order(-count)]
mean_deck_counts <- deck_picks[
  , .(deck, count = count/sum(count)), by = "tournament"
][, .(count = mean(count)), by = "deck"
][order(-count)
][, .(deck = factor(deck, deck), count)]
# Use decks with at least 4 picks, so includes DAB
trimmed_deck_picks <- deck_picks[
  , .(deck, count = count/sum(count)), by = "tournament"
][total_deck_counts[count >= 4, deck], on = "deck"]
trimmed_mean_deck_counts <- mean_deck_counts[
  total_deck_counts[count >= 4, deck], on = "deck"
][order(-count)
][, .(deck = factor(deck, deck), count)]

deck_line <- ggplot(
  trimmed_deck_picks[
    ,
    .(tournament,
      deck = factor(deck, levels = levels(trimmed_mean_deck_counts$deck)),
      count)],
  aes(x = factor(tournament, starts$tournament), y = count, group = deck, colour = "black")
) +
  geom_line() +
  geom_hline(
    aes(yintercept = count, colour = "grey"),
    trimmed_mean_deck_counts
  ) +
  xlab("tournament") +
  ylab("proportion") +
  scale_colour_identity() +
  facet_wrap(. ~ deck) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

data_grid <- grid.arrange(
  grobs = list(player_line, start_line, spec_line, deck_line),
  layout_matrix = matrix(
    c(1, 2,
      3, 4,
      3, 4),
    byrow = TRUE,
    nrow = 3
  )
)
ggsave("results/data.png", data_grid, width = 24, height = 13.5)
ggsave("results/starter_timeline.png", start_line, width = 8, height = 4.5)
ggsave("results/spec_timeline.png", spec_line, width = 8*4/3, height = 4.5*4/3)
ggsave("results/deck_timeline.png", deck_line, width = 8*4/3, height = 4.5*5/4)
ggsave("results/spec_fill.png", spec_fill + theme_classic(), width = 16, height = 9)

# 2D tier list

results <- readRDS("results/tidy_vs_split.rds")
vs_array <- results$vs_array
balanced_vs_array <- (vs_array - aperm(vs_array, c(1L, 3:2)))/2
mean_vs_array <- apply(balanced_vs_array, 2:3, mean)
mean_strengths <- apply(mean_vs_array, 1L, mean)
sd_strengths <- apply(mean_vs_array, 1L, sd)
starter_mean_strengths <- mean_strengths[1:7]
starter_sd_strengths <- sd_strengths[1:7]
spec_mean_strengths <- mean_strengths[8:length(mean_strengths)]
spec_sd_strengths <- sd_strengths[8:length(mean_strengths)]
strengths <- data.table(
  component = names(mean_strengths),
  mean = mean_strengths,
  sd = sd_strengths,
  colour = c(starter_colours, spec_colours)
)
starter_strengths <- strengths[1:7]
spec_strengths <- strengths[8:nrow(strengths)]

library(ggrepel)
ceilby <- function(x, by = 1) {
  ceiling(x/by)*by
}
floorby <- function(x, by = 1) {
  floor(x/by)*by
}
epsilon <- 0.01
starter_xlims <- c(
  floorby(min(starter_sd_strengths), epsilon/2),
  ceilby(max(starter_sd_strengths), epsilon/2)
)
starter_ylims <- c(
  floorby(min(starter_mean_strengths), epsilon),
  ceilby(max(starter_mean_strengths), epsilon)
)
spec_xlims <- c(
  floorby(min(spec_sd_strengths), epsilon/2),
  ceilby(max(spec_sd_strengths), epsilon/2)
)
spec_ylims <- c(
  floorby(min(spec_mean_strengths), epsilon),
  ceilby(max(spec_mean_strengths), epsilon)
)
tier_plot <- function(strengths, epsilon = 0.01) {
  xlims <- c(
    floorby(min(strengths$sd), epsilon/2) - epsilon/4,
    ceilby(max(strengths$sd), epsilon/2) + epsilon/4
  )
  ylims <- c(
    floorby(min(strengths$mean), epsilon) - epsilon/2,
    ceilby(max(strengths$mean), epsilon) + epsilon/2
  )
  ggplot(strengths, aes(x = sd, y = mean)) +
    geom_polygon(
      aes(x = x, y = y, fill = "lightgrey", alpha = 0.5, group = 1),
      data = data.frame(
        x = c(rev(xlims),
              xlims),
        y = quantile(strengths$mean, c(0.25, 0.25, 0.75, 0.75))
      )
    ) +
    geom_polygon(
      aes(x = x, y = y, fill = "lightgrey", alpha = 0.5, group = 1),
      data = data.frame(
        y = c(ylims,
              rev(ylims)),
        x = quantile(strengths$sd, c(0.25, 0.25, 0.75, 0.75))
      )
    ) +
    geom_hline(yintercept = median(strengths$mean)) +
    geom_vline(xintercept = median(strengths$sd)) +
    geom_point(aes(colour = "black", fill = colour, size = 2L, shape = 21L)) +
    geom_text_repel(aes(label = component)) +
    scale_colour_identity() +
    scale_fill_identity() +
    scale_alpha_identity() +
    scale_shape_identity() +
    scale_size_identity() +
    scale_x_continuous(
      name = "effect spread (sd)",
      limits = rev(xlims),
      trans = "reverse"
    ) +
    scale_y_continuous(name = "average effect", limits = ylims) +
    coord_cartesian(expand = FALSE) +
    theme_minimal()
}
starter_tier <- tier_plot(starter_strengths)
spec_tier <- tier_plot(spec_strengths)

nash <- reformat_used_nash(readRDS("results/multicolour_nash_vs_split.rds"))
starter_weights <- nash[
  , .(components(Deck, starters, nicknames), Both)
][, .(weight = sum(Both)), by = starter
][order(starter)]
spec_weights <- nash[
  , .(components(Deck, starters, nicknames), Both)
][, .(spec = c(spec1, spec2, spec3), Both = rep(Both, 3L))
][, .(weight = sum(Both)), by = spec
][order(spec)]
starter_nash_tier <- tier_plot(
  cbind(
    starter_weights[, .(component = starter, mean = weight)],
    starter_strengths[, c("sd", "colour")]
  )
) +
  scale_y_continuous(name = "average Nash weight")
spec_nash_tier <- tier_plot(
  cbind(
    spec_weights[, .(component = spec, mean = weight)],
    spec_strengths[, c("sd", "colour")]
  )
) +
  scale_y_continuous(name = "average Nash weight")
ggsave("results/starter_tier.png", starter_tier, width = 8, height = 5)
ggsave("results/spec_tier.png", spec_tier, width = 8, height = 5)
ggsave("results/starter_nash_tier.png", starter_nash_tier, width = 8, height = 5)
ggsave("results/spec_nash_tier.png", spec_nash_tier, width = 8, height = 5)

mononames <- paste0(
  "Mono",
  c("Black", "Blue", "Green", "Purple", "Red", "White")
)
mono_matchups <- get_matchup_array(results, mononames)
balanced_mono_matchups <- (mono_matchups + 1 - aperm(mono_matchups, c(1L, 3:2)))/2
mono_strengths <- data.table(
  component = mononames,
  mean = apply(balanced_mono_matchups, 2L, mean),
  sd = apply(apply(balanced_mono_matchups, 2:3, mean), 1L, sd),
  colour = starter_colours[-match("Neutral", names(starter_colours))]
)
mono_nash <- reformat_used_nash(nash_mean(get_nash_array_from_sim(results, "mono")))[order(Deck)]

ggsave(
  "results/mono_tier.png", tier_plot(mono_strengths), width = 8, height = 5
)
ggsave(
  "results/mono_nash_tier.png",
  tier_plot(cbind(mono_nash[, .(component = Deck, mean = Both)],
                  mono_strengths[, c("sd", "colour")])) +
    scale_y_continuous(name = "average Nash weight"),
  width = 8, height = 5
)
