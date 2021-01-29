data {
  int<lower=0> M;                          // number of matches
  int<lower=0> P;                          // number of players
  int<lower=0> D;                          // number of decks
  int<lower=1> first_player[M];            // ID number of first player
  int<lower=1> second_player[M];           // ID number of second player
  int<lower=1> first_deck[M];           // ID number of first deck
  int<lower=1> second_deck[M];          // ID number of second deck
  int<lower=0, upper=1> w[M];              // 1 = first player wins, 0 = second player wins
}
parameters {
  real turn;                               // first-player advantage in log odds
  vector[P] player_std;                        // player skill levels in log odds effect
  vector[P] player_turn_std;                   // player skill level adjustment for going first (penalty if second)
  vector[D] deck_std;                       // deck strengths
  vector[D] deck_turn_std;                  // deck strenth adjustment for going first (penalty if second)
  real lsd_player;                // player skill log spread
  real lsd_player_turn;                // player skill log spread
  real lsd_deck;
  real lsd_deck_turn;
}
transformed parameters{
  vector[M] matchup;                                      // log-odds of a first-player win for each match
  real<lower=0> sd_player = exp(lsd_player);              // player skill spread
  real<lower=0> sd_player_turn = exp(lsd_player_turn);              // player skill spread
  real<lower=0> sd_deck = exp(lsd_deck);
  real<lower=0> sd_deck_turn = exp(lsd_deck_turn);
  vector[P] player = sd_player * player_std;
  vector[P] player_turn = sd_player_turn * player_turn_std;
  vector[D] deck_turn = sd_deck_turn * deck_turn_std;
  vector[D] deck = sd_deck * deck_std;
  matchup = turn + player[first_player] + player_turn[first_player] - player[second_player] + player_turn[second_player] + deck[first_deck] - deck[second_deck] + deck_turn[first_deck] + deck_turn[second_deck];
}
model {
  lsd_player ~ std_normal();
  lsd_player_turn ~ std_normal();
  lsd_deck ~ std_normal();
  lsd_deck_turn ~ std_normal();
  turn ~ std_normal();
  player_std ~ std_normal();
  player_turn_std ~ std_normal();
  deck_std ~ std_normal();
  deck_turn_std ~ std_normal();
  w ~ bernoulli_logit(matchup);
}
generated quantities {
  vector[M] log_lik;
  for (m in 1:M)
  log_lik[m] = bernoulli_logit_lpmf(w[m] | matchup[m]);
}
