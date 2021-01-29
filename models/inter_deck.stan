data {
  int<lower=0> M;                          // number of matches
  int<lower=0> P;                          // number of players
  int<lower=0> St;                          // number of starter decks
  int<lower=0> Sp;                         // number of specs
  int<lower=1> first_player[M];            // ID number of first player
  int<lower=1> second_player[M];           // ID number of second player
  int<lower=1> first_starter[M];           // ID number of first starter deck
  int<lower=1> second_starter[M];          // ID number of second starter deck
  int<lower=1> first_specs1[M];
  int<lower=1> first_specs2[M];
  int<lower=1> first_specs3[M];
  int<lower=1> second_specs1[M];
  int<lower=1> second_specs2[M];
  int<lower=1> second_specs3[M];
  int<lower=0, upper=1> w[M];              // 1 = first player wins, 0 = second player wins
}
parameters {
  real turn;                               // first-player advantage in log odds
  vector[P] player_std;                        // player skill levels in log odds effect
  vector[P] player_turn_std;                   // player skill level adjustment for going first (penalty if second)
  vector[St] starter_std;                       // deck strengths
  vector[St] starter_turn_std;                  // deck strenth adjustment for going first (penalty if second)
  vector[Sp] spec_std;
  vector[Sp] spec_turn_std;
  matrix[St, Sp] starter_spec_std;
  real lsd_player;                // player skill log spread
  real lsd_player_turn;                // player skill log spread
  real lsd_starter;
  real lsd_starter_turn;
  real lsd_spec;
  real lsd_spec_turn;
  real lsd_starter_spec;
}
transformed parameters{
  vector[M] matchup;                                      // log-odds of a first-player win for each match
  vector[M] first_starter_spec1;
  vector[M] first_starter_spec2;
  vector[M] first_starter_spec3;
  vector[M] second_starter_spec1;
  vector[M] second_starter_spec2;
  vector[M] second_starter_spec3;
  real<lower=0> sd_player = exp(lsd_player);              // player skill spread
  real<lower=0> sd_player_turn = exp(lsd_player_turn);              // player skill spread
  real<lower=0> sd_starter = exp(lsd_starter);
  real<lower=0> sd_starter_turn = exp(lsd_starter_turn);
  real<lower=0> sd_spec = exp(lsd_spec);
  real<lower=0> sd_spec_turn = exp(lsd_spec_turn);
  real<lower=0> sd_starter_spec = exp(lsd_starter_spec);
  vector[P] player = sd_player * player_std;
  vector[P] player_turn = sd_player_turn * player_turn_std;
  vector[St] starter = sd_starter * starter_std;
  vector[St] starter_turn = sd_starter_turn * starter_turn_std;
  vector[Sp] spec = sd_spec * spec_std;
  vector[Sp] spec_turn = sd_spec_turn * spec_turn_std;
  matrix[St, Sp] starter_spec = sd_starter_spec * starter_spec_std;
  for (i in 1:M) {
    first_starter_spec1[i] = starter_spec[first_starter[i], first_specs1[i]];
    first_starter_spec2[i] = starter_spec[first_starter[i], first_specs2[i]];
    first_starter_spec3[i] = starter_spec[first_starter[i], first_specs3[i]];
    second_starter_spec1[i] = starter_spec[second_starter[i], second_specs1[i]];
    second_starter_spec2[i] = starter_spec[second_starter[i], second_specs2[i]];
    second_starter_spec3[i] = starter_spec[second_starter[i], second_specs3[i]];
  }
  matchup = turn +
    player[first_player] + player_turn[first_player] - player[second_player] + player_turn[second_player] +
    starter[first_starter] - starter[second_starter] + starter_turn[first_starter] + starter_turn[second_starter] +
    spec[first_specs1] - spec[second_specs1] + spec_turn[first_specs1] + spec_turn[second_specs1] +
    spec[first_specs2] - spec[second_specs2] + spec_turn[first_specs2] + spec_turn[second_specs2] +
    spec[first_specs3] - spec[second_specs3] + spec_turn[first_specs3] + spec_turn[second_specs3] +
    first_starter_spec1 + first_starter_spec2 + first_starter_spec3 -
    second_starter_spec1 - second_starter_spec2 - second_starter_spec3;
}
model {
  lsd_player ~ std_normal();
  lsd_player_turn ~ std_normal();
  lsd_starter ~ std_normal();
  lsd_starter_turn ~ std_normal();
  lsd_spec ~ std_normal();
  lsd_spec_turn ~ std_normal();
  lsd_starter_spec ~ std_normal();
  turn ~ std_normal();
  player_std ~ std_normal();
  player_turn_std ~ std_normal();
  starter_std ~ std_normal();
  starter_turn_std ~ std_normal();
  spec_std ~ std_normal();
  spec_turn_std ~ std_normal();
  for (i in 1:St) {
    for (j in 1:Sp) {
      starter_spec_std[i, j] ~ std_normal();
    }
  }
  w ~ bernoulli_logit(matchup);
}
generated quantities {
  vector[M] log_lik;
  for (m in 1:M)
  log_lik[m] = bernoulli_logit_lpmf(w[m] | matchup[m]);
}
