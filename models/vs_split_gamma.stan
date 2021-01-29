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
  vector[P] player_std;                    // player skill levels in log odds effect
  matrix[St, St] starter_vs_starter_std;   // starter vs. starter effect in log odds
  matrix[St, Sp] starter_vs_spec_std;      // starter vs. spec effect in log odds
  matrix[Sp, St] spec_vs_starter_std;      // spec vs. starter effect in log odds
  matrix[Sp, Sp] spec_vs_spec_std;         // spec vs. spec effect in log odds
  real<lower=0> var_player;                          // player skill spread
  real<lower=0> var_starter_vs_starter;              // starter vs. starter effect spread
  real<lower=0> var_starter_vs_spec;                 // starter vs. spec effect (or vice versa) spread
  real<lower=0> var_spec_vs_spec;                    // spec vs. spec effect spread
}
transformed parameters {
  vector[M] matchup;                       // log-odds of a first-player win for each match
  vector[P] player = sqrt(var_player) * player_std;
  matrix[St, St] starter_vs_starter = sqrt(var_starter_vs_starter) * starter_vs_starter_std;
  matrix[St, Sp] starter_vs_spec = sqrt(var_starter_vs_spec) * starter_vs_spec_std;
  matrix[Sp, St] spec_vs_starter = sqrt(var_starter_vs_spec) * spec_vs_starter_std;
  matrix[Sp, Sp] spec_vs_spec = sqrt(var_spec_vs_spec) * spec_vs_spec_std;
  for (i in 1:M) {
  matchup[i] = player[first_player[i]] - player[second_player[i]] +
    starter_vs_starter[first_starter[i], second_starter[i]] +
    starter_vs_spec[first_starter[i], second_specs1[i]] +
    starter_vs_spec[first_starter[i], second_specs2[i]] +
    starter_vs_spec[first_starter[i], second_specs3[i]] +
    spec_vs_starter[first_specs1[i], second_starter[i]] +
    spec_vs_starter[first_specs2[i], second_starter[i]] +
    spec_vs_starter[first_specs3[i], second_starter[i]] +
    spec_vs_spec[first_specs1[i], second_specs1[i]] +
    spec_vs_spec[first_specs1[i], second_specs2[i]] +
    spec_vs_spec[first_specs1[i], second_specs3[i]] +
    spec_vs_spec[first_specs2[i], second_specs1[i]] +
    spec_vs_spec[first_specs2[i], second_specs2[i]] +
    spec_vs_spec[first_specs2[i], second_specs3[i]] +
    spec_vs_spec[first_specs3[i], second_specs1[i]] +
    spec_vs_spec[first_specs3[i], second_specs2[i]] +
    spec_vs_spec[first_specs3[i], second_specs3[i]];
  }
}
model {
  // Total variance distribution is Gamma(64, 8), shape split into additive components
  var_player ~ gamma(16, 8);
  var_starter_vs_starter ~ gamma(2, 8);
  var_starter_vs_spec ~ gamma(2, 8);
  var_spec_vs_spec ~ gamma(2, 8);
  player_std ~ std_normal();
  for (i in 1:St) {
  starter_vs_starter_std[i, ] ~ std_normal();
  starter_vs_spec_std[i, ] ~ std_normal();
  }
  for (i in 1:Sp) {
  spec_vs_starter_std[i, ] ~ std_normal();
  spec_vs_spec_std[i, ] ~ std_normal();
  }
  w ~ bernoulli_logit(matchup);
}
generated quantities {
  vector[M] log_lik;
  for (m in 1:M)
  log_lik[m] = bernoulli_logit_lpmf(w[m] | matchup[m]);
}
