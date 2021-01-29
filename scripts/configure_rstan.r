options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)
# Sys.setenv(LOCAL_CPPFLAGS = '-march=native') # Makes models with vector parameters crash
