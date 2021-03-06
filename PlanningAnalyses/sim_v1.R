library(tidyverse)
library(broom)
library(brms)



gen_data = function(run,n,prop){
  df = data.frame(run = run,
                  pos = rbinom(1, n, prop),
                  N = n) %>%
    mutate(rate = pos/N)
  return(df)
}

initial_form = function(n = 150,
                        prop = .85,
                        sim_prior = set_prior("beta(17, 3)", class = "b", lb = 0, ub = 1)){
  init_df = data.frame(run = 1,
                       pos = rbinom(1, n, prop),
                       N = n) %>%
    mutate(rate = pos/N)
  fit <- brm(
    pos | trials(N) ~ 0 + Intercept,
    family = binomial(link = "identity"),
    prior = sim_prior,
    data = init_df)
  return(fit)
}

set.seed(08202020)
nsims = 1000
ci = .95
hyp_test = "Intercept > 0.8"
fit = initial_form(n = 150,
                   prop = .85,
                   sim_prior = set_prior("beta(17, 3)", class = "b", lb = 0, ub = 1))

bin_sims = tibble(run = 1:nsims) %>%
  mutate(d = map(run, gen_data, n = 150, prop = .85)) 

bin_sims = bin_sims %>%
  mutate(fit  = map(d, ~update(fit, newdata = .x, refresh = 0)))

bin_est = bin_sims %>%
  mutate(test = map(fit,tidy,prob=ci)) %>%
  unnest(test) %>%
  filter(term == "b_Intercept") %>%
  select(-d,-fit) %>%
  mutate(width = upper-lower)

bin_hyp = bin_sims %>%
  mutate(hyp = map(fit,hypothesis,hyp_test)) %>%
  select(run,hyp)

hyp_df = data.frame(1,2,3,4,5,6,7,8)
colnames(hyp_df) = colnames(bin_hyp$hyp[[1]]$hypothesis)
hyp_df = hyp_df[FALSE,]

for (i in 1:nrow(bin_hyp)){
  hyp_df = rbind(hyp_df, as.data.frame(bin_hyp$hyp[[i]]$hypothesis))
  
}


