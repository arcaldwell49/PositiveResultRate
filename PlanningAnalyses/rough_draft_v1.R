# Get necessary packages
library(brms)
library(tidyverse)
library(here)

pd <- tibble(
  x = seq(0, 1, by = .01),
  Prior = dbeta(x, 12, 8)
)

ggplot(pd, aes(x, Prior)) +
  geom_line() +
  #coord_cartesian(xlim = 0:1, expand = 0) +
  labs(y = "Density", x = bquote(theta))



alpha_single = 17
beta_single = 3
ggplot(data.frame(x = c(0, 1)), aes(x)) +
  stat_function(fun = dbeta,
                args = list(shape1 = alpha_single, shape2 = beta_single)) +
  stat_function(
    data = data.frame(x = c(0, 1)),
    aes(x),
    fun = dbeta,
    args = list(shape1 = alpha_single, shape2 = beta_single),
    geom = "area",
    fill = "green",
    alpha = .25,
    inherit.aes = F
  ) +
  labs(y = "Density", x = bquote(theta)) +
  theme_bw() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0))

pd <- tibble(
  x = seq(0, 1, by = .01),
  Prior = dbeta(x, 30, 20)
)

ggplot(pd, aes(x, Prior)) +
  geom_line() +
  #coord_cartesian(xlim = 0:1, expand = 0) +
  labs(y = "Density", x = bquote(theta))


prior_1 = set_prior("beta(17, 3)", class = "b", lb = 0, ub = 1)
set.seed(08212020)
test_df = data.frame(run = 1,
                pos = rbinom(1, 150, .85),
                N = rep(150, 1)) %>%
  mutate(rate = pos/N)


m_test <- brm(
  pos | trials(N) ~ 0 + Intercept,
  family = binomial(link = "identity"),
  prior = prior_1,
  data = test_df,
  sample_prior = TRUE,
  iter = 1e4,
  cores = 4,
  refresh = 0
)

samples <- posterior_samples(m_test, "b")

gather(samples, Type, value) %>% 
  ggplot(aes(value, fill = Type)) +
  geom_density(alpha = .25) +
  geom_vline(xintercept = .8) +
  labs(y = "Density", x = bquote(theta)) +
  theme_bw() +
  scale_y_continuous(expand = c(0,.05)) +
  scale_x_continuous(limits = c(0,1),
                     breaks = seq(0.1,.9,.1),
                     expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 40)) +
  scale_fill_viridis_d(labels = c("Posterior","Prior"),
                       option = "D")

h_test <- hypothesis(m_test, "intercept < 0.8")
#print(h_test)

test_pos = posterior_interval(m_test,
                              
                   prob = .95)

## set the name

test_pos


## Set --------------------------- 

bf_binom_power = function(nsims = 100,
                          n = 150,
                          prop = .85,
                          ci = .95,
                          sim_prior = set_prior("beta(17, 3)", class = "b", lb = 0, ub = 1),
                          hyp_test = "Intercept > 0.8"){
  
  df_sims = data.frame(run = 1:nsims,
                       pos = rbinom(nsims, n, prop),
                       N = rep(n, nsims)) %>%
    mutate(rate = pos / N)
  
  df_res = data.frame(BF = rep(NA, nsims),
                      low_pos = rep(NA, nsims),
                      high_pos = rep(NA, nsims))
  
  for (i in 1:nrow(df_res)) {
    df_run = df_sims[i,]
    
    h_test <- hypothesis(m_test, "intercept < 0.8")
    #print(h_test)
    
    m_test <- brm(
      pos | trials(N) ~ 0 + Intercept,
      family = binomial(link = "identity"),
      prior = sim_prior,
      data = df_run,
      sample_prior = TRUE,
      iter = 1e4,
      cores = 4
    )
    
    post_run = posterior_interval(m_test,
                       pars = "b_Intercept",
                       prob = .98)
    
    h_run <- hypothesis(m_test, hyp_test)
    df_res$BF = h_run$hypothesis$Evid.Ratio
    df_res$low_pos = post_run[1]
    df_res$high_pos = post_run[2]
    
  }
  
  return(list(df_res = df_res,
              df_sims = df_sims))
  
  
}

test_sim = bf_binom_power()
