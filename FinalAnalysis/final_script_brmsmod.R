library(readr)
library(tidyverse)
library(tidyselect)
library(brms)
library(ggdist)
library(distributional)

# Testing out data analysis
# import data
df_all =  read_csv("df_all.csv")

hyp_tested = df_all %>%
  filter(!is.na(di_sup))
#nrow(hyp_tested)

# ANALYSIS 1: Positive Result Rate ------------
#Set prior
prior_1 = set_prior("beta(17, 3)", class = "b", lb = 0, ub = 1)

#transform data
hyp_pos = hyp_tested %>%
  summarize(
    pos = sum(di_sup == "Y"),
    N = length(di_sup),
    rate = pos/N
  )

#Build model
m_final <- brm(
  pos | trials(N) ~ 0 + Intercept,
  family = binomial(link = "identity"),
  prior = prior_1,
  data = hyp_pos,
  sample_prior = "yes",
  refresh = 0
)
write_rds(m_final, "m_final.rds")
h_test <- hypothesis(m_final, "Intercept > 0.8")
#knitr::kable(h_test$hypothesis, caption = "Hypothesis Test #1")

test_pos = posterior_interval(m_final,
                              prob = .95)
#knitr::kable(test_pos, caption = "Hyp Test #1: 95% Posterior C.I.")

# ANALYSIS 2: Hypothesis Test Rate ------------
#Set prior
prior_2 = set_prior("beta(12, 8)", class = "b", lb = 0, ub = 1)

#Generate test data
hyp_test = df_all %>%
  summarize(
    pos = sum(hypo_tested == "Yes"),
    N = length(hypo_tested),
    rate = pos/N
  )

#Build model
m_final2 <- brm(
  pos | trials(N) ~ 0 + Intercept,
  family = binomial(link = "identity"),
  prior = prior_2,
  data = hyp_test,
  sample_prior = "yes",
  refresh = 0
)
write_rds(m_final2, "m_final2.rds")
h_test2 <- hypothesis(m_final2, "Intercept > 0.6")

test_pos2 = posterior_interval(m_final2,
                               prob = .95)