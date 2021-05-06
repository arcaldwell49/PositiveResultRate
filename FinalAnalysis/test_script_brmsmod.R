library(readr)
library(tidyverse)
library(tidyselect)
library(brms)
library(ggdist)
library(distributional)

# Testing out data analysis
# import data
df_all = read_csv("raw_code_des2.csv") %>%
  mutate(a_doi = paste0(tolower(author),"_",doi)) %>% # create unique ident
  mutate(id = str_replace(id," ","_")) %>% # modify id
  mutate(di_sup = ifelse(support == "Not supported", "N","Y")) %>%  # create lable
  mutate_if(is.character,as.factor) %>% # convert string to factor
  # Test my just taking team lead results
  filter(id %in% c("Vanessa_Yingling","Rosie_Twomey","Joe_Warne"))

hyp_tested = df_all %>%
  filter(hypo_tested == "Yes")
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
m_test <- brm(
  pos | trials(N) ~ 0 + Intercept,
  family = binomial(link = "identity"),
  prior = prior_1,
  data = hyp_pos,
  sample_prior = "yes",
  refresh = 0
)
write_rds(m_test, "m_test.rds")
h_test <- hypothesis(m_test, "Intercept > 0.8")
#knitr::kable(h_test$hypothesis, caption = "Hypothesis Test #1")

test_pos = posterior_interval(m_test,
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
m_test2 <- brm(
  pos | trials(N) ~ 0 + Intercept,
  family = binomial(link = "identity"),
  prior = prior_2,
  data = hyp_test,
  sample_prior = "yes",
  refresh = 0
)
write_rds(m_test2, "m_test2.rds")
h_test2 <- hypothesis(m_test2, "Intercept > 0.6")

test_pos2 = posterior_interval(m_test2,
                               prob = .95)