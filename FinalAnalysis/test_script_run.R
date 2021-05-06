library(readr)
library(tidyverse)
library(tidyselect)
library(brms)
library(ggdist)
library(distributional)

# Testing out data analysis
# import data ------------
df_all = read_csv("raw_code_des2.csv") %>%
  mutate(a_doi = paste0(tolower(author),"_",doi)) %>% # create unique ident
  mutate(id = str_replace(id," ","_")) %>% # modify id
  mutate(di_sup = ifelse(support == "Not supported", "N","Y")) %>%  # create lable
  mutate_if(is.character,as.factor) %>% # convert string to factor
  # Test my just taking team lead results
  filter(id %in% c("Vanessa_Yingling","Rosie_Twomey","Joe_Warne"))

# Get hypothesis tested set ------------
hyp_tested = df_all %>%
  filter(hypo_tested == "Yes")

# Import brms analysis 1 (positive) ---------
m_test = read_rds("m_test.rds")
# Import brms analysis 2 (prop. of hyp. tested) ------
m_test2 = read_rds("m_test2.rds")

h_test <- hypothesis(m_test, "Intercept > 0.8")
#knitr::kable(h_test$hypothesis, caption = "Hypothesis Test #1")

test_pos = posterior_interval(m_test,
                              prob = .95)
#knitr::kable(test_pos, caption = "Hyp Test #1: 95% Posterior C.I.")

h_test2 <- hypothesis(m_test2, "Intercept > 0.6")

test_pos2 = posterior_interval(m_test2,
                               prob = .95)

