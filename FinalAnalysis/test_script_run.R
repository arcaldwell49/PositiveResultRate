library(readr)
library(tidyverse)
library(tidyselect)
library(brms)
library(ggdist)
library(distributional)
library(broom)
library(epitab)
library(WRS2)

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

# Contingency Table 1 ----

ctab1 = df_all %>% 
  #filter(hypo_tested == "Yes") %>%
  select(effect_size, sig_test, hypo_tested) %>%
  drop_na() %>%
  contingency_table(independents=list("Significance Testing" = "sig_test",
                                      "Effect Size Reported" = "effect_size"),
                    outcomes=list("Hypothesis Tested" = "hypo_tested"),
                    crosstab_funcs=list(freq())) #%>%

ctab2 = df_all %>% 
  filter(sig_test == "Yes") %>%
  select(effect_size, pval_sig, pval_type) %>%
  drop_na() %>%
  contingency_table(independents=list("Effect Size Reported" = "effect_size",
                                      "p-value Type" = "pval_type"),
                    outcomes=list("Significant p-value" = "pval_sig"),
                    crosstab_funcs=list(freq())) #%>%
  #neat_table(caption = "Statistics Reported") 

# Prereg Descriptives -----

ctab_prereg = contingency_table(
  independents = list(
    "Clinical Trial" = "clin_trial",
    "RCT" = "RCT",
    "Animal" = "animal"
  ),
  outcomes = list("Preregistration" = "prereg"),
  crosstab_funcs = list(freq()),
  data = df_all
) 

ct_prereg = table(df_all$prereg)

binom_prereg = binom.test(ct_prereg[[2]],sum(ct_prereg))
prereg_pr = paste0(round(binom_prereg$estimate*100,2),"% [",
                   round(binom_prereg$conf.int[1]*100,2),", ", 
                   round(binom_prereg$conf.int[2]*100,2)
                   ,"] of manuscripts reported preregistration or clinical trial registration information.")

# Sample Size Information -----------

ctab_ss = df_all %>%
  select(journal, N_just,sample_info) %>%
  drop_na()%>%
  contingency_table(
  independents = list(
    "Sample Size Justification" = "N_just",
    "Sample Size Reported" = "sample_info"
  ),
  outcomes = list("Journal" = "journal"),
  crosstab_funcs = list(freq())
) 

ct_njust = table(df_all$N_just)

binom_njust = binom.test(ct_njust[[2]], sum(ct_njust))
njust_pr = paste0(
  round(binom_njust$estimate * 100, 2),
  "% [",
  round(binom_njust$conf.int[1] * 100, 2),
  ", ",
  round(binom_njust$conf.int[2] * 100, 2)
  ,
  "] of manuscripts reported some form of sample size justification."
)

ct_samp = table(df_all$sample_info)

binom_samp = binom.test(ct_samp[[3]], sum(ct_samp))
samp_pr = paste0(
  round(binom_samp$estimate * 100, 2),
  "% [",
  round(binom_samp$conf.int[1] * 100, 2),
  ", ",
  round(binom_samp$conf.int[2] * 100, 2)
  ,
  "] of manuscripts reported all the required sample size information."
)

# Sample Size Analysis ------

samp_1way = WRS2::med1way(N ~ sci_cat,
                        df_all)

df_samp1way = data.frame(
  `Analysis` = "Heteroscedastic one-way ANOVA for medians",
  `F-value` = samp_1way$test,
  `p-value` = samp_1way$p.value
)

# Other open Science Practices -----

ct_odat = table(df_all$open_data)

binom_odat = binom.test(ct_odat[[2]],300)
odat_pr = paste0(round(binom_odat$estimate*100,2),"% [",
                 round(binom_odat$conf.int[1]*100,2),", ", 
                 round(binom_odat$conf.int[2]*100,2)
                 ,"] of manuscripts reported some form of data sharing or open data.")


ct_replic = table(df_all$replic)

binom_replic = binom.test(ct_replic[[2]],300)
replic_pr = paste0(
  round(binom_replic$estimate * 100, 2),
  "% [",
  round(binom_replic$conf.int[1] * 100, 2),
  ", ",
  round(binom_replic$conf.int[2] * 100, 2)
  ,
  "] of manuscripts explicitly stated they were replicating a previous study."
)

p_n = df_all %>%
  ggplot(aes(x = N)) +
  geom_boxplot(fill = "skyblue3",
               alpha = 0.55) +
  labs(title = "Sample Size by Discipline",
       x = "Sample Size (log10 scale)") +
  theme_bw() +
  facet_wrap(~sci_cat,
             scales ="free",
             ncol = 2) +
  scale_x_continuous(trans = "log10") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
