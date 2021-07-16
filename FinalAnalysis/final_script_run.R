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
df_all =  read_csv("df_all.csv") %>%
  mutate(support = factor(support,
                          levels = c("Unclear or not stated", 
                                     "Not supported",
                                     "Partial support", 
                                     "Full support"))) %>%
  # Coding error in 20 cases
  mutate(sig_test = ifelse(is.na(sig_test), "No", sig_test))
# Get hypothesis tested set ------------
hyp_tested = df_all %>%
  filter(hypo_tested == "Yes")

# Import brms analysis 1 (positive) ---------
m_final = read_rds("m_final.rds")
# Import brms analysis 2 (prop. of hyp. tested) ------
m_final2 = read_rds("m_final2.rds")

h_test <- hypothesis(m_final, "Intercept > 0.8")
#knitr::kable(h_test$hypothesis, caption = "Hypothesis Test #1")

test_pos = posterior_interval(m_final,
                              prob = .95)
#knitr::kable(test_pos, caption = "Hyp Test #1: 95% Posterior C.I.")

h_test2 <- hypothesis(m_final2, "Intercept > 0.6")

test_pos2 = posterior_interval(m_final2,
                               prob = .95)

# Contingency Table 1 ----

ctab1 = df_all %>% 
  #filter(hypo_tested == "Yes") %>%
  select(effect_size, sig_test, hypo_tested) %>%
  mutate_all(as.factor) %>%
  drop_na() %>%
  epitab::contingency_table(independents=list("Significance Testing" = "sig_test",
                                      "Effect Size Reported" = "effect_size"),
                    outcomes=list("Hypothesis Tested" = "hypo_tested"),
                    crosstab_funcs=list(freq())) #%>%

ct_effsize = table(df_all$effect_size)

binom_eff = binom.test(ct_effsize[2], sum(ct_effsize))
eff_pr = paste0(round(binom_eff$estimate*100,2),"% [",
                   round(binom_eff$conf.int[1]*100,2),", ", 
                   round(binom_eff$conf.int[2]*100,2)
                   ,"] of manuscripts reported information on the effect size.")

ct_sig = table(df_all$sig_test)

binom_sig = binom.test(ct_sig[2], sum(ct_sig))
sig_pr = paste0("Most manuscripts, ",round(binom_sig$estimate*100,2),"% [",
                round(binom_sig$conf.int[1]*100,2),", ", 
                round(binom_sig$conf.int[2]*100,2)
                ,"], reported using significance testing.")




ctab2 = df_all %>% 
  filter(sig_test == "Yes") %>%
  select(effect_size, pval_sig, pval_type) %>%
  mutate_all(as.factor) %>%
  drop_na() %>%
  contingency_table(independents=list("Effect Size Reported" = "effect_size",
                                      "p-value Type" = "pval_type"),
                    outcomes=list("Significant p-value" = "pval_sig"),
                    crosstab_funcs=list(freq())) #%>%
  #neat_table(caption = "Statistics Reported") 

# Prereg Descriptives -----

ctab_prereg = df_all %>% 
  select(clin_trial, rct, animal, prereg) %>%
  mutate_all(as.factor) %>%
  drop_na() %>%
  contingency_table(
  independents = list(
    "Clinical Trial" = "clin_trial",
    "RCT" = "rct",
    "Animal Study" = "animal"
  ),
  outcomes = list("Preregistration" = "prereg"),
  crosstab_funcs=list(freq()))

ct_prereg = table(df_all$prereg)

binom_prereg = binom.test(ct_prereg[[2]],sum(ct_prereg))
prereg_pr = paste0(round(binom_prereg$estimate*100,2),"% [",
                   round(binom_prereg$conf.int[1]*100,2),", ", 
                   round(binom_prereg$conf.int[2]*100,2)
                   ,"] of manuscripts reported preregistration or clinical trial registration information.")

# Sample Size Information -----------

ctab_ss = df_all %>%
  select(journal, n_just, sample_info) %>%
  mutate_all(as.factor) %>%
  drop_na() %>%
  contingency_table(
  independents = list(
    "Sample Size Justification" = "n_just",
    "Sample Size Reported" = "sample_info"
  ),
  outcomes = list("Journal" = "journal"),
  crosstab_funcs = list(freq())
) 

ct_njust = table(df_all$n_just)

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


# problems with med1way not sure if result is accurate
samp_1way = df_all %>%
  select(n, sci_cat) %>%
  mutate(n = as.numeric(n)) %>%
  drop_na() %>%
  WRS2::med1way(formula = n ~ sci_cat)

# checking residuals; aov are fair
library(afex)
aov_1way = df_all %>%
  select(n, sci_cat, doi) %>%
  mutate(n = as.numeric(n)) %>%
  drop_na() %>%
  afex::aov_4(formula = log(n) ~ sci_cat + (1|doi))

df_samp1way = aov_1way$anova_table

med_samps = df_all %>%
  select(n, sci_cat) %>%
  mutate(n = as.numeric(n)) %>%
  drop_na() %>%
  group_by(sci_cat) %>%
  summarize(median_n = round(median(n, na.rm = TRUE) , 0),
            IQR_n = round(IQR(n, na.rm  = TRUE),0),
            max = max(n),
            min = min(n),
            count = n(),
            .groups = 'drop')
library(emmeans)
emm_samps = emmeans::emmeans(aov_1way, ~ sci_cat,
                             type = "response")
emm_plot = plot(emm_samps) +
  scale_x_continuous(trans = "log",
                     breaks = c(20,150,1095)) +
  labs(x = "Estimated Mean Sample Size (log scale)",
       y = "") +
  theme_bw()
  
# Other open Science Practices -----

ct_datstat = table(df_all$data_state)

binom_datstat = binom.test(ct_datstat[[2]],300)
datstat_pr = paste0(round(binom_datstat$estimate*100,2),"% [",
                 round(binom_datstat$conf.int[1]*100,2),", ", 
                 round(binom_datstat$conf.int[2]*100,2)
                 ,"] of manuscripts had a data accessibility statement.")

ct_odat = table(df_all$open_data)

binom_odat = binom.test(ct_odat[[2]],300)
odat_pr = paste0(round(binom_odat$estimate*100,2),"% [",
                 round(binom_odat$conf.int[1]*100,2),", ", 
                 round(binom_odat$conf.int[2]*100,2)
                 ,"] of manuscripts reported some form of data sharing or open data.")


ct_replic = table(df_all$replic)

binom_replic = binom.test(0,300)
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
  ggplot(aes(x = as.numeric(n))) +
  geom_boxplot(fill = "skyblue3",
               alpha = 0.55) +
  labs(title = "Sample Size by Discipline",
       x = "Total Sample Size (log scale)") +
  theme_bw() +
  facet_wrap(~sci_cat,
             scales ="free",
             ncol = 2) +
  scale_x_continuous(trans = "log10") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

# By Journal ----

## Hypothesis Support -----
tab_jhyp = table(df_all$journal,df_all$support)
chisq_support = chisq.test(tab_jhyp)

## Hypothesis Tested
tab_jtest = table(df_all$journal,df_all$hypo_tested)
chisq_jtest = chisq.test(tab_jtest)

## Significance Testing
tab_jsig = table(df_all$journal,df_all$sig_test)
chisq_jsig = chisq.test(tab_jsig)

## Effect Size
tab_jes = table(df_all$journal,df_all$effect_size)
chisq_jes = chisq.test(tab_jes)

## Sample Size Justification
tab_jjust = table(df_all$journal,df_all$n_just)
chisq_jjust = chisq.test(tab_jjust)

# 