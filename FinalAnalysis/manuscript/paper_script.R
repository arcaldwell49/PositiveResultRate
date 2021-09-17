library(readr)
library(tidyverse)
library(tidyselect)
library(brms)
library(ggdist)
library(distributional)
library(broom)
library(epitab)
library(WRS2)
#library(extrafont)
library(ggpubr)

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
h_ci = fixef(m_final)
test_pos = posterior_interval(m_final,
                              prob = .95)
#knitr::kable(test_pos, caption = "Hyp Test #1: 95% Posterior C.I.")

h_test2 <- hypothesis(m_final2, "Intercept > 0.6")
h_ci2 = fixef(m_final2)
test_pos2 = posterior_interval(m_final2,
                               prob = .95)

# Main Figures

dat_mfinal = posterior_samples(m_final, "b") %>%
  mutate(Test = "Positive Result Rate")
dat_mfinal2 = posterior_samples(m_final2, "b") %>%
  mutate(Test = "Rate of Hypothesis Tests")

df_mfinal = rbind(dat_mfinal, dat_mfinal2)

p_f1a = df_mfinal %>%
  ggplot(aes(x=b_Intercept,
             fill = Test)) +
  stat_halfeye(alpha = .75) +
  #scale_fill_brewer(direction = -1, na.translate = FALSE) +
  #scale_fill_viridis_d() +
  #scale_fill_brewer(direction = -1, na.translate = FALSE) +
  labs(fill = "Interval",
       x = "Probability",
       y = "") +
  theme_bw() +
  facet_wrap(~Test) +
  scale_fill_manual(values =c("lightgreen","skyblue2")) +
  theme(legend.position = "none",
        #axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size = 14,
                            face = "bold"))

fig_1b = df_all %>%
  select(support) %>%
  drop_na() %>%
  ggplot(aes(support,
             fill = support)) +
  geom_bar(aes(y = (..count..) / sum(..count..)),
           color = "black") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,.5),
                     breaks = c(0,.1,.2,.3,.4,.5),
                     expand = c(0,0)) +
  labs(x = "Level of Hypothesis Support",
       y = "Relative Frequency",
       fill = "Hypothesis Support") +
  theme_classic() +
  scale_fill_viridis_d(option = "E") +
  theme(legend.position = "none")

fig_1c = df_all %>%
  select(hypo_tested) %>%
  drop_na() %>%
  ggplot(aes(hypo_tested,
             fill = hypo_tested)) +
  geom_bar(aes(y = (..count..) / sum(..count..)),
           color = "black") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,.75),
                     breaks = c(0,.25,.5,.75),
                     expand = c(0,0)) +
  labs(x = "Hypothesis Tested",
       y = "Relative Frequency",
       fill = "") +
  theme_classic() +
  scale_fill_viridis_d(option = "E") +
  theme(legend.position = "none")
fig1 = ggarrange(p_f1a,
                 fig_1b,
                 hjust = -0.2,
                 ncol = 1,
                 labels = "AUTO")
ggsave("figure1.jpg",
       fig1,
       #compression = "lzw",
       height = 8,
       width = 7,
       dpi = 800)

tab_sup = df_all %>%
  select(hypo_tested, support) %>%
  drop_na() %>%
  group_by(support) %>%
  summarize(n = n())
tots_sup = sum(tab_sup$n)
tab_sup$perc = tab_sup$n/tots_sup*100
  

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
eff_pr = paste0(round(binom_eff$estimate*100,2),"\\% [",
                   round(binom_eff$conf.int[1]*100,2),", ", 
                   round(binom_eff$conf.int[2]*100,2)
                   ,"]")

ct_sig = table(df_all$sig_test)

binom_sig = binom.test(ct_sig[2], sum(ct_sig))
sig_pr = paste0(round(binom_sig$estimate*100,2),"\\% [",
                round(binom_sig$conf.int[1]*100,2),", ", 
                round(binom_sig$conf.int[2]*100,2)
                ,"]")

ct_sig2 = table(subset(df_all, hypo_tested == "No")$sig_test)

binom_sig2 = binom.test(ct_sig2[2], sum(ct_sig2))
sig_pr2 = paste0(round(binom_sig2$estimate*100,2),"\\% [",
                round(binom_sig2$conf.int[1]*100,2),", ", 
                round(binom_sig2$conf.int[2]*100,2)
                ,"]")

ct_sig3 = table(subset(df_all, hypo_tested == "Yes")$sig_test)

binom_sig3 = binom.test(ct_sig3[2], sum(ct_sig3))
sig_pr3 = paste0(round(binom_sig3$estimate*100,2),"\\% [",
                 round(binom_sig3$conf.int[1]*100,2),", ", 
                 round(binom_sig3$conf.int[2]*100,2)
                 ,"]")


ct_ptype = table(df_all$pval_type)
binom_ptype = binom.test(ct_ptype[[2]],sum(ct_ptype))
ptype_pr = paste0(round(binom_ptype$estimate*100,2),"\\% [",
                  round(binom_ptype$conf.int[1]*100,2),", ", 
                  round(binom_ptype$conf.int[2]*100,2)
                  ,"] of manuscripts reported exact p-values for all results (p = .045) versus only relative p-values (p < .05)")

ct_ptype2 = table(df_all$pval_type)

binom_ptype2 = binom.test(ct_ptype2[[2]]+ct_ptype2[[1]],sum(ct_ptype2))
ptype_pr2 = paste0(round(binom_ptype2$estimate*100,2),"\\% [",
                   round(binom_ptype2$conf.int[1]*100,2),", ", 
                   round(binom_ptype2$conf.int[2]*100,2)
                   ,"] of manuscripts reported at least *some* exact p-values (e.g., p = .045) versus relative p-values (e.g., p < .05)")


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
prereg_pr = paste0(round(binom_prereg$estimate*100,2),"\\% [",
                   round(binom_prereg$conf.int[1]*100,2),", ", 
                   round(binom_prereg$conf.int[2]*100,2)
                   ,"] of manuscripts reporting preregistration or clinical trial registration information")

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
  "]"
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
  "] of manuscripts reported all the required sample size information (total and group sample sizes)."
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


library(emmeans)
emm_samps = emmeans::emmeans(aov_1way, ~ sci_cat,
                             type = "response")

  
# Other open Science Practices -----

ct_datstat = table(df_all$data_state)

binom_datstat = binom.test(ct_datstat[[2]],300)
datstat_pr = paste0(round(binom_datstat$estimate*100,2),"% [",
                 round(binom_datstat$conf.int[1]*100,2),", ", 
                 round(binom_datstat$conf.int[2]*100,2)
                 ,"] of manuscripts had a data accessibility statement")

ct_odat = table(df_all$open_data)

binom_odat = binom.test(ct_odat[[2]],300)
odat_pr = paste0(round(binom_odat$estimate*100,2),"% [",
                 round(binom_odat$conf.int[1]*100,2),", ", 
                 round(binom_odat$conf.int[2]*100,2)
                 ,"] of manuscripts reported some form of data sharing or open data")


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

## Hypothesis Tested -----
tab_jtest = table(df_all$journal,df_all$hypo_tested)
chisq_jtest = chisq.test(tab_jtest)

## Significance Testing -----
tab_jsig = table(df_all$journal,df_all$sig_test)
chisq_jsig = chisq.test(tab_jsig)

## Effect Size -----
tab_jes = table(df_all$journal,df_all$effect_size)
chisq_jes = chisq.test(tab_jes)

## Sample Size Justification -----
tab_jjust = table(df_all$journal,df_all$n_just)
chisq_jjust = chisq.test(tab_jjust)

# Clinical Trial breakdown ------
df_clin = subset(df_all, clin_trial == "Yes")

## Hypothesis Support (di)
tab_clindisup = table(df_clin$di_sup)
binom_clindisup = binom.test(tab_clindisup[2], sum(tab_clindisup),
                             p = .8)
tab_clinsup = table(df_clin$support)

## Hypothesis Tested
tab_clinhypo = table(df_clin$hypo_tested)
binom_clinhypo = binom.test(tab_clinhypo[2], sum(tab_clinhypo),
                             p = .6)

## Sample Size Just ---------
tab_clinjust = table(df_clin$n_just)
binom_clinjust = binom.test(tab_clinjust[2], sum(tab_clinjust))

## Pregreg -------

tab_clinreg = table(df_clin$prereg)

binom_clinreg = binom.test(tab_clinreg[2], sum(tab_clinreg))

### by journal -----
tab_clinregj = table(df_clin$prereg, df_clin$journal)


# RCT breakdown ---------

df_rct = subset(df_all, rct == "Yes")

## Hypothesis Support (di)
tab_rctdisup = table(df_rct$di_sup)
binom_rctdisup = binom.test(tab_rctdisup[2], sum(tab_rctdisup),
                             p = .8)
tab_rctsup = table(df_rct$support)

## Hypothesis Tested
tab_rcthypo = table(df_rct$hypo_tested)
binom_rcthypo = binom.test(tab_rcthypo[2], sum(tab_rcthypo),
                            p = .6)

## Sample Size Just ---------
tab_rctjust = table(df_rct$n_just)
binom_rctjust = binom.test(tab_rctjust[2], sum(tab_rctjust))

tab_rctreg = table(df_rct$prereg)

binom_rctreg = binom.test(tab_rctreg[2], sum(tab_rctreg))

## Sample Size Info -------
# All studies reported sample size information
#tab_clinssj = table(df_clin$sample_info)
#binom_clinssj = binom.test(tab_clinssj)


# Breakdown by Discipline ----------

## Hypothesis Tested ---------

tab_dissupp = table(df_all$sci_cat, df_all$support)
chisq_dissupp = chisq.test(tab_dissupp)

tab_dishypop = table(df_all$sci_cat, df_all$hypo_tested)
chisq_dishypop = chisq.test(tab_dishypop)

p_dissup = df_all %>%
  group_by(support, sci_cat) %>%
  summarize(count = n(),
            .groups = 'drop') %>%
  filter(!is.na(support)) %>%
  ggplot( aes(fill=support, y=count, x=sci_cat)) + 
  geom_bar(position="fill", stat="identity",
           color = "black")+
  scale_y_continuous(labels = scales::percent) +
  labs(x = "",
       y = "Relative Frequency",
       fill = "") +
  theme_classic() +
  scale_fill_viridis_d(option = "E") +
  theme(legend.position = "bottom") +
  coord_flip()+
  theme(text = element_text(face = "bold"))

p_dishypo = df_all %>%
  group_by(hypo_tested, sci_cat) %>%
  summarize(count = n(),
            .groups = 'drop') %>%
  filter(!is.na(hypo_tested)) %>%
  ggplot( aes(fill=hypo_tested, y=count, x=sci_cat)) + 
  geom_bar(position="fill", stat="identity",
           color = "black")+
  scale_y_continuous(labels = scales::percent) +
  labs(x = "",
       y = "Relative Frequency",
       fill = "Hypothesis Tested") +
  theme_classic() +
  scale_fill_viridis_d(option = "E") +
  theme(legend.position = "bottom") +
  coord_flip() +
  theme(text = element_text(face = "bold"))

emm_plot = plot(emm_samps) +
  scale_x_continuous(trans = "log",
                     breaks = c(10,15,20,30,40,50,65,80,110,150,1095)) +
  labs(x = "Estimated Mean Sample Size (log scale)",
       y = "") +
  theme_bw() +
  theme(text = element_text(face = "bold"))

fig3 = ggarrange(p_dishypo,p_dissup,emm_plot,
                 ncol = 1,
                 labels = "AUTO")

ggsave("figure3.jpg",
       fig3,
       dpi = 1000,
       width = 9,
       height = 11)

# Main Figures --------------

p_2a = df_all %>%
  group_by(journal, support) %>%
  summarize(count = n(),
            .groups = 'drop') %>%
  filter(!is.na(support)) %>%
  ggplot( aes(fill=support, y=count, x=journal)) + 
  geom_bar(position="fill", stat="identity",
           color = "black")+
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Journal",
       y = "Relative Frequency",
       fill = "Hypothesis Support") +
  theme_classic() +
  scale_fill_viridis_d(option = "E") +
  theme(legend.position = "top",
        text = element_text(face = "bold"))

p_2b = df_all %>%
  group_by(journal, hypo_tested) %>%
  summarize(count = n(),
            .groups = 'drop') %>%
  filter(!is.na(hypo_tested)) %>%
  ggplot( aes(fill=hypo_tested, y=count, x=journal)) + 
  geom_bar(position="fill", stat="identity",
           color = "black")+
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Journal",
       y = "Relative Frequency",
       fill = "Hypothesis Tested") +
  theme_classic() +
  scale_fill_viridis_d(option = "E") +
  theme(legend.position = "top",
        text = element_text(face = "bold"))

p_2c = df_all %>%
  group_by(journal, effect_size) %>%
  summarize(count = n(),
            .groups = 'drop') %>%
  filter(!is.na(effect_size)) %>%
  ggplot( aes(fill=effect_size, y=count, x=journal)) + 
  geom_bar(position="fill", stat="identity",
           color = "black")+
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Journal",
       y = "Relative Frequency",
       fill = "Effect Size Reported") +
  theme_classic() +
  scale_fill_viridis_d(option = "E") +
  theme(legend.position = "top",
        text = element_text(face = "bold"))

p_2d = df_all %>%
  group_by(journal, n_just) %>%
  summarize(count = n(),
            .groups = 'drop') %>%
  filter(!is.na(n_just)) %>%
  ggplot( aes(fill=n_just, y=count, x=journal)) + 
  geom_bar(position="fill", stat="identity",
           color = "black")+
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Journal",
       y = "Relative Frequency",
       fill = "Sample Size Justification") +
  theme_classic() +
  scale_fill_viridis_d(option = "E") +
  theme(legend.position = "top",
        text = element_text(face = "bold"))


fig_2  = ggarrange(p_2b, p_2a, p_2c, p_2d,
                   ncol = 1,
                   labels = "AUTO")

ggsave("figure2.jpg",
       fig_2,
       dpi = 1000,
       width = 8,
       height = 11)
