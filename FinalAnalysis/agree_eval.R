library(readr)
library(tidyverse)

# import data
df_r1 = read_csv("raw_code_des2.csv") %>%
  mutate(a_doi = paste0(tolower(author),"_",doi)) %>% # create unique ident
  mutate(id = str_replace(id," ","_")) %>% # modify id
  mutate(di_sup = ifelse(support == "Not supported", "N","Y")) %>%  # create lable
  mutate_if(is.character,as.factor) # convert string to factor

df_r1 = df_r1[-c(562), ] # remove duplicate study

# Seperate by journal
df_msse = df_r1 %>%
  filter(journal == "MSSE")

df_jsams = df_r1 %>%
  filter(journal == "JSAMS")

df_ejss = df_r1 %>%
  filter(journal == "EJSS")

hypo_msse = df_msse %>%
  select(id,doi,hypo_tested) %>%
  spread(id, hypo_tested) %>%
  janitor::clean_names() %>%
  column_to_rownames(var = "doi")

irr::kappam.fleiss(hypo_msse)

hypo_jsams = df_jsams %>%
  select(id,doi,hypo_tested) %>%
  spread(id, hypo_tested) %>%
  janitor::clean_names() %>%
  column_to_rownames(var = "doi")

irr::kappam.fleiss(hypo_jsams)

hypo_ejss = df_ejss %>%
  select(id,doi,hypo_tested) %>%
  spread(id, hypo_tested) %>%
  janitor::clean_names() %>%
  column_to_rownames(var = "doi")

irr::kappam.fleiss(hypo_ejss)
