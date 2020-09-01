# Import data
library(readxl)
library(tidyverse)
library(irr)
pilotv1 <- read_excel("pilotv1.xlsx") %>%
  mutate(a_doi = paste0(tolower(author),"_",doi)) %>%
  mutate(id = str_replace(id," ","_")) %>%
  mutate(di_sup = ifelse(support == "Not supported", "N","Y")) %>% 
  mutate_if(is.character,as.factor)


hypo_wide = pilotv1 %>%
  select(id,a_doi,hypo_tested) %>%
  spread(id, hypo_tested) %>%
  janitor::clean_names() %>%
  select(-na) %>%
  column_to_rownames(var = "a_doi")

irr::kappam.fleiss(hypo_wide)

support_wide = pilotv1 %>%
  select(id,a_doi,di_sup) %>%
  spread(id, di_sup) %>%
  janitor::clean_names() %>%
  select(-na) %>%
  column_to_rownames(var = "a_doi") %>%
  drop_na()

irr::kappam.fleiss(support_wide)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

df_pilot = pilotv1 %>%
  select(-id,-journal,-author,-doi) %>%
  filter(a_doi != "NA_NA") %>%
  group_by(a_doi) %>%
  summarize(across(everything(), Mode),
            .groups = 'drop')

df_test = data.frame(a = c(rep("a",7),rep("b",8)),
                     b = c(rep("a",7),rep("b",8)),
                     c = c(rep("a",7),rep("b",7),"a"))
irr::kappam.fleiss(df_test)
