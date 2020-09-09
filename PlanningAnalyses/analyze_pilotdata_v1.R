# Import data
library(readxl)
library(tidyverse)
library(irr)
pilotv1 <- read_excel("pilotv2.xlsx") %>%
  mutate(a_doi = paste0(tolower(author),"_",doi)) %>%
  mutate(id = str_replace(id," ","_")) %>%
  mutate(di_sup = ifelse(support == "Not supported", "N","Y")) %>% 
  mutate_if(is.character,as.factor)

# put data in wide format for agreement checks
hypo_wide = pilotv1 %>%
  select(id,a_doi,hypo_tested) %>%
  spread(id, hypo_tested) %>%
  janitor::clean_names() %>%
  #select(-na) %>%
  column_to_rownames(var = "a_doi")

irr::kappam.fleiss(hypo_wide)

support_wide = pilotv1 %>%
  select(id,a_doi,di_sup) %>%
  spread(id, di_sup) %>%
  janitor::clean_names() %>%
  # select(-na) %>%
  column_to_rownames(var = "a_doi") %>%
  drop_na()

irr::kappam.fleiss(support_wide)

support_wide2 = pilotv1 %>%
  select(id,a_doi,support) %>%
  spread(id, support) %>%
  janitor::clean_names() %>%
  # select(-na) %>%
  column_to_rownames(var = "a_doi") %>%
  drop_na()

irr::kappam.fleiss(support_wide2)

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

write.csv(df_pilot,"df_pilot.csv")
