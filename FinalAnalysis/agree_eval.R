library(readr)
library(tidyverse)
library(tidyselect)
library(modeest)

# import data
df_r1 = read_csv("raw_code_des2.csv") %>%
  mutate(a_doi = paste0(tolower(author),"_",doi)) %>% # create unique ident
  mutate(id = str_replace(id," ","_")) %>% # modify id
  mutate(di_sup = ifelse(support == "Not supported", "N","Y")) %>%  # create lable
  mutate_if(is.character,as.factor) # convert string to factor

df_r1 = df_r1[-c(562), ] # remove duplicate study

# Separate by journal
df_msse = df_r1 %>%
  filter(journal == "MSSE")

df_jsams = df_r1 %>%
  filter(journal == "JSAMS")

df_ejss = df_r1 %>%
  filter(journal == "EJSS")

# Function for IRR
irr_grp = function(df,
                   cols1 = c("id","doi","hypo_tested"),
                   spread1 = c("id"),
                   spread2 = "hypo_tested",
                   row1 = "doi"){
  hypo = df %>%
    select(all_of(cols1)) %>%
    spread(all_of(spread1),all_of(spread2)) %>%
    janitor::clean_names() %>%
    column_to_rownames(var = all_of(row1))
  
  #return(hypo)
  return(irr::kappam.fleiss(hypo))
}

icc_grp = function(df,
                   cols1 = c("id","doi","hypo_tested"),
                   spread1 = c("id"),
                   spread2 = "hypo_tested",
                   row1 = "doi"){
  hypo = df %>%
    select(all_of(cols1)) %>%
    spread(all_of(spread1),all_of(spread2)) %>%
    janitor::clean_names() %>%
    column_to_rownames(var = all_of(row1))
  
  #return(hypo)
  return(psych::ICC(data.matrix(hypo))$results$ICC[2])
}

wide_set = function(df,
                    cols1 = c("id","doi","hypo_tested"),
                    spread1 = c("id"),
                    spread2 = "hypo_tested",
                    row1 = "doi"){
  hypo = df %>%
    select(all_of(cols1)) %>%
    spread(all_of(spread1),all_of(spread2)) %>%
    janitor::clean_names() %>%
    column_to_rownames(var = all_of(row1))
  
  return(hypo)
  #return(irr::kappam.fleiss(hypo))
}

agree_na = function(df,
                    cols1 = c("id","doi","hypo_tested"),
                    spread1 = c("id"),
                    spread2 = "hypo_tested",
                    row1 = "doi"){
  hypo = df %>%
    select(all_of(cols1)) %>%
    spread(all_of(spread1),all_of(spread2)) %>%
    janitor::clean_names() %>%
    column_to_rownames(var = all_of(row1))
  dat = hypo[which(rowMeans(!is.na(hypo)) > 0.5), ]
  
  #return(dat)
  #return(irr::kappam.fleiss(hypo))
  
  bool <- apply(dat, 1, function(row) length(unique(row)) == 1)
  
  return(list((sum(bool)/nrow(dat)),
              nrow(dat)))
}


test = wide_set(df_jsams,
                cols1 = c("id","doi","N"),
                spread1 = c("id"),
                spread2 = "N",
                row1 = "doi") 

