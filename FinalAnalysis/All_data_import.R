# Import data ----

df_all =  readxl::read_excel("final_data1.xlsx", 
                             range = "A1:AC301", col_types = c("text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "numeric", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "numeric", "text", "text")) %>%
  janitor::clean_names() %>% 
  mutate(a_doi = paste0(tolower(author),"_",doi)) %>% # create unique ident
  mutate(id = str_replace(id," ","_")) %>% # modify id
  mutate(di_sup = ifelse(support == "Not supported", "N","Y"),
         di_sup = ifelse(support == "Unclear or not stated", NA,di_sup)) %>%  # create lable 
  mutate(rct = ifelse(rct == "yes", "Yes", rct)) %>%
  mutate_if(is.character,as.factor)  # convert string to factor

write.csv(df_all, "df_all.csv")