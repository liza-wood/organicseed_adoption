df <- readRDS("~/Box/organicseed_adoption/df_multilevel.rds")

text_barrier <- df %>% 
  filter(!(barrier_text %in% c("", "-2", "-1"))) %>% 
  select(survey_year, contains('barrier_'))

text_barrier$barrier_text


