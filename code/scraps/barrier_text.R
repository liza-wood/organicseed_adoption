df <- readRDS("~/Box/organicseed_adoption/df_multilevel.rds")

text_barrier <- df %>% 
  filter(!(barrier_text %in% c("", "-2", "-1", "-3"))) %>% 
  select(survey_year, contains('barrier_')) %>% 
  filter(barrier_text != "Skipped due to branching")

text_barrier$barrier_text[text_barrier$survey_year == 2011]

text_barrier$barrier_text[text_barrier$survey_year == 2016]

text_barrier$barrier_text[text_barrier$survey_year == 2021]

