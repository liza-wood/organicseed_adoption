library(dplyr)
library(stringr)
# 0. Read in data just like for data summary ----
df.full <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds") %>% 
  filter(Region != "International", maincrop != "Unknown") %>% 
  select(OGseed_percent_maincrop,
         #barrier_buyerReq,
         barrier_insuffQuantity,
         barrier_lackdesiredTraits,
         barrier_availability, 
         OG_value,
         CertifierRequestn,
         maincrop, survey_year, Region, 
         TotalAcreage, cropcat_n, 
         VegiAcreage, FieldAcreage, ForageAcreage,
         noncommercial_seedsource, data_coll_method) %>% 
  na.omit() %>% 
  mutate(OGseed_percent_maincrop_prop = OGseed_percent_maincrop/100) %>% 
  mutate(survey_year = case_when(
    survey_year == 2011 ~ 2010,
    survey_year == 2016 ~ 2015,
    survey_year == 2021 ~ 2020,
    T ~ survey_year
  ))

# 1. Compare convenience versus random ----
table(df.full$data_coll_method, df.full$survey_year)

## Counts ----

## Counts comparison year only
table_a1 <- df.full %>% 
  group_by(data_coll_method, survey_year) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(survey_year) %>% 
  mutate(total = sum(n)) %>% 
  tidyr::pivot_wider(names_from = data_coll_method, values_from = n) %>% 
  mutate(conv_perc = paste(round(100*(Convenience/total)),"%"),
         ran_perc = paste(round(100*(Random/total)),"%"))
table_a1

## Table A1 ----
table_a1$Convenience <- paste0(table_a1$Convenience, " (",
                               table_a1$conv_perc, ")")
table_a1$Random <- paste0(table_a1$Random, " (",
                          table_a1$ran_perc, ")")
table_a1 <- select(table_a1, -c(conv_perc, ran_perc))
write.csv(table_a1, "~/Documents/Davis/R-Projects/organicseed_adoption/tables/append_samplet1.csv", row.names = F)

## Table A2 ----
table_a2 <- df.full %>% 
  group_by(data_coll_method, survey_year, Region, maincrop) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(data_coll_method, survey_year) %>% 
  mutate(total = sum(n),
         prop = round(100*(n/total)),
         n = paste0(n, " (", prop, "%)")) %>% 
  select(-c(total, prop)) %>% 
  pivot_wider(names_from = data_coll_method, values_from = n) %>% 
  filter(survey_year != 2010) %>% 
  ungroup()
table_a2_2015 <- filter(table_a2, survey_year == 2015) %>% select(-survey_year)
table_a2_2020 <- filter(table_a2, survey_year == 2020)  %>% select(-c(Region, maincrop,survey_year))

table_a2 <- cbind(table_a2_2015, table_a2_2020)
write.csv(table_a2, "~/Documents/Davis/R-Projects/organicseed_adoption/tables/append_samplet2.csv", row.names = F)