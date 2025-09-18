library(dplyr)
library(betareg)
df <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds")
model_ids <- readRDS('data/model_ids.RDS')
df10 <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled10.rds") %>% 
  #filter(RespondentID %in% model_ids) %>% 
  select(OGseed_percent_maincrop_prop,
         OG_value_s,
         barrier_all_rev_s,
         CertifierRequestn,
         noncommercial_seedsource_s,
         acreage_log_s,
         cropcat_n_s,
         years_certified_s,
         maincrop,
         Region,
         survey_year) 
df15 <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled15.rds")  %>% 
  filter(RespondentID %in% model_ids) %>% 
  select(OGseed_percent_maincrop_prop,
         OG_value_s,
         barrier_all_rev_s,
         CertifierRequestn,
         noncommercial_seedsource_s,
         acreage_log_s,
         cropcat_n_s,
         years_certified_s,
         maincrop,
         Region,
         survey_year)
df20 <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled20.rds")  %>% 
  filter(RespondentID %in% model_ids) %>% 
  select(OGseed_percent_maincrop_prop,
         OG_value_s,
         barrier_all_rev_s,
         CertifierRequestn,
         noncommercial_seedsource_s,
         acreage_log_s,
         cropcat_n_s,
         years_certified_s,
         maincrop,
         Region,
         survey_year) %>% 
  na.omit()

s = 0.5

n = nrow(df10)
x = df10$OGseed_percent_maincrop_prop
rounded = (x*(n-1)+s)/n
df10$OGseed_percent_maincrop_prop <- (df10$OGseed_percent_maincrop_prop * (n-1) + s)/n
set.seed(19)
m_ix10 <- betareg(OGseed_percent_maincrop_prop ~ 
                  barrier_all_rev_s +
                  OG_value_s + 
                  barrier_all_rev_s*OG_value_s +
                  CertifierRequestn + 
                  acreage_log_s + 
                  cropcat_n_s +
                  noncommercial_seedsource_s +
                  years_certified_s + 
                  maincrop +
                  Region,
                data = df10, link = "logit", link.phi = NULL, type = "ML")

n = nrow(df15)
x = df15$OGseed_percent_maincrop_prop
rounded = (x*(n-1)+s)/n
df15$OGseed_percent_maincrop_prop <- (df15$OGseed_percent_maincrop_prop * (n-1) + s)/n
set.seed(19)
m_ix15 <- betareg(OGseed_percent_maincrop_prop ~ 
                    barrier_all_rev_s +
                    OG_value_s + 
                    barrier_all_rev_s*OG_value_s +
                    CertifierRequestn + 
                    acreage_log_s + 
                    cropcat_n_s +
                    noncommercial_seedsource_s +
                    years_certified_s + 
                    maincrop +
                    Region,
                  data = df15, link = "logit", link.phi = NULL, type = "ML")

n = nrow(df20)
x = df20$OGseed_percent_maincrop_prop
rounded = (x*(n-1)+s)/n
df20$OGseed_percent_maincrop_prop <- (df20$OGseed_percent_maincrop_prop * (n-1) + s)/n
set.seed(19)
m_ix20 <- betareg(OGseed_percent_maincrop_prop ~ 
                    barrier_all_rev_s +
                    OG_value_s + 
                    barrier_all_rev_s*OG_value_s +
                    CertifierRequestn + 
                    acreage_log_s + 
                    cropcat_n_s +
                    noncommercial_seedsource_s +
                    years_certified_s + 
                    maincrop +
                    Region,
                  data = df20, link = "logit", link.phi = NULL, type = "ML")

summary(m_ix10)
summary(m_ix15)
summary(m_ix20)
m_list <- list(m_ix10, m_ix15, m_ix20)
saveRDS(m_list, 'tables/append_models_over_time.rds')
