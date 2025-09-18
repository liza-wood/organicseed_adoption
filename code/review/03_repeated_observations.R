# Farm size and crop diversity?
model_ids <- readRDS('data/model_ids.RDS')
df.long <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_long.rds") %>% 
  select(TotalAcreage,
         TotalAcreage_maincrop,
         cropcat_n,
         OGseed_percent_maincrop_prop,
         OG_value,
         OG_value_s,
         OG_value_minus1,
         barrier_all_rev_s,
         barrier_all_rev_minus1,
         CertifierRequestn,
         noncommercial_seedsource_s,
         acreage_log,
         acreage_log_s,
         cropcat_n_s,
         years_certified_s,
         maincrop,
         Region,
         survey_year,
         RespondentID) %>% 
  na.omit()

acres_cat7 <- c("1 to 9", "10 to 49", "50 to 179", "180 to 499",
                "500 to 999", "1,000 to 1,999", "2,000 +")

df.full <- df.full %>% 
  mutate(acreage_range = case_when(
    TotalAcreage <10 ~ acres_cat7[1],
    TotalAcreage >= 10 & TotalAcreage < 50 ~ acres_cat7[2],
    TotalAcreage >= 50 & TotalAcreage < 180 ~ acres_cat7[3],
    TotalAcreage >= 180 & TotalAcreage < 500~ acres_cat7[4],
    TotalAcreage >= 500 & TotalAcreage < 1000 ~ acres_cat7[5],
    TotalAcreage >= 1000 & TotalAcreage < 2000 ~ acres_cat7[6],
    TotalAcreage >= 2000 ~ acres_cat7[7],
    T ~ NA
  )) %>% 
  mutate(crops_per_acre = cropcat_n/TotalAcreage,
         crops_per_log_acre = cropcat_n/(acreage_log+0.001))



source('code/functions.R')
df_long <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_long.rds") %>% 
  filter(RespondentID %in% model_ids)
dfs <- scale_data(df, OGseed_percent_maincrop) %>% 
  select(OGseed_percent_maincrop_prop,
         OG_value_s,
         OG_value_minus1,
         barrier_all_rev_s,
         barrier_all_rev_minus1,
         CertifierRequestn,
         noncommercial_seedsource_s,
         acreage_log_s,
         acreage_log_mincrop_s,
         cropcat_n_s,
         years_certified_s,
         mincrop,
         maincrop,
         Region,
         survey_year,
         RespondentID) 

df.full_rounded <- dfs
s = 0.5
n = nrow(dfs)
x = df.full_rounded$OGseed_percent_maincrop_prop
rounded = (x*(n-1)+s)/n
summary(rounded)

df.full_rounded$OGseed_percent_maincrop_prop <- (df.full_rounded$OGseed_percent_maincrop_prop * (n-1) + s)/n

m_ix <- betareg(OGseed_percent_maincrop_prop ~ 
                  barrier_all_rev_s +
                  OG_value_s + 
                  barrier_all_rev_s*OG_value_s +
                  CertifierRequestn + 
                  acreage_log_mincrop_s + 
                  cropcat_n_s +
                  noncommercial_seedsource_s +
                  years_certified_s + 
                  mincrop +
                  Region +
                  survey_year,
                data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m_ix)
