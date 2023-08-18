library(dplyr)
df <- readRDS("~/Box/organicseed_adoption/df.rds")

# Scale data ----
# Considering acreage as the dependent variable 
## Use c() when scaling numbers because it "strips" the dimension (i.e. scaling converts them into some kind of matrix and Stack Overflow told me that c would strip this)

df <- df %>% 
  filter(Region != "International", maincrop != "Other") %>% 
  select(RespondentID, 
         OGseed_percent_maincrop, #dependent variable
         #OGVarietiesSuccessn, # legitimation
         OGSeedOGIntegrityn,
         CertifierRequestn, # guidance of the search
         maincrop, survey_year, Region, # system level features
         TotalAcreage, cropcat_n, # operation controls
         contains('barrier_'),
         noncommercial_seedsource,
         YearsCertified
  ) %>% 
  mutate(Region = factor(Region, levels = c("North Central", "West", 
                                            "Northeast", "South"))) %>% 
  mutate(OGseed_percent_maincrop_prop = OGseed_percent_maincrop/100,
         CVseed_percent_maincrop_prop = 1-OGseed_percent_maincrop_prop,
         acreage_log = log(TotalAcreage),
         acreage_log_s = c(scale(acreage_log, scale = T)), 
         acreage_s = c(scale(TotalAcreage, scale = T)), 
         cropcat_n_s = c(scale(cropcat_n, scale = T)),
         CertifierRequestn_s = c(scale(CertifierRequestn, scale = T)),
         legitimation_user_s = c(scale(OGSeedOGIntegrityn, scale = T)),
         survey_year = factor(survey_year),
         barrier_price_s = c(scale(barrier_price, scale = T)),
         barrier_availability_s = c(scale(barrier_availability, scale = T)),
         barrier_distrustQuality_s = c(scale(barrier_distrustQuality, scale = T)),
         barrier_buyerReq_s = c(scale(barrier_buyerReq, scale = T)),
         barrier_saveOwn_s  = c(scale(barrier_saveOwn, scale = T)),
         barrier_lackdesiredTraits_s = c(scale(barrier_lackdesiredTraits, scale = T)),
         barrier_insuffQuantity_s = c(scale(barrier_insuffQuantity, scale = T)),
         noncommercial_seedsource_s = c(scale(noncommercial_seedsource, 
                                              scale = T)),
         years_certified_s = c(scale(YearsCertified, scale = T)))

head(df[c('CVseed_percent_maincrop_prop', 'OGseed_percent_maincrop_prop')])

saveRDS(df, "~/Box/organicseed_adoption/df_scaled.rds")
