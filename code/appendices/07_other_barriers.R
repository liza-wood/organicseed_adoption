# Other barriers
library(dplyr)
df <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds")

# Scale data ----
# Considering acreage as the dependent variable 
## Use c() when scaling numbers because it "strips" the dimension (i.e. scaling converts them into some kind of matrix and Stack Overflow told me that c would strip this)

df.full <- df %>% 
  filter(Region != "International", maincrop != "Other") %>% 
  select(RespondentID, 
         OGseed_percent_maincrop, 
         barrier_availability,
         barrier_insuffQuantity,
         barrier_lackdesiredTraits,
         barrier_buyerReq,
         barrier_price,
         OG_value,
         CertifierRequestn, 
         maincrop, survey_year, Region, 
         TotalAcreage, cropcat_n,
         noncommercial_seedsource
  ) %>% 
  na.omit() %>%  # do this now so that it scales around the sample we are using
  mutate(Region = factor(Region, levels = c("North Central", "West", 
                                            "Northeast", "South")),
         survey_year = factor(survey_year, c(2016, 2011, 2021))) %>% 
  mutate(OGseed_percent_maincrop_prop = OGseed_percent_maincrop/100,
         acreage_log = log(TotalAcreage),
         acreage_log_s = c(scale(acreage_log, scale = F)),
         cropcat_n_s = c(scale(cropcat_n, scale = T)),
         OG_value_s = c(scale(OG_value, scale = T)),
         survey_year = factor(survey_year),
         barrier_availability_s = c(scale(barrier_availability, scale = T)),
         barrier_lackdesiredTraits_s = c(scale(barrier_lackdesiredTraits, scale = T)),
         barrier_insuffQuantity_s = c(scale(barrier_insuffQuantity, scale = T)),
         noncommercial_seedsource_s = c(scale(noncommercial_seedsource, 
                                              scale = T)),
         #barrier_distrustQuality_s = c(scale(barrier_distrustQuality, scale = T)),
         barrier_buyerReq_s = c(scale(barrier_buyerReq, scale = T)),
         #barrier_saveOwn_s  = c(scale(barrier_saveOwn, scale = T)),
         barrier_price_s = c(scale(barrier_price, scale = T)),
         #years_certified_s = c(scale(YearsCertified, scale = T))
  )

df.full_rounded <- df.full
df.full_rounded$OGseed_percent_maincrop_prop <- ifelse(df.full_rounded$OGseed_percent_maincrop_prop == 1, 0.999, ifelse(df.full_rounded$OGseed_percent_maincrop_prop == 0, 0.001, df.full_rounded$OGseed_percent_maincrop_prop))

library(betareg)
set.seed(19)
m <- betareg(OGseed_percent_maincrop_prop ~ 
               barrier_availability_s +
               barrier_lackdesiredTraits_s +
               barrier_insuffQuantity_s +
               barrier_price_s +
               barrier_buyerReq_s +
               survey_year +
               OG_value_s + 
               acreage_log_s + 
               cropcat_n_s +
               CertifierRequestn +
               noncommercial_seedsource_s +
               maincrop +
               Region, 
             data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m)

saveRDS(m, 'tables/append_models_barriers.rds')
