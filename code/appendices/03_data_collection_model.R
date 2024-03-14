# Model with random versus convenience check, 2021
library(dplyr)
library(ggplot2)
source('code/functions.R')
# Model using only certifier data
df.full <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds") %>% 
  filter(Region != "International", maincrop != "Other") %>% 
  select(RespondentID, 
         OGseed_percent_maincrop, #dependent variable
         #OGVarietiesSuccessn, # legitimation
         OGSeedOGIntegrityn,
         CertifierRequestn, # guidance of the search
         maincrop, survey_year, Region, # system level features
         TotalAcreage, cropcat_n, # operation controls
         #contains('barrier_'),
         barrier_buyerReq,
         barrier_insuffQuantity,
         barrier_lackdesiredTraits,
         barrier_availability,
         noncommercial_seedsource,
         data_coll_method#,
         #YearsCertified
  ) %>% 
  na.omit() %>%  # do this now so that it scales around the sample we are using
  mutate(OGseed_percent_maincrop_prop = OGseed_percent_maincrop/100,
         CVseed_percent_maincrop_prop = 1-OGseed_percent_maincrop_prop) %>%  # do this now so that it
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
         #barrier_price_s = c(scale(barrier_price, scale = T)),
         barrier_availability_s = c(scale(barrier_availability, scale = T)),
         #barrier_distrustQuality_s = c(scale(barrier_distrustQuality, scale = T)),
         barrier_buyerReq_s = c(scale(barrier_buyerReq, scale = T)),
         #barrier_saveOwn_s  = c(scale(barrier_saveOwn, scale = T)),
         barrier_lackdesiredTraits_s = c(scale(barrier_lackdesiredTraits, scale = T)),
         barrier_insuffQuantity_s = c(scale(barrier_insuffQuantity, scale = T)),
         noncommercial_seedsource_s = c(scale(noncommercial_seedsource, 
                                              scale = T)),
         #years_certified_s = c(scale(YearsCertified, scale = T))
  ) %>% 
  filter(survey_year == 2021)

## Descriptively, can we see anything obvious?
df.full %>% 
  ggplot(aes(y = CVseed_percent_maincrop_prop, color = data_coll_method)) +
  geom_density() +
  theme_minimal()

df.full_rounded <- df.full
df.full_rounded$CVseed_percent_maincrop_prop <- ifelse(df.full_rounded$CVseed_percent_maincrop_prop == 1, 0.999, ifelse(df.full_rounded$CVseed_percent_maincrop_prop == 0, 0.001, df.full_rounded$CVseed_percent_maincrop_prop))

library(betareg)
set.seed(19)
df.full_rounded$maincrop <- factor(df.full_rounded$maincrop, c("Field", "Forage", "Vegetables"))
df.full_rounded$Region <- factor(df.full_rounded$Region, c("North Central", "West", "Northeast", "South"))
m21_datacoll <- betareg(CVseed_percent_maincrop_prop ~ 
                          barrier_availability_s +
                          barrier_buyerReq_s +
                          barrier_lackdesiredTraits_s +
                          barrier_insuffQuantity_s +
                          #survey_year +
                          legitimation_user_s + 
                          acreage_s + 
                          cropcat_n_s +
                          noncommercial_seedsource_s +
                          CertifierRequestn_s +
                          maincrop +
                          Region +
                         data_coll_method, 
             data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m21_datacoll)
m21_datacoll_df <- prep_betreg_data(m21_datacoll)
plot_inla_models(m21_datacoll_df, 1)

m21 <- betareg(CVseed_percent_maincrop_prop ~ 
                          barrier_availability_s +
                          barrier_buyerReq_s +
                          barrier_lackdesiredTraits_s +
                          barrier_insuffQuantity_s +
                          #survey_year +
                          legitimation_user_s + 
                          acreage_s + 
                          cropcat_n_s +
                          noncommercial_seedsource_s +
                          CertifierRequestn_s +
                          maincrop +
                          Region, 
                        data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m21)
m21_df <- prep_betreg_data(m21)
plot_inla_models(m21_df, 1)

saveRDS(list(m21_datacoll, m21), 'tables/tablesa5_models.RDS')
