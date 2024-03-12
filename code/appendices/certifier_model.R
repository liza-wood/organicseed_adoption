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
         certifier_codename#,
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
  mutate(acreage_s = acreage_log,
         cropcat_n_s = cropcat_n) 
cert_n <- df.full %>% 
  group_by(certifier_codename) %>% count() %>% 
  arrange(-n)


df.full$certifier_codename <- ifelse(df.full$certifier_codename %in% 
                                       cert_n$certifier_codename[cert_n$n > 20],
                                     df.full$certifier_codename, "other_certifier")
table(df.full$certifier_codename)
lvls <- rev(names(table(df.full$certifier_codename)))
df.full$certifier_codename <- factor(df.full$certifier_codename, lvls)

# Relationship between certifier request use and not:
df.full %>% 
  filter(CVseed_percent_maincrop_prop <= .99) %>% 
  group_by(certifier_codename) %>% 
  summarize(mean_request_s = mean(CertifierRequestn_s),
            mean_request_n = mean(CertifierRequestn))

df.full %>% 
  filter(CVseed_percent_maincrop_prop <= .99) %>% 
  group_by(certifier_codename, CertifierRequestn) %>% 
  count()

df.full_rounded <- df.full
df.full_rounded$CVseed_percent_maincrop_prop <- ifelse(df.full_rounded$CVseed_percent_maincrop_prop == 1, 0.999, ifelse(df.full_rounded$CVseed_percent_maincrop_prop == 0, 0.001, df.full_rounded$CVseed_percent_maincrop_prop))

library(betareg)
set.seed(19)
m <- betareg(CVseed_percent_maincrop_prop ~ 
               legitimation_user_s + 
               acreage_s + 
               cropcat_n_s +
               barrier_buyerReq_s +
               barrier_insuffQuantity_s +
               barrier_lackdesiredTraits_s +
               barrier_availability_s +
               maincrop +
               Region +
               survey_year +
               #CertifierRequestn_s + #is this redundant?
               noncommercial_seedsource_s  +
               certifier_codename, 
             data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m)
mdf <- prep_betreg_data(m)
plot_inla_models(mdf, 1)
# With certifier request
#Log-likelihood: 631.5 on 24 Df
#Pseudo R-squared: 0.2698
#Number of iterations: 34 (BFGS) + 2 (Fisher scoring) 

# Without certifier request
#Log-likelihood: 625.5 on 23 Df
#Pseudo R-squared: 0.2404
#Number of iterations: 33 (BFGS) + 2 (Fisher scoring) 

# Compared to the non certifier model
# Type of estimator: ML (maximum likelihood)
# Log-likelihood:  2266 on 18 Df
# Pseudo R-squared: 0.2437
# Number of iterations: 28 (BFGS) + 2 (Fisher scoring) 
# AIC: -4495.492
# BIC: -4397.058