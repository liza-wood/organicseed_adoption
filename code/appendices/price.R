library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(jtools)
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
source('code/functions.R')
setwd("~/Documents/Davis/R-Projects/organicseed_adoption/")

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
         barrier_price,
         barrier_buyerReq,
         barrier_insuffQuantity,
         barrier_lackdesiredTraits,
         barrier_availability,
         noncommercial_seedsource
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
         barrier_price_s = c(scale(barrier_price, scale = T)),
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
         cropcat_n_s = cropcat_n)  %>% 
  na.omit()

df.full_rounded <- df.full
df.full_rounded$CVseed_percent_maincrop_prop <- ifelse(df.full_rounded$CVseed_percent_maincrop_prop == 1, 0.999, ifelse(df.full_rounded$CVseed_percent_maincrop_prop == 0, 0.001, df.full_rounded$CVseed_percent_maincrop_prop))


# Check
library(betareg)
set.seed(19)
m <- betareg(CVseed_percent_maincrop_prop ~ 
               barrier_availability_s +
               barrier_buyerReq_s +
               barrier_lackdesiredTraits_s +
               barrier_insuffQuantity_s +
               barrier_price_s +
               survey_year +
               legitimation_user_s + 
               acreage_s + 
               cropcat_n_s +
               noncommercial_seedsource_s +
               CertifierRequestn_s +
               maincrop +
               Region, 
             data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m)
mdf <- prep_betreg_data(m)