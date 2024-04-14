## Interactions
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(jtools)

source('code/functions.R')
df.full <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled.rds") %>% 
  select(RespondentID,
         OGseed_percent_maincrop_prop,
         OG_value_s,
         CertifierRequestn,
         noncommercial_seedsource_s,
         acreage_log_s,
         cropcat_n_s,
         #barrier_buyerReq_s,
         barrier_insuffQuantity_s,
         barrier_lackdesiredTraits_s,
         barrier_availability_s,
         maincrop,
         Region,
         survey_year) %>% 
  na.omit()

df.full_rounded <- df.full
df.full_rounded$OGseed_percent_maincrop_prop <- ifelse(df.full_rounded$OGseed_percent_maincrop_prop == 1, 0.999, ifelse(df.full_rounded$OGseed_percent_maincrop_prop == 0, 0.001, df.full_rounded$OGseed_percent_maincrop_prop))

library(betareg)
set.seed(19)
m_ix1 <- betareg(OGseed_percent_maincrop_prop ~ 
               barrier_availability_s +
               barrier_lackdesiredTraits_s +
               barrier_insuffQuantity_s +
               survey_year +
               OG_value_s + 
               acreage_log_s + 
               cropcat_n_s +
               CertifierRequestn +
               noncommercial_seedsource_s +
               maincrop +
               Region +
               barrier_insuffQuantity_s*acreage_log_s, 
             data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")

summary(m_ix1)
rev(df.full$barrier_insuffQuantity_s)[1:10]
(df.full$barrier_insuffQuantity_s)[1:10]
summary(df.full$barrier_insuffQuantity_s)
df.full_rounded_rev <- df.full_rounded
df.full_rounded_rev$barrier_availability_s <- ifelse(df.full_rounded_rev$barrier_availability_s < 0,
                                                     abs(df.full_rounded_rev$barrier_availability_s),
                                                     ifelse(df.full_rounded_rev$barrier_availability_s > 0,
                                                            df.full_rounded_rev$barrier_availability_s*-1, 
                                                            df.full_rounded_rev$barrier_availability_s))
df.full_rounded_rev$barrier_lackdesiredTraits_s <- ifelse(df.full_rounded_rev$barrier_lackdesiredTraits_s < 0,
                                                     abs(df.full_rounded_rev$barrier_lackdesiredTraits_s),
                                                     ifelse(df.full_rounded_rev$barrier_lackdesiredTraits_s > 0,
                                                            df.full_rounded_rev$barrier_lackdesiredTraits_s*-1, 
                                                            df.full_rounded_rev$barrier_lackdesiredTraits_s))
df.full_rounded_rev$barrier_insuffQuantity_s <- ifelse(df.full_rounded_rev$barrier_insuffQuantity_s < 0,
                                                          abs(df.full_rounded_rev$barrier_insuffQuantity_s),
                                                          ifelse(df.full_rounded_rev$barrier_insuffQuantity_s > 0,
                                                                 df.full_rounded_rev$barrier_insuffQuantity_s*-1, 
                                                                 df.full_rounded_rev$barrier_insuffQuantity_s))
m_ix2 <- betareg(OGseed_percent_maincrop_prop ~ 
                   barrier_availability_s +
                   barrier_lackdesiredTraits_s +
                   barrier_insuffQuantity_s +
                   survey_year +
                   OG_value_s + 
                   acreage_s + 
                   cropcat_n_s +
                   CertifierRequestn_s +
                   noncommercial_seedsource_s +
                   maincrop +
                   Region +
                   barrier_availability_s*OG_value_s*CertifierRequestn_s, 
                 data = df.full_rounded_rev, link = "logit", link.phi = NULL, type = "ML")

summary(m_ix2)

