# SEM?

library(dplyr)
library(tidyr)
library(lavaan)
setwd("~/Documents/Davis/R-Projects/organicseed_adoption/")
df.full <- readRDS("~/Box/organicseed_adoption/df_full.rds")
dfm1 <- readRDS("~/Box/organicseed_adoption/dfm1.rds") %>% 
  group_by(Region, survey_year, maincrop) %>% 
  mutate(cluster = n())


model <- '
    level: 1
      guid =~ CertifierRequestn + rev(barrier_buyerReq_s)
      leg =~ legitimation + rev(barrier_distrustQuality_s)
      supplychain_n_avg_s ~ funding_100thous + kd + guid + sales_mill
      kd =~ supplychain_n_avg_s + funding_100thous
      guid ~ sales_mill + kd
      sales_mill ~ supplychain_n_avg_s
      funding_100thous ~ sales_mill + leg + supplychain_n_avg_s + guid
      leg ~ supplychain_n_avg_s
      OGseed_percent_maincrop ~ kd + guid + leg + supplychain_n_avg_s + sales_mill + funding_100thous
    level: 2
      kd =~ organicip_10 + rev(barrier_availability_s)
'
fit <- sem(model = model, data = dfm1, cluster = "cluster")
summary(fit)
Demo.twolevel
dvs_core <- "legitimation + 
      
                 supplychain_n_avg_s +
                 barrier_lackdesiredTraits_s +
                 organicip_10 +
                 barrier_availability_s +
                 sales_mill + 
                 #barrier_saveOwn_s +
                 #SeedSourceOwn_s +
                 noncommercial_seedsource_s +
                 acreage_log +  
                 cropcat_n + 
                 years_certified_s + "
subsyst_onelevel <- "maincrop +
                     Region +
                     survey_year"
subsyst_multilevel <- "(1|maincrop) +
                     (1|Region) +
                     (1|survey_year)"

subsyst_multilevel_inla <- "f(maincrop, model = 'iid') +
                            f(Region, model = 'iid') +
                            f(survey_year, model = 'iid')"
iv_perc <- "OGseed_percent_maincrop ~ "
iv_prop <- "OGseed_percent_maincrop_prop ~ "
iv_binary <- "OGseed_percent_maincrop_binary ~ "