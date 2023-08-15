# SEM?

library(dplyr)
library(tidyr)
library(lavaan)
setwd("~/Documents/Davis/R-Projects/organicseed_adoption/")

df <- readRDS('~/Box/organicseed_adoption/sem_summary.rds') %>% 
  ungroup() %>% 
  #filter() should I scale them all then divide by year?
  mutate(annual_average = c(scale(annual_average)),
         grower_n_building = c(scale(grower_n_building)),
         avg_certifier_req = c(scale(avg_certifier_req)),
         supplychain_n = c(scale(supplychain_n)),
         organicip_n = c(scale(organicip_n))) %>% 
  mutate(t = case_when(
    year_range == 'pre-2002' ~ 0,
    year_range == '2002-2008' ~ 1,
    year_range == '2009-2013' ~ 2,
    year_range == '2014-2018' ~ 3
  ))

m1 <- lm(df$annual_average[df$t == 1] ~ df$grower_n_building[df$t == 0])
summary(m1)

library(fable)
?TSLM
check <- fpp3::us_change
tsm1 <- TSLM(df$annual_average ~ df$grower_n_building)
report(model(tsm1))
  
  
#lm(df$avg_certifier_req[df$t == 1] ~ df$grower_n_building[df$t == 0])
summary(m2)
m11_3 <- lm(supplychain_n ~ grower_n_building, df11)
summary(m11_3)
# path analyses
model11 <- '
  annual_average ~ grower_n_building
  avg_certifier_req ~ grower_n_building
  supplychain_n ~ grower_n_building
  # variance of exogenous
  grower_n_building ~~ grower_n_building
  # covariance of exogenous (NA for now)
  # residual variance of endogenous (NA for now)
  # residual covariance among endogenous (NA for now)
'

fit <- sem(model = model, data = df11)
summary(fit, fit.measures = T)


model <- '
  annual_average ~ grower_n_building + supplychain_n
  avg_certifier_req ~ grower_n_building + supplychain_n + organicip_n
  supplychain_n ~ grower_n_building + annual_average + avg_certifier_req
  organicip_n ~ annual_average + supplychain_n
  # variance of exogenous
  grower_n_building ~~ grower_n_building
  # covariance of exogenous (?)
  # residual variance of endogenous
  annual_average ~~ annual_average
  avg_certifier_req ~~ avg_certifier_req
  supplychain_n ~~ supplychain_n
  organicip_n ~~ organicip_n
  # residual covariance among endogenous
  annual_average ~~ avg_certifier_req + supplychain_n
  avg_certifier_req ~~ organicip_n
  
'

fit <- sem(model = model, data = df11)
summary(fit, fit.measures = T)


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