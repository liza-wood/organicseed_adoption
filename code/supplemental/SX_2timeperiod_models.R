library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
library(cowplot)
library(brms)
library(RColorBrewer)
library(jtools)
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
setwd("~/Documents/Davis/R-Projects/organicseed_adoption/")
source('code/functions_model.R')
df <- readRDS("~/Box/organicseed_adoption/df_multilevel.rds")

greens <- colorRampPalette(c("lightgrey", "#55752f"))(8) # 2014-2018
reds <- colorRampPalette(c("lightgrey", "#90353b"))(8) # 2002-2008
blues <- colorRampPalette(c("lightgrey", "#1A476f"))(8) # 2009-2013

# 1. Select and scale data ----
# Considering acreage as the dependent variable 
## Use c() when scaling numbers because it "strips" the dimension (i.e. scaling converts them into some kind of matrix and Stack Overflow told me that c would strip this)
df.full <- df %>% 
  filter(Region != "International", maincrop != "Other") %>% 
  select(RespondentID, 
         OGseed_percent_maincrop, #dependent variable
         OGVarietiesSuccessn, # legitimation
         CertifierRequestn, # guidance of the search
         annual_average, # knowledge development/resource mobilization
         ratio_building, # entrepreneurial activities 
         dnb_regional_sales, # sales would be market formation?
         organicip_n, # knowledge development
         n_nosb_recs,
         maincrop, survey_year, Region, # system level features
         TotalAcreage, cropcat_n, # operation controls
         contains('barrier_'),
         #SeedSourceOwn
         noncommercial_seedsource
  ) %>% 
  mutate(Region = factor(Region, levels = c("North Central", "West", 
                                            "Northeast", "South"))) %>% 
  mutate(OGseed_percent_maincrop_prop = OGseed_percent_maincrop/100,
         OGseed_percent_maincrop_binary = ifelse(OGseed_percent_maincrop >= 50, 1, 
                                                 ifelse(OGseed_percent_maincrop < 50, 0, NA)),
         acreage_log = c(scale(log(TotalAcreage), scale = F)), 
         cropcat_n = c(scale(cropcat_n, scale = F)),
         funding_100thous = c(scale(annual_average/100000, scale = F)),
         legitimation = c(scale(OGVarietiesSuccessn, scale = F)),
         ratio = c(scale(ratio_building, scale = F)), # entrepreneurial activities 
         sales_mill = c(scale(dnb_regional_sales/1000000, scale = F)),
         organicip_10 = c(scale(organicip_n/10, scale = F)), # knowledge development
         n_nosb_recs_s = c(scale(n_nosb_recs, scale = F)),
         survey_year = factor(survey_year),
         barrier_availability_s = c(scale(barrier_availability, scale = F)),
         barrier_distrustQuality_s = c(scale(barrier_distrustQuality, scale = F)),
         barrier_buyerReq_s = c(scale(barrier_buyerReq, scale = F)),
         barrier_saveOwn_s  = c(scale(barrier_saveOwn, scale = F)),
         barrier_lackdesiredTraits_s = c(scale(barrier_lackdesiredTraits, scale = F)),
         barrier_insuffQuantity_s = c(scale(barrier_insuffQuantity, scale = F)),
         #SeedSourceOwn_s = c(scale(SeedSourceOwn/10, scale = F))
         noncommercial_seedsource = c(scale(noncommercial_seedsource/10, scale = F)))


# 3. Select complete cases ----
dfm1 <- select(df.full, 
               OGseed_percent_maincrop, 
               OGseed_percent_maincrop_prop, 
               OGseed_percent_maincrop_binary,
               maincrop,
               CertifierRequestn,
               acreage_log, 
               cropcat_n,
               funding_100thous,
               legitimation,
               #n_nosb_recs_s,
               #ratio, 
               supplychain_n_avg,
               sales_mill,
               organicip_10,
               barrier_availability_s,
               barrier_distrustQuality_s,
               barrier_buyerReq_s,
               #barrier_saveOwn_s,
               barrier_lackdesiredTraits_s,
               #barrier_insuffQuantity_s,
               Region, 
               survey_year,
               SeedSourceOwn_s) %>% 
  filter(complete.cases(.))

# yearly models ----

df2011 <- filter(dfm1, survey_year == 2011)
df2016_21 <- filter(dfm1, survey_year %in% c(2016, 2021))

acreage_T1_bayes <- yearlymodel_fx(data = df2011, 
                                     model_type = "bayes multi-level", 
                                     year = 2011)
acreage_T2_bayes <- fullmodel_fx(data = df2016_21, 
                                     model_type = "bayes multi-level")

summary(acreage_T1_bayes)
summary(acreage_T2_bayes)

# This is the one I want for fixed effects:
jtools::plot_coefs(acreage_T1_bayes, 
                   acreage_T2_bayes, 
                   legend.title = "", 
                   model.names = c("2010", "2015 & 2020"), 
                   colors = c(greens[5], blues[5]),  
                   coefs = coefs) +
  #theme(legend.position = "bottom") +
  labs(title = "")
