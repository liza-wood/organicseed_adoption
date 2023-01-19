library(dplyr)
library(tidyr)
library(stargazer)
library(lme4)
library(ggplot2)
library(equatiomatic)
library(cowplot)
library(brms)
library(RColorBrewer)
library(jtools)
setwd("~/Documents/Davis/R-Projects/organicseed_adoption/")
df <- readRDS("~/Box/organicseed_adoption/df_multilevel.rds")
hist(df$OGseed_percent_maincrop)
plot(df$OGseed_percent_maincrop)

greens <- colorRampPalette(c("lightgrey", "#55752f"))(8) # 2014-2018
reds <- colorRampPalette(c("lightgrey", "#90353b"))(8) # 2002-2008
blues <- colorRampPalette(c("lightgrey", "#1A476f"))(8) # 2009-2013

# Select data ----
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
         dnb_regional_sales,
         organicip_n, # knowledge development
         n_nosb_recs,
         # sales would be market formation?
         maincrop, survey_year, Region, # system level features
         TotalAcreage, cropcat_n, # operation controls
         contains('barrier_')
  ) %>% 
  mutate(Region = factor(Region, levels = c("North Central", "West", 
                                            "Northeast", "South"))) %>% 
  mutate(OGseed_percent_maincrop_prop = OGseed_percent_maincrop/100,
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
         barrier_insuffQuantity_s = c(scale(barrier_insuffQuantity, scale = F)))

summary(df.full$OGseed_percent_maincrop_prop)
summary(df.full$funding_100thous)
summary(df.full$sales_mill)
summary(df.full$organicip_10)
summary(df.full$n_nosb_recs_s)
summary(df.full$ratio)
summary(df.full$acreage_log)

dfm1 <- select(df.full, 
               OGseed_percent_maincrop, 
               OGseed_percent_maincrop_prop, 
               maincrop,
               CertifierRequestn,
               acreage_log, 
               cropcat_n,
               funding_100thous,
               legitimation,
               n_nosb_recs_s,
               ratio, 
               sales_mill,
               organicip_10,
               barrier_availability_s,
               barrier_distrustQuality_s,
               barrier_buyerReq_s,
               barrier_saveOwn_s,
               barrier_lackdesiredTraits_s,
               barrier_insuffQuantity_s,
               Region, 
               survey_year) %>% 
  filter(complete.cases(.))

# Selecting models ----
# Overall, nothing is horribly different from one another. When I run the 2016 on its own, I get a singularity problem, so I might prefer the Bayesian approach. 
# I would like to make coefficient plots of all four models: All years, then 2011, 2016, and 2018. 
# It is hard to make a stargazer table from a Bayesian output.
# It is also nearly impossible to make a coefficient plants with multiple models together. jtools::plot_coefs can do it for just the fixed effcts, but so I decided to duplicate it with random effects, by extracting the random effetcs from the sjr::plot_summs. It is messy...
set.seed(116)
model1 <- brm(OGseed_percent_maincrop_prop ~ 
                legitimation + 
                barrier_distrustQuality_s +
                barrier_buyerReq_s +
                #barrier_insuffQuantity_s +
                CertifierRequestn + 
                #n_nosb_recs_s +
                barrier_lackdesiredTraits_s +
                funding_100thous + 
                organicip_10 +
                ratio +
                barrier_availability_s +
                sales_mill + 
                (1|maincrop) +
                (1|Region) +
                (1|survey_year) +
                barrier_saveOwn_s +
                acreage_log +  
                cropcat_n, 
              data = dfm1,
              family = zero_one_inflated_beta(
                link = "logit",
                link_phi = "log",
                link_zoi = "logit",
                link_coi = "logit"
              ),
              iter = 4000)
summary(model1)




# https://mvuorre.github.io/posts/2019-02-18-analyze-analog-scale-ratings-with-zero-one-inflated-beta-models/#zoib-regression
zoib_model <- bf(
  OGseed_percent_maincrop_prop ~ 
    legitimation + 
    barrier_distrustQuality_s +
    barrier_buyerReq_s +
    #barrier_insuffQuantity_s +
    CertifierRequestn + 
    #n_nosb_recs_s +
    barrier_lackdesiredTraits_s +
    funding_100thous + 
    organicip_10 +
    ratio +
    barrier_availability_s +
    sales_mill + 
    (1|maincrop) +
    (1|Region) +
    (1|survey_year) +
    barrier_saveOwn_s +
    acreage_log +  
    cropcat_n,
  phi ~     legitimation + 
    barrier_distrustQuality_s +
    barrier_buyerReq_s +
    #barrier_insuffQuantity_s +
    CertifierRequestn + 
    #n_nosb_recs_s +
    barrier_lackdesiredTraits_s +
    funding_100thous + 
    organicip_10 +
    ratio +
    barrier_availability_s +
    sales_mill + 
    (1|maincrop) +
    (1|Region) +
    (1|survey_year) +
    barrier_saveOwn_s +
    acreage_log +  
    cropcat_n,
  zoi ~     legitimation + 
    barrier_distrustQuality_s +
    barrier_buyerReq_s +
    #barrier_insuffQuantity_s +
    CertifierRequestn + 
    #n_nosb_recs_s +
    barrier_lackdesiredTraits_s +
    funding_100thous + 
    organicip_10 +
    ratio +
    barrier_availability_s +
    sales_mill + 
    (1|maincrop) +
    (1|Region) +
    (1|survey_year) +
    barrier_saveOwn_s +
    acreage_log +  
    cropcat_n,
  coi ~     legitimation + 
    barrier_distrustQuality_s +
    barrier_buyerReq_s +
    #barrier_insuffQuantity_s +
    CertifierRequestn + 
    #n_nosb_recs_s +
    barrier_lackdesiredTraits_s +
    funding_100thous + 
    organicip_10 +
    ratio +
    barrier_availability_s +
    sales_mill + 
    (1|maincrop) +
    (1|Region) +
    (1|survey_year) +
    barrier_saveOwn_s +
    acreage_log +  
    cropcat_n, 
  family = zero_one_inflated_beta()
)

model2 <- brm(
  formula = zoib_model,
  data = dfm1,
  cores = 4,
  file = "brm-zoib"
)
summary(model2)

library(tidyverse)
posterior_samples(model2, pars = "b_")[,1:4] %>% 
  mutate_at(c("b_phi_Intercept"), exp) %>% 
  mutate_at(vars(-"b_phi_Intercept"), plogis) %>% 
  posterior_summary() %>% 
  as.data.frame() %>% 
  rownames_to_column("Parameter") 

mean(dfm1$OGseed_percent_maincrop_prop %in% 0:1) %>% round(3)

plot(
  conditional_effects(model2, dpar = "mu"), 
  points = TRUE, 
  point_args = list(width = .05, shape = 1)
)

# Fixed effects plot ----
library(jtools)
plot_coefs(model1,
           legend.title = "", 
           colors = "black",  
           coefs = c("Perceived importance of organic seed" = "legitimation", 
                     "Seed quality distrust barrier" = "barrier_distrustQuality_s",
                     "Buyer requirement barrier" = "barrier_buyerReq_s",
                     "Certifiers request organic seed use" = "CertifierRequestn", 
                     "Limited seed traits barrier" = "barrier_lackdesiredTraits_s",
                     "Organic research funding (million USD)" = "funding_100thous", 
                     "Number of registered organically bred varieties" = "organicip_10",
                     "Ratio of organic growers to supply chain actors" = "ratio",
                     "Seed availability barrier" = "barrier_availability_s",
                     "Organic seed company sales" = "sales_mill",
                     "Saves own seed" = "barrier_saveOwn_s",
                     "Farm size (log acres)" = "acreage_log",
                     "Number of crop categories" = "cropcat_n")) 
ggsave("tables/figure3a.jpeg", width = 9, height = 4.5)
# Full model ----
fullmodel_fx <- function(data, model_type = "lm"){
  if(model_type == "bayes"){
    model <- brm(OGseed_percent_maincrop_prop ~ 
                   legitimation + 
                   barrier_distrustQuality_s +
                   barrier_buyerReq_s +
                   #barrier_insuffQuantity_s +
                   CertifierRequestn + 
                   #n_nosb_recs_s +
                   barrier_lackdesiredTraits_s +
                   funding_100thous + 
                   organicip_10 +
                   ratio +
                   barrier_availability_s +
                   sales_mill + 
                   (1|maincrop) +
                   (1|Region) +
                   (1|survey_year) +
                   barrier_saveOwn_s +
                   acreage_log +  
                   cropcat_n, 
                 data = data,
                 family = zero_one_inflated_beta(
                   link = "logit",
                   link_phi = "log",
                   link_zoi = "logit",
                   link_coi = "logit"
                 ))
  } else if(model_type == "lm"){
    model <- lm(OGseed_percent_maincrop ~ 
                  legitimation + 
                  CertifierRequestn + 
                  funding_million + 
                  ratio +
                  organicip +
                  maincrop +
                  Region +
                  survey_year +
                  acreage_log +  
                  cropcat_n, 
                data = data)
  } else if (model_type == "tobit"){
    model <- censReg::censReg(OGseed_percent_maincrop ~ 
                                legitimation + 
                                CertifierRequestn + 
                                funding_million + 
                                ratio +
                                organicip +
                                maincrop +
                                Region +
                                survey_year +
                                acreage_log +  
                                cropcat_n,
                              right = 100,
                              left = 0,
                              data = dfm1)
  } else if (model_type == "lmer"){
    model <- lmer(OGseed_percent_maincrop ~ 
                    legitimation + 
                    CertifierRequestn + 
                    funding_million + 
                    ratio +
                    organicip +
                    (1|maincrop) +
                    (1|Region) +
                    (1|survey_year) +
                    acreage_log +  
                    cropcat_n, 
                  data = data)
  }
  return(model)
}

acreage_bayes <- fullmodel_fx(data = dfm1, model_type = "bayes")
#acreage_bayes <- add_ic(acreage_bayes, ic = c("loo", "waic"))

yearlymodel_fx <- function(data, model_type = "lm", year){
  if(year %in% c(2011, 2016)){
    if(model_type == "bayes"){
      model <- brm(OGseed_percent_maincrop_prop ~ 
                     legitimation + 
                     barrier_distrustQuality_s +
                     barrier_buyerReq_s +
                     #barrier_insuffQuantity_s +
                     CertifierRequestn + 
                     #n_nosb_recs_s +
                     barrier_lackdesiredTraits_s +
                     funding_100thous + 
                     #organicip_10 +
                     ratio +
                     barrier_availability_s +
                     sales_mill + 
                     (1|maincrop) +
                     (1|Region) +
                     #(1|survey_year) +
                     barrier_saveOwn_s +
                     acreage_log +  
                     cropcat_n, 
                   data = data,
                   family = zero_one_inflated_beta(
                     link = "logit",
                     link_phi = "log",
                     link_zoi = "logit",
                     link_coi = "logit"
                   ))
    } else if(model_type == "lm"){
      model <- lm(OGseed_percent_maincrop ~ 
                    legitimation + 
                    CertifierRequestn + 
                    funding_million + 
                    ratio +
                    #organicip +
                    maincrop +
                    Region +
                    acreage_log +  
                    cropcat_n, 
                  data = data)
    } else if (model_type == "tobit"){
      model <- censReg::censReg(OGseed_percent_maincrop ~ 
                                  legitimation + 
                                  CertifierRequestn + 
                                  funding_million + 
                                  ratio +
                                  #organicip +
                                  maincrop +
                                  Region +
                                  acreage_log +  
                                  cropcat_n,
                                right = 100,
                                left = 0,
                                data = dfm1)
    } else if (model_type == "lmer"){
      model <- lmer(OGseed_percent_maincrop ~ 
                      legitimation + 
                      CertifierRequestn + 
                      funding_million + 
                      ratio +
                      #organicip +
                      (1|maincrop) +
                      (1|Region) +
                      acreage_log +  
                      cropcat_n, 
                    data = data)
    }
  } else if(year == 2021){
    if(model_type == "bayes"){
      model <- brm(OGseed_percent_maincrop_prop ~ 
                     legitimation + 
                     barrier_distrustQuality_s +
                     barrier_buyerReq_s +
                     #barrier_insuffQuantity_s +
                     CertifierRequestn + 
                     #n_nosb_recs_s +
                     barrier_lackdesiredTraits_s +
                     funding_100thous + 
                     organicip_10 +
                     ratio +
                     barrier_availability_s +
                     sales_mill + 
                     (1|maincrop) +
                     (1|Region) +
                     #(1|survey_year) +
                     barrier_saveOwn_s +
                     acreage_log +  
                     cropcat_n, 
                   data = data,
                   family = zero_one_inflated_beta(
                     link = "logit",
                     link_phi = "log",
                     link_zoi = "logit",
                     link_coi = "logit"
                   ))
    } else if(model_type == "lm"){
      model <- lm(OGseed_percent_maincrop ~ 
                    legitimation + 
                    CertifierRequestn + 
                    funding_million + 
                    ratio +
                    organicip +
                    maincrop +
                    Region +
                    acreage_log +  
                    cropcat_n, 
                  data = data)
    } else if (model_type == "tobit"){
      model <- censReg::censReg(OGseed_percent_maincrop ~ 
                                  legitimation + 
                                  CertifierRequestn + 
                                  funding_million + 
                                  ratio +
                                  organicip +
                                  maincrop +
                                  Region +
                                  acreage_log +  
                                  cropcat_n,
                                right = 100,
                                left = 0,
                                data = dfm1)
    } else if (model_type == "lmer"){
      model <- lmer(OGseed_percent_maincrop ~ 
                      legitimation + 
                      CertifierRequestn + 
                      funding_million + 
                      ratio +
                      organicip +
                      (1|maincrop) +
                      (1|Region) +
                      acreage_log +  
                      cropcat_n, 
                    data = data)
    }
  }
 
  return(model)
}
df2011 <- filter(dfm1, survey_year == 2011)
df2016 <- filter(dfm1, survey_year == 2016)
df2021 <- filter(dfm1, survey_year == 2021)

ggplot(df2021, aes(x = OGseed_percent_maincrop)) +
  geom_histogram()
ggplot(df, aes(x = OGseed_percent_maincrop, color = factor(survey_year))) +
  geom_density(position = 'dodge', alpha = .5)

acreage_2011_bayes <- yearlymodel_fx(data = df2011, model_type = "bayes", year = 2011)
acreage_2016_bayes <- yearlymodel_fx(data = df2016, model_type = "bayes", year = 2016)
acreage_2021_bayes <- yearlymodel_fx(data = df2021, model_type = "bayes", year = 2021)

# This is the one I want for fixed effects:
jtools::plot_coefs(acreage_bayes, acreage_2011_bayes, acreage_2016_bayes, acreage_2021_bayes,
                   legend.title = "", 
                   model.names = c("All years", "2010", "2015", "2020"), 
                   colors = c("black", greens[5], reds[5], blues[5]),  
                   coefs = c("Perceived importance of organic seed" = "legitimation", 
                             "Seed quality distrust barrier" = "barrier_distrustQuality_s",
                             "Buyer requirement barrier" = "barrier_buyerReq_s",
                             "Certifiers request organic seed use" = "CertifierRequestn", 
                             "Limited seed traits barrier" = "barrier_lackdesiredTraits_s",
                             "Organic research funding" = "funding_100thous", 
                             "Number of registered organically bred varieties" = "organicip_10",
                             "Ratio of organic growers to supply chain actors" = "ratio",
                             "Seed availability barrier" = "barrier_availability_s",
                             "Organic seed company sales" = "sales_mill",
                             "Saves own seed" = "barrier_saveOwn_s",
                             "Farm size (log acres)" = "acreage_log",
                             "Number of crop categories" = "cropcat_n")) +
  theme(legend.position = "bottom")
ggsave("tables/figure5a.jpeg", width = 9, height = 5)

# Nothing will automatically make what I want for random effects, so here it goes:
out <- sjPlot::plot_model(acreage_bayes, type = "re")
ranefdf_full <- data.frame(out[[1]]) %>% 
  mutate(term = c("Field", "Forage", "Vegetables", 
                  "North Central", "West", "Northeast", "South",
                  "2011", "2016", "2021")) %>% 
  rename("conf.low.inner" = "conf.low50",
         "conf.high.inner" = "conf.high50") %>% 
  select(facet, term, estimate, `conf.low`, `conf.low.inner`, `conf.high.inner`, `conf.high`) %>% 
  mutate(model = "All years")
ranef_table_yrly <- function(lmer_model, year_in_model){
  out <- sjPlot::plot_model(lmer_model, type = "re")
  ranefdf <- data.frame(out[[1]]) %>% 
    mutate(term = c("Field", "Forage", "Vegetables", 
                    "North Central", "West", "Northeast", "South")) %>% 
    rename("conf.low.inner" = "conf.low50",
           "conf.high.inner" = "conf.high50") %>% 
    select(facet, term, estimate, `conf.low`, `conf.low.inner`, `conf.high.inner`, `conf.high`) %>%
    mutate(model = case_when(
      year_in_model == "All years" ~ "All years",
      year_in_model == "2011" ~ "2010",
      year_in_model == "2016" ~ "2015",
      year_in_model == "2021" ~ "2020",
    ))
  return(ranefdf)
}
ranefdf11 <- ranef_table_yrly(acreage_2011_bayes, 2011)
ranefdf16 <- ranef_table_yrly(acreage_2016_bayes, 2016)
ranefdf21 <- ranef_table_yrly(acreage_2021_bayes, 2021)

ranefdf <- rbind(ranefdf_full, ranefdf11, ranefdf16, ranefdf21)
ranefdf$model <- factor(ranefdf$model, c("All years", "2010", "2015", "2020"))
pd <- position_dodge(width=0.75)
ggplot(ranefdf, aes(x = estimate, 
                    y = factor(term, rev(unique(ranefdf$term))))) +
  #geom_point(size=3,  position = pd, fill = "white", shape = c(rep(1,10), rep(2, 7),
  #                                                             rep(24,7), rep(25,7))) +
  ggstance::geom_pointrangeh(aes(xmin = conf.low, 
                                 xmax = conf.high,
                                 color = factor(model, rev(c("All years", "2020", "2015", "2010"))), 
                                 shape = factor(model, rev(c("All years", "2020", "2015", "2010")))),
                             position = ggstance::position_dodgev(height = .6), 
                             fill = "white", fatten = 3, size = 0.8) +
  #geom_errorbar(aes(xmin=`conf.high`,xmax=`conf.low`), width=0.1, position = pd) +
  geom_vline(xintercept = 0, linetype = 2, size = 0.25) + 
  scale_colour_manual(values = c("black", greens[5], reds[5], blues[5]),
                      limits = levels(ranefdf$model)) + 
  scale_shape_manual(limits = levels(ranefdf$model), 
                       values = 21:24,) +
  theme_nice(legend.pos = "right") + 
  drop_y_gridlines() + 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 10), 
        #legend.position="none",
        panel.grid.major.x = element_line(linetype = "solid")) + 
  labs(x = "Intercept", shape = "", color = "")
ggsave("tables/figure5b.jpeg", width = 8, height = 4)

# Making a table ----
summary(acreage_bayes)
out <- sjPlot::plot_model(acreage_bayes)
fixefdf_full <- data.frame(out[[1]]) %>% 
  mutate(Variable = c("Perceived importance of organic seed", 
                      "Seed quality distrust barrier",
                      "Buyer requirement barrier",
                      "Certifiers request organic seed use", 
                      "Limited seed traits barrier",
                      "Organic research funding",
                      "Number of registered organically bred varieties",
                      "Ratio of organic growers to supply chain actors",
                      "Seed availability barrier",
                      "Organic seed company sales",
                      "Saves own seed",
                      "Farm size (log acres)",
                      "Number of crop categories", "phi")) %>% 
  mutate(`Effect size` = round(estimate, 2),
         `Upper CI` = round(conf.high, 2),
         `Lower CI` = round(conf.low, 2)) %>% 
  select(Variable, `Effect size`, `Upper CI`, `Lower CI`)

out <- sjPlot::plot_model(acreage_2011_bayes)
summary(acreage_2011_bayes)
fixefdf_2011 <- data.frame(out[[1]]) %>% 
  mutate(Variable = c("Perceived importance of organic seed", 
                      "Seed quality distrust barrier",
                      "Buyer requirement barrier",
                      "Certifiers request organic seed use", 
                      "Limited seed traits barrier",
                      "Organic research funding",
                      #"Number of registered organically bred varieties",
                      "Ratio of organic growers to supply chain actors",
                      "Seed availability barrier",
                      "Organic seed company sales",
                      "Saves own seed",
                      "Farm size (log acres)",
                      "Number of crop categories")) %>% 
  mutate(`Effect size` = round(estimate, 2),
         `Upper CI` = round(conf.high, 2),
         `Lower CI` = round(conf.low, 2)) %>% 
  select(Variable, `Effect size`, `Upper CI`, `Lower CI`)

out <- sjPlot::plot_model(acreage_2016_bayes)
fixefdf_2016 <- data.frame(out[[1]]) %>% 
  mutate(Variable = c("Perceived importance of organic seed", 
                      "Seed quality distrust barrier",
                      "Buyer requirement barrier",
                      "Certifiers request organic seed use", 
                      "Limited seed traits barrier",
                      "Organic research funding",
                      #"Number of registered organically bred varieties",
                      "Ratio of organic growers to supply chain actors",
                      "Seed availability barrier",
                      "Organic seed company sales",
                      "Saves own seed",
                      "Farm size (log acres)",
                      "Number of crop categories")) %>% 
  mutate(`Effect size` = round(estimate, 2),
         `Upper CI` = round(conf.high, 2),
         `Lower CI` = round(conf.low, 2)) %>% 
  select(Variable, `Effect size`, `Upper CI`, `Lower CI`)

out <- sjPlot::plot_model(acreage_2021_bayes)
fixefdf_2021 <- data.frame(out[[1]]) %>% 
  mutate(Variable = c("Perceived importance of organic seed", 
                      "Seed quality distrust barrier",
                      "Buyer requirement barrier",
                      "Certifiers request organic seed use", 
                      "Limited seed traits barrier",
                      "Organic research funding",
                      "Number of registered organically bred varieties",
                      "Ratio of organic growers to supply chain actors",
                      "Seed availability barrier",
                      "Organic seed company sales",
                      "Saves own seed",
                      "Farm size (log acres)",
                      "Number of crop categories")) %>% 
  mutate(`Effect size` = round(estimate, 2),
         `Upper CI` = round(conf.high, 2),
         `Lower CI` = round(conf.low, 2)) %>% 
  select(Variable, `Effect size`, `Upper CI`, `Lower CI`)

ranef(acreage_bayes)$Region
ranef(acreage_bayes)$maincrop
ranef(acreage_bayes)$survey_year

text <- df %>% 
  filter(!(barrier_text %in% c("", "-2", "-1"))) %>% 
  select(survey_year, contains('barrier_'))
