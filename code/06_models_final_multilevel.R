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

# Selecting models ----
# Overall, nothing is horribly different from one another. When I run the 2016 on its own, I get a singularity problem, so I might prefer the Bayesian approach. 
# I would like to make coefficient plots of all four models: All years, then 2011, 2016, and 2018. 
# It is hard to make a stargazer table from a Bayesian output.
# It is also nearly impossible to make a coefficient plants with multiple models together. jtools::plot_coefs can do it for just the fixed effcts, but so I decided to duplicate it with random effects, by extracting the random effetcs from the sjr::plot_summs. It is messy...

# Full model ----
fullmodel_fx <- function(data, model_type = "lm"){
  if(model_type == "bayes"){
    model <- brm(OGseed_percent_maincrop ~ 
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

yearlymodel_fx <- function(data, model_type = "lm"){
  if(model_type == "bayes"){
    model <- brm(OGseed_percent_maincrop ~ 
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
  return(model)
}
df2011 <- filter(dfm1, survey_year == 2011)
df2016 <- filter(dfm1, survey_year == 2016)
df2021 <- filter(dfm1, survey_year == 2021)

acreage_2011_bayes <- yearlymodel_fx(data = df2011, model_type = "bayes")
acreage_2016_bayes <- yearlymodel_fx(data = df2016, model_type = "bayes")
acreage_2021_bayes <- yearlymodel_fx(data = df2021, model_type = "bayes")

# This is the one I want for fixed effects:
jtools::plot_coefs(acreage_bayes, acreage_2011_bayes, acreage_2016_bayes, acreage_2021_bayes,
                   legend.title = "", 
                   model.names = c("All years", "2004-2010", "2011-2015", "2016-2020"), 
                   colors = c("black", reds[5], blues[5], greens[5]),  
                   coefs = c("Legitimation:\nOrganic seed impacts success" = "legitimation", 
                             "Guidance:\nCertifiers request organic seed use" = "CertifierRequestn", 
                             "Resource mobilization:\nOrganic research funding (million USD)" = "funding_million", 
                             "Entrepreneurship:\nRatio of organic growers to supply chain actors" = "ratio",
                             "Knowledge development:\nOrganically bred varieties" = "organicip",
                             "Control: Farm size (log acres)" = "acreage_log",
                             "Control: Number of crop categories" = "cropcat_n"))
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
    mutate(model = year_in_model)
  return(ranefdf)
}
ranefdf11 <- ranef_table_yrly(acreage_2011_bayes, 2011)
ranefdf16 <- ranef_table_yrly(acreage_2016_bayes, 2016)
ranefdf21 <- ranef_table_yrly(acreage_2021_bayes, 2021)

ranefdf <- rbind(ranefdf_full,ranefdf11, ranefdf16, ranefdf21)
pd <- position_dodge(width=0.75)
ggplot(ranefdf, aes(x = estimate, y = factor(term, rev(unique(ranefdf$term))), 
                    color = model, shape = model)) +
  geom_point(size=3,  position = pd, fill = "white", shape = c(rep(1,10), rep(2, 7),
                                                               rep(24,7), rep(25,7))) +
  geom_errorbar(aes(xmin=`conf.high`,xmax=`conf.low`), width=0.1, position = pd) +
  geom_vline(xintercept = 0, linetype = 2, size = 0.25) + 
  scale_colour_manual(values = c("black", greens[3:5])) + 
  theme_nice(legend.pos = "right") + 
  drop_y_gridlines() + 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 10), 
        panel.grid.major.x = element_line(linetype = "solid")) + 
  labs(x = "Intercept", shape = "", color = "")
