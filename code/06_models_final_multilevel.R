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

modelforeq <- lmer(OGseed_percent_maincrop_prop ~ legitimation + 
                                                barrier_distrustQuality_s +
                                                barrier_buyerReq_s +
                                                CertifierRequestn + 
                                                barrier_lackdesiredTraits_s +
                                                funding_100thous + 
                                                organicip_10 +
                                                ratio +
                                                barrier_availability_s +
                                                sales_mill + 
                                                SeedSourceOwn_s +
                                                acreage_log +  
                                                cropcat_n + 
                                                (1|maincrop) +
                                                (1|Region) +
                                                (1|survey_year),
                 data = dfm1)
equatiomatic::extract_eq(modelforeq)

model <- inla(OGseed_percent_maincrop_prop ~ legitimation + 
                    barrier_distrustQuality_s +
                    barrier_buyerReq_s +
                    CertifierRequestn + 
                    barrier_lackdesiredTraits_s +
                    funding_100thous + 
                    organicip_10 +
                    ratio +
                    barrier_availability_s +
                    sales_mill + 
                    SeedSourceOwn_s +
                    acreage_log +  
                    cropcat_n + 
                    f(maincrop, model = 'iid') +
                    f(Region, model = 'iid') +
                    f(survey_year, model = 'iid')
                  data = dfm1,
                  family = "beta",
                  ontrol.compute = list(cpo=TRUE, waic=TRUE),
                  control.family = list(beta.censor.value = 0.01))


# 1. FINAL MODEL: FIXED EFFECTS ----

# 2. FINAL MODEL: RANDOM EFFECTS ----
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

# 3. TABLE ----
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
