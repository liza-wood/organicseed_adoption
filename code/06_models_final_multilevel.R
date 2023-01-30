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
df.full <- readRDS("~/Box/organicseed_adoption/df_full.rds")
dfm1 <- readRDS("~/Box/organicseed_adoption/dfm1.rds")

greens <- colorRampPalette(c("lightgrey", "#55752f"))(8) # 2014-2018
reds <- colorRampPalette(c("lightgrey", "#90353b"))(8) # 2002-2008
blues <- colorRampPalette(c("lightgrey", "#1A476f"))(8) # 2009-2013

# For the equation ----
modelforeq <- lmer(eval(parse(text=multilevel_perc)),
                 data = dfm1)
equatiomatic::extract_eq(modelforeq)

# 1. FINAL MODEL: FIXED EFFECTS ----
acreage_bayes <- fullmodel_fx(data = dfm1, 
                                     model_type = "bayes multi-level")

df2011 <- filter(dfm1, survey_year == 2011)
df2016 <- filter(dfm1, survey_year == 2016)
df2021 <- filter(dfm1, survey_year == 2021)

acreage_2011_bayes <- yearlymodel_fx(data = df2011, 
                                     model_type = "bayes multi-level", 
                                     year = 2011)
acreage_2016_bayes <- yearlymodel_fx(data = df2016, 
                                     model_type = "bayes multi-level", 
                                     year = 2016)
acreage_2021_bayes <- yearlymodel_fx(data = df2021, 
                                     model_type = "bayes multi-level", 
                                     year = 2021)

jtools::plot_coefs(acreage_2011_bayes, 
                   acreage_2016_bayes, 
                   acreage_2021_bayes,
                   legend.title = "", 
                   model.names = c("2010", "2015", "2020"), 
                   colors = c(greens[5], reds[5], blues[5]),  
                   coefs = coefs) +
  theme(legend.position = "bottom") +
  labs(title = "")
ggsave(filename = "tables/figure5a.jpeg", width = 8, height = 5.5)

# 2. TABLE FIXED EFFECTS ----
tbl11 <- data.frame(sjPlot::plot_model(acreage_2011_bayes)[[1]])[c(1,2,4,5)] %>% 
mutate(`2010 Effect` = round(estimate, 2),
         `2010 U CI` = round(conf.high, 2),
         `2010 L CI` = round(conf.low, 2)) %>% 
  select(-c(estimate, conf.low, conf.high))
tbl16 <- data.frame(sjPlot::plot_model(acreage_2016_bayes)[[1]])[c(1,2,4,5)] %>% 
  mutate(`2015 Effect` = round(estimate, 2),
         `2015 U CI` = round(conf.high, 2),
         `2015 L CI` = round(conf.low, 2)) %>% 
  select(-c(estimate, conf.low, conf.high))
tbl21 <- data.frame(sjPlot::plot_model(acreage_2021_bayes)[[1]])[c(1,2,4,5)] %>% 
  mutate(`2020 Effect` = round(estimate, 2),
         `2020 U CI` = round(conf.high, 2),
         `2020 L CI` = round(conf.low, 2)) %>% 
  select(-c(estimate, conf.low, conf.high))
tbl <- left_join(tbl21, tbl16) %>% left_join(tbl11) %>% 
  mutate(term = names(coefs)) %>% 
  rename("Variable" = term) 

write.csv(tbl, "tables/tableS7.csv", row.names = F)

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

ranefdf <- rbind(#ranefdf_full, 
                 ranefdf11, ranefdf16, ranefdf21)
ranefdf$model <- factor(ranefdf$model, c(#"All years", 
                                         "2010", "2015", "2020"))
pd <- position_dodge(width=0.75)
ggplot(ranefdf, aes(x = estimate, 
                    y = factor(term, rev(unique(ranefdf$term))))) +
  #geom_point(size=3,  position = pd, fill = "white", shape = c(rep(1,10), rep(2, 7),
  #                                                             rep(24,7), rep(25,7))) +
  ggstance::geom_pointrangeh(aes(xmin = conf.low, 
                                 xmax = conf.high,
                                 color = factor(model, rev(c(#"All years",
                                                             "2020", "2015", "2010"))), 
                                 shape = factor(model, rev(c(#"All years",
                                                             "2020", "2015", "2010")))),
                             position = ggstance::position_dodgev(height = .6), 
                             fill = "white", fatten = 3, size = 0.8) +
  #geom_errorbar(aes(xmin=`conf.high`,xmax=`conf.low`), width=0.1, position = pd) +
  geom_vline(xintercept = 0, linetype = 2, size = 0.25) + 
  scale_colour_manual(values = c(#"black", 
                                        greens[5], reds[5], blues[5]),
                      limits = levels(ranefdf$model)) + 
  scale_shape_manual(limits = levels(ranefdf$model), 
                       values = #21
                     22:24) +
  theme_nice(legend.pos = "right") + 
  drop_y_gridlines() + 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 10), 
        #legend.position="none",
        panel.grid.major.x = element_line(linetype = "solid")) + 
  labs(x = "Intercept", shape = "", color = "")
ggsave("tables/figure5b.jpeg", width = 8, height = 4)

