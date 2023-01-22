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
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
setwd("~/Documents/Davis/R-Projects/organicseed_adoption/")
source('code/functions_model.R')
df.full <- readRDS("~/Box/organicseed_adoption/df_full.rds")
dfm1 <- readRDS("~/Box/organicseed_adoption/dfm1.rds")

greens <- colorRampPalette(c("lightgrey", "#55752f"))(8) # 2014-2018
reds <- colorRampPalette(c("lightgrey", "#90353b"))(8) # 2002-2008
blues <- colorRampPalette(c("lightgrey", "#1A476f"))(8) # 2009-2013

# 1. Select and scale data ----
# Considering acreage as the dependent variable 
## Use c() when scaling numbers because it "strips" the dimension (i.e. scaling converts them into some kind of matrix and Stack Overflow told me that c would strip this)


# Selecting models ----
# Overall, nothing is horribly different from one another. When I run the 2016 on its own, I get a singularity problem, so I might prefer the Bayesian approach. 
# I would like to make coefficient plots of all four models: All years, then 2011, 2016, and 2018. 
# It is hard to make a stargazer table from a Bayesian output.
# It is also nearly impossible to make a coefficient plants with multiple models together. jtools::plot_coefs can do it for just the fixed effcts, but so I decided to duplicate it with random effects, by extracting the random effetcs from the sjr::plot_summs. It is messy...

# Full model function for 4 different estimations: LM, GLM, BRMS ----
## Make 4 different equations

# Linear approaches ----
acreage_lm <- fullmodel_fx(data = dfm1, model_type = "lm")
summary(acreage_lm)
acreage_lmer <- fullmodel_fx(data = dfm1, model_type = "lmer")
summary(acreage_lmer)

# Horrible
acreage_bayeslm <- fullmodel_fx(data = dfm1, model_type = "bayes one-level")
summary(acreage_bayeslm)
acreage_bayeslmer <- fullmodel_fx(data = dfm1, model_type = "bayes multi-level")
summary(acreage_bayeslmer)
acreage_bayeslmer <- fullmodel_fx(data = dfm1, model_type = "normal inla multi-level")
summary(acreage_bayeslmer)

# Binary approaches ----
## This doesn't seem to default to 0-1
acreage_glm <- fullmodel_fx(data = dfm1, model_type = "binomial one-level")
summary(acreage_glm) # Set to quasibinomial to make it a little happer with non-integer DVs
acreage_glmer <- fullmodel_fx(data = dfm1, model_type = "binomial multi-level")
summary(acreage_glmer) # Doesn't accept quasibinomial as a family
acreage_glmer <- fullmodel_fx(data = dfm1, model_type = "binomial inla multi-level")
summary(acreage_glmer) # Doesn't accept quasibinomial as a family

# Beta approaches ----
acreage_beta <- fullmodel_fx(data = dfm1, model_type = "beta inla one-level")
summary(acreage_beta)
which(acreage_beta$cpo$failure > 0)
acreage_beta_multi <- fullmodel_fx(data = dfm1, model_type = "beta inla multi-level")
summary(acreage_beta_multi)
which(acreage_beta_multi$cpo$failure > 0)

acreage_beta_multi$waic$waic
acreage_glmer$waic$waic
acreage_bayeslmer$waic$waic

# 
plot(acreage_beta_multi, plot.fixed.effects=FALSE, 
     plot.lincomb=FALSE, plot.random.effects=FALSE,
     plot.hyperparameters=FALSE, plot.predictor=FALSE, 
     plot.q=FALSE, plot.cpo=TRUE, single=FALSE)
plot(acreage_lmer, plot.fixed.effects=FALSE, plot.lincomb=FALSE, 
     plot.random.effects=FALSE,
     plot.hyperparameters=FALSE, plot.predictor=FALSE, 
     plot.q=FALSE, plot.cpo=TRUE,
     single=FALSE)
plot(acreage_glmer, plot.fixed.effects=FALSE, plot.lincomb=FALSE, plot.random.effects=FALSE,
     plot.hyperparameters=FALSE, plot.predictor=FALSE, plot.q=FALSE, plot.cpo=TRUE,
     single=FALSE)

# Comparing multi-level ----
sum_lmer <- summary(acreage_lmer)
sum_lmer <- sum_lmer$coefficients
sum_lmer <- as.data.frame(sum_lmer) %>% 
  mutate(term = row.names(.)) %>% 
  rename("Est lmer" = Estimate,
         "SE lmer" = "Std. Error")
sum_bayeslmer <- data.frame(sjPlot::plot_model(acreage_bayeslmer)[[1]]) %>% 
  rename("Est bayes" = estimate,
         "SE bayes" = "std.error")
sum_glmer <- summary(acreage_glmer)
sum_glmer <- sum_glmer$coefficients
sum_glmer <- as.data.frame(sum_glmer) %>% 
  mutate(term = row.names(.),
         Estimate = (exp(Estimate)-1),
         `Std. Error` = (exp(`Std. Error`)-1)) %>% 
  rename("Est glmer" = Estimate,
         "SE glmer" = "Std. Error")
sum_beta_m <- summary(acreage_beta_multi)
sum_beta_m <- sum_beta_m$fixed
sum_beta_m <- as.data.frame(sum_beta_m) %>% 
  mutate(term = row.names(.),
         se = sd/sqrt(nrow(dfm1))) %>% 
  rename("Est beta_m" = mean,
         "SE beta_m" = se)

summarytable <- full_join(sum_lmer, sum_bayeslmer) %>% full_join(sum_glmer) %>% full_join(sum_beta_m)
summarytable2 <- summarytable[,c(4,1:3,5:8,18:21,22:23,29,24,26)]

jtools::plot_coefs(acreage_lmer, acreage_bayeslmer,
                   legend.title = "", 
                   model.names = c("Freq lmer", "Bayesian lmer"), 
                   colors = c("darkblue", "darkorange"),
                   coefs = coefs) +
  labs(title = "lmer") +
  theme(legend.position = "bottom")
ggsave(filename = "tables/figureS5_modellmer.png", width = 6, height = 4.5)

jtools::plot_coefs(acreage_glmer, 
                   exp = T,
                   legend.title = "", 
                   model.names = "Freq GLM", 
                   colors = "darkgreen",
                   coefs = coefs) +
  labs(title = "glmer")
ggsave(filename = "tables/figureS5_modelglmer.png", width = 6, height = 3.5)

mod <- acreage_beta_multi$summary.fixed %>% 
  select(mean, sd, `0.025quant`,`0.975quant`) %>% 
  slice(-1) %>% 
  mutate(parameter = factor(names(coefs), levels = rev(names(coefs)))) %>% 
  rename("lower" = "0.025quant", "upper" = "0.975quant")

ggplot(mod, aes(x=parameter, y=mean)) +
  geom_hline(yintercept=0, linetype='dashed')+
  geom_blank()+
  geom_linerange(aes(ymin=lower, ymax=upper),
                 color = "darkred") +
  geom_point(aes(x = parameter), shape=21, size = 3, 
             fill = "white", color = "darkred")+
  coord_flip()+
  theme_nice(legend.pos = "right") + 
  drop_y_gridlines() + 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 10), 
        #legend.position="none",
        panel.grid.major.x = element_line(linetype = "solid")) + 
  labs(x = "Intercept", shape = "", color = "", title = "beta",
       y = "Estimate (Proportion)")

ggsave(filename = "tables/figureS5_modelbeta.png", width = 6, height = 3.5)


# yearly models ----


onelevel_prop_11_16 <- paste0(iv_prop, dvs_core_noIP, subsyst_onelevel_noyear)
onelevel_prop_21 <- paste0(iv_prop, dvs_core, subsyst_onelevel_noyear)
onelevel_prop_11_16 <- paste0(iv_prop, dvs_core_noIP, subsyst_onelevel_noyear)
onelevel_prop_21 <- paste0(iv_prop, dvs_core, subsyst_onelevel_noyear)
onelevel_perc_11_16 <- paste0(iv_perc, dvs_core_noIP, subsyst_onelevel_noyear)
onelevel_perc_21 <- paste0(iv_perc, dvs_core, subsyst_onelevel_noyear)
multilevel_prop_11_16 <- paste0(iv_prop, dvs_core_noIP, subsyst_multilevel_noyear)
multilevel_prop_21 <- paste0(iv_prop, dvs_core, subsyst_multilevel_noyear)
multilevel_perc_11_16 <- paste0(iv_perc, dvs_core_noIP, subsyst_multilevel_noyear)
multilevel_perc_21 <- paste0(iv_perc, dvs_core, subsyst_multilevel_noyear)
multilevel_prop_inla_11_16 <- paste0(iv_prop, dvs_core_noIP, subsyst_multilevel_inla_noyear)
multilevel_prop_inla_21 <- paste0(iv_prop, dvs_core, subsyst_multilevel_inla_noyear)


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
acreage_2011_beta <- yearlymodel_fx(data = df2011, 
                                    model_type = "beta inla multi-level", 
                                    year = 2011)
acreage_2016_beta <- yearlymodel_fx(data = df2016, 
                                    model_type = "beta inla multi-level", 
                                    year = 2016)
acreage_2021_beta <- yearlymodel_fx(data = df2021, 
                                    model_type = "beta inla multi-level", 
                                    year = 2021)
acreage_2011_glmer <- yearlymodel_fx(data = df2011, 
                                     model_type = "binomial multi-level", 
                                     year = 2011)
acreage_2016_glmer <- yearlymodel_fx(data = df2016, 
                                     model_type = "binomial multi-level", 
                                     year = 2016)
acreage_2021_glmer <- yearlymodel_fx(data = df2021, 
                                     model_type = "binomial multi-level", 
                                     year = 2021)

# This is the one I want for fixed effects:
jtools::plot_coefs(acreage_2011_bayes, 
                   acreage_2016_bayes, 
                   acreage_2021_bayes,
                   legend.title = "", 
                   model.names = c("2010", "2015", "2020"), 
                   colors = c(greens[5], reds[5], blues[5]),  
                   coefs = coefs) +
  #theme(legend.position = "bottom") +
  labs(title = "Bayes lmer")
ggsave(filename = "tables/figureS5_modellmer_yrly.png", width = 8, height = 4.5)

jtools::plot_coefs(acreage_2011_glmer, 
                   acreage_2016_glmer, 
                   acreage_2021_glmer,
                   exp = T,
                   legend.title = "", 
                   model.names = c("2010", "2015", "2020"), 
                   colors = c(greens[5], reds[5], blues[5]),  
                   coefs = coefs) +
  #theme(legend.position = "bottom") +
  labs(title = "glmer")
ggsave(filename = "tables/figureS5_modelglmer_yrly.png", width = 8, height = 4.5)

mod1 <- rbind(acreage_2011_beta$summary.fixed, 
              acreage_2016_beta$summary.fixed) %>% 
  slice(-c(1, 14)) %>% 
  select(mean, sd, `0.025quant`,`0.975quant`) %>% 
  mutate(parameter = rep(names(coefs[-7]), 2))
mod <- acreage_2021_beta$summary.fixed %>% 
  slice(-1) %>% 
  select(mean, sd, `0.025quant`,`0.975quant`) %>% 
  mutate(parameter = factor(names(coefs), levels = rev(names(coefs)))) %>% 
  rbind(mod1) %>% 
  rename("lower" = "0.025quant", "upper" = "0.975quant") %>% 
  mutate(year = c(rep(2021, 13), rep(2011, 12), rep(2016, 12)))

ggplot(mod, aes(x = mean, 
                y = parameter)) +
  ggstance::geom_pointrangeh(aes(xmin = lower, 
                                 xmax = upper,
                                 color = factor(year, 
                                                levels = c("2021", "2016", "2011")), 
                                 shape = factor(year, 
                                                levels = c("2021", "2016", "2011"))),
                             position = ggstance::position_dodgev(height = .6), 
                             fill = "white", fatten = 3, size = 0.8) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.25) + 
  scale_colour_manual(values = c(greens[5], reds[5], blues[5]),
                      limits = levels(factor(mod$year))) + 
  scale_shape_manual(limits = levels(factor(mod$year)), 
                     values = c(21, 22, 23)) +
  theme_nice(legend.pos = "right") +
  drop_y_gridlines() + 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 10), 
        #legend.position="none",
        panel.grid.major.x = element_line(linetype = "solid")) + 
  labs(x = "Intercept", shape = "", color = "", title = "beta")
ggsave(filename = "tables/figureS5_modelbeta_yrly.png", width = 8, height = 4.5)

