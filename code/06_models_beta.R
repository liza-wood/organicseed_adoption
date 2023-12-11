library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(jtools)
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
source('code/functions.R')
setwd("~/Documents/Davis/R-Projects/organicseed_adoption/")
df.full <- readRDS("~/Box/organicseed_adoption/df_scaled.rds") %>% 
  #mutate(acreage_s = c(scale(acreage_log, center = T))) %>% 
  mutate(acreage_s = acreage_log,
         cropcat_n_s = cropcat_n) %>% 
  select(CVseed_percent_maincrop_prop,
         legitimation_user_s,
         CertifierRequestn_s,
         noncommercial_seedsource_s,
         acreage_s,
         cropcat_n_s,
         barrier_buyerReq_s,
         barrier_insuffQuantity_s,
         barrier_lackdesiredTraits_s,
         barrier_availability_s,
         maincrop,
         Region,
         survey_year) %>% 
  na.omit()

df.full_rounded <- df.full
df.full_rounded$CVseed_percent_maincrop_prop <- ifelse(df.full_rounded$CVseed_percent_maincrop_prop == 1, 0.999, ifelse(df.full_rounded$CVseed_percent_maincrop_prop == 0, 0.001, df.full_rounded$CVseed_percent_maincrop_prop))

#m <- inla(CVseed_percent_maincrop_prop ~ 
#             legitimation_user_s + 
#             acreage_s + 
#             cropcat_n_s +
#             barrier_buyerReq_s +
#             barrier_insuffQuantity_s +
#             barrier_lackdesiredTraits_s +
#             barrier_availability_s +
#             maincrop +
#             Region +
#             survey_year +
#             CertifierRequestn_s + 
#             noncommercial_seedsource_s, 
#           data = df.full_rounded,
#           family = "beta",
#           control.compute = list(cpo= T, waic= T))

#mdf <- prep_inla_data(m)
#plot_inla_models(mdf, 1)

# Check
library(betareg)
set.seed(19)
m <- betareg(CVseed_percent_maincrop_prop ~ 
               legitimation_user_s + 
               acreage_s + 
               cropcat_n_s +
               barrier_buyerReq_s +
               barrier_insuffQuantity_s +
               barrier_lackdesiredTraits_s +
               barrier_availability_s +
               maincrop +
               Region +
               survey_year +
               CertifierRequestn_s + 
               noncommercial_seedsource_s, 
             data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m)
mdf <- prep_betreg_data(m)
plot_inla_models(mdf, 1)
ggsave("figures/figure2.png", width = 8, height = 3.9)

tablea3 <- mdf[,c('term', 'mean', 'se')]
tablea3$mean <- round(tablea3$mean, 3)
tablea3$se <- round(tablea3$se, 3)
tablea3 <- tablea3 %>% 
  mutate(term = factor(term, rev(coef_order))) %>% 
  arrange(rev(term))
write.csv(tablea3, 'tables/tablea3.csv', row.names = F)

# So, the idea is to exponentiate the coefficient because it is reported as log odds
# https://stats.stackexchange.com/questions/297659/interpretation-of-betareg-coef
# I have checked in this in appendices, bonus beta check
# As SO says, "It's just not a change in "odds" (= ratio of probabilities) but a change in a ratio of proportions.
## "If the temperature coef is .05 so exp(.05) = 1.05 that would say a one unit increase in temp leads to a 1.05 increase in what?"
## Response: "it represents an increase in the ratio of proportion species observed to proportion of species not observed. The odds is just the ratio between positive and negative classes (p/1-p), so rather than saying "odds" you can just describe the ratio explicitly."
## "the interpretation would be: one unit increase in temp leads to 5% increase in the ratio of proportion species observed to proportion of species not observed. or simply, one unit increase in temp leads to 5% increase in the ratio of proportion species observed"

m_summ <- summary(m)
exp(m_summ$coefficients$mean)

# PREDICTED PROBABILITY ----
## Two farm models in several settings ----
focal_crop = "Vegetables"
dfw11 <- make_pp_df(filter(df.full, Region == "West" &
                              maincrop == focal_crop & 
                              survey_year == 2011))
dfs11 <- make_pp_df(filter(df.full, Region == "South" &
                   maincrop == focal_crop & 
                   survey_year == 2011))
dfnc11 <- make_pp_df(filter(df.full, Region == "North Central" &
                    maincrop == focal_crop & 
                    survey_year == 2011))
dfne11 <- make_pp_df(filter(df.full, Region == "Northeast" &
                    maincrop == focal_crop & 
                    survey_year == 2011))
dfw16 <- make_pp_df(filter(df.full, Region == "West" &
                          maincrop == focal_crop & 
                          survey_year == 2016))
dfs16 <- make_pp_df(filter(df.full, Region == "South" &
                   maincrop == focal_crop & 
                   survey_year == 2016))
dfnc16 <- make_pp_df(filter(df.full, Region == "North Central" &
                   maincrop == focal_crop & 
                   survey_year == 2016))
dfne16 <- make_pp_df(filter(df.full, Region == "Northeast" &
                    maincrop == focal_crop & 
                    survey_year == 2016))
dfw21 <- make_pp_df(filter(df.full, Region == "West" &
                   maincrop == focal_crop & 
                   survey_year == 2021))
dfs21 <- make_pp_df(filter(df.full, Region == "South" &
                   maincrop == focal_crop & 
                   survey_year == 2021))
dfnc21 <- make_pp_df(filter(df.full, Region == "North Central" &
                    maincrop == focal_crop & 
                    survey_year == 2021))
dfne21 <- make_pp_df(filter(df.full, Region == "Northeast" &
                    maincrop == focal_crop & 
                    survey_year == 2021))


df_og <- rbind(dfw11, dfw16, dfw21, dfs11, dfs16, dfs21,
      dfnc11, dfnc16, dfnc21, dfne11, dfne16, dfne21)
df_og$CVseed_percent_maincrop_prop <- rep(NA, nrow(df_og))
df <- select(df_og, colnames(df.full_rounded))

df$mean <- predict(b, newdata = df)
df$model <- ifelse(df$legitimation_user_s < 0, "Practical", "Principled")
plot_predicted_probs(df, .15, .85)

ggsave('figures/figure3.png', width = 7, height = 4)

# Text
wide11 <- df %>%
  select(survey_year, Region, model, mean) %>% 
  filter(survey_year == 2011) %>% 
  pivot_wider(names_from = model,
              values_from = mean) 

wide11$Practical-wide11$Principled

wide16 <- df %>%
  select(survey_year, Region, model, mean) %>% 
  filter(survey_year == 2016) %>% 
  pivot_wider(names_from = model,
              values_from = mean) 
wide16$Practical-wide16$Principled

wide21 <- df %>%
  select(survey_year, Region, model, mean) %>% 
  filter(survey_year == 2021) %>% 
  pivot_wider(names_from = model,
              values_from = mean) 
wide21$Practical-wide21$Principled

