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
  select(CVseed_percent_maincrop_prop,
         legitimation_user_s,
         CertifierRequestn_s,
         noncommercial_seedsource_s,
         #acreage_log_s,
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
df.full_rounded$CVseed_percent_maincrop_prop <- ifelse(df.full_rounded$CVseed_percent_maincrop_prop == 1, 0.99, ifelse(df.full_rounded$CVseed_percent_maincrop_prop == 0, 0.01, df.full_rounded$CVseed_percent_maincrop_prop))

m <- inla(CVseed_percent_maincrop_prop ~ 
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
           data = df.full_rounded,
           family = "beta",
           control.compute = list(cpo= T, waic= T))

mdf <- prep_inla_data(m)

plot_inla_models(mdf, 1)
ggsave("figures/figure2.png", width = 8, height = 4)

tablea3 <- mdf[,c('term', 'mean', 'se')]
tablea3$mean <- round(tablea3$mean, 3)
tablea3$se <- round(tablea3$se, 3)
write.csv(tablea3, 'tables/tablea3.csv', row.names = F)


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

newdf <- rbind(df.full_rounded, df)

# predictions with INLA
#pred_veg_west <- cbind(veg_west, predict(m3, 
#                                         newdata = veg_west, 
#                                         se=TRUE))
## https://www.r-inla.org/faq#h.821k2r53fvx3
## Basically the model predicts NAs, so you build in NAs to the model
## So you need to add a few more observations to the data frame but
## leave y as NA


#veg_west$CVseed_percent_maincrop_prop <- rep(NA, nrow(veg_west))
#veg_west_match <- select(veg_west, colnames(df.full))
#field_nc$CVseed_percent_maincrop_prop <- rep(NA, nrow(field_nc))
#field_nc_match <- select(field_nc, colnames(df.full))
#newdf <- rbind(df.full, veg_west_match) %>% 
#  rbind(field_nc_match)

# test one prediction on my own
summ <- summary(m)
dfw11
summ$fixed[2]*(dfw11$legitimation_user_s[1]) + 
  summ$fixed[3]*(dfw11$acreage_s[1]) + summ$fixed[4]*(dfw11$cropcat_n_s[1]) +
  summ$fixed[5]*(dfw11$barrier_buyerReq_s[1]) + summ$fixed[6]*(dfw11$barrier_insuffQuantity_s[1]) + 
  summ$fixed[7]*(dfw11$barrier_lackdesiredTraits_s[1]) + 
  summ$fixed[8]*(dfw11$barrier_availability_s[1]) +
  summ$fixed[9]*0 + summ$fixed[10]*1 +summ$fixed[11]*1 + summ$fixed[12]*0 +
  summ$fixed[13]*0 + summ$fixed[14]*0 + summ$fixed[15]*0 +
  summ$fixed[16]*(dfw11$CertifierRequestn_s[1]) + summ$fixed[17]*(dfw11$noncommercial_seedsource_s[1])


formula = CVseed_percent_maincrop_prop ~ legitimation_user_s + 
                                         CertifierRequestn_s + 
                                         noncommercial_seedsource_s + 
                                         acreage_s + 
                                         cropcat_n_s +
                                         #years_certified_s + 
                                         barrier_buyerReq_s +
                                         barrier_insuffQuantity_s +
                                         barrier_lackdesiredTraits_s +
                                         barrier_availability_s +
                                         maincrop +
                                         Region +
                                         survey_year

r = inla(formula, data = newdf,
         family = "beta",
         control.family = list(initial = 10, fixed=TRUE), ## no observational noise 
         control.predictor=list(link=1)) 

# Exploring what is happening here with NAs
## Basically it fits the values of these data and then pretends like
## they are the predictors... if you don't set link=1, then this does not happen
r$summary.linear.predictor[c((nrow(newdf)-3):nrow(newdf)), c("mean", "sd",
                                                            "0.025quant", 
                                                            "0.975quant")]
r$summary.fitted.values[c((nrow(newdf)-3):nrow(newdf)), c("mean", "sd",
                                                         "0.025quant", 
                                                         "0.975quant")]

pred_df <- df_og %>% 
                  cbind(r$summary.fitted.values[c((nrow(newdf)-
                                                     (nrow(df_og)-1)):nrow(newdf)), 
                                              c("mean", "sd",
                                                "0.025quant", 
                                                "0.975quant")])


plot_predicted_probs(pred_df)

ggsave('figures/figure3.png', width = 7, height = 4)

# Text
wide11 <- pred_df %>%
  select(survey_year, Region, model, mean) %>% 
  filter(survey_year == 2011) %>% 
  pivot_wider(names_from = model,
              values_from = mean) 

wide11$Practical-wide11$Principled

wide16 <- pred_df %>%
  select(survey_year, Region, model, mean) %>% 
  filter(survey_year == 2016) %>% 
  pivot_wider(names_from = model,
              values_from = mean) 
wide16$Practical-wide16$Principled

wide21 <- pred_df %>%
  select(survey_year, Region, model, mean) %>% 
  filter(survey_year == 2021) %>% 
  pivot_wider(names_from = model,
              values_from = mean) 
wide21$Practical-wide21$Principled
