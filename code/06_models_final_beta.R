library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(jtools)
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
setwd("~/Documents/Davis/R-Projects/organicseed_adoption/")
df.full <- readRDS("~/Box/organicseed_adoption/df_full.rds") %>% 
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

cens <- 0.001
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
           control.compute = list(cpo= T, waic= T),
           control.family = list(beta.censor.value = cens))

mdf <- prep_inla_data(m)
plot_inla_models(mdf, 1)

ggsave("figures/model_results.png", width = 8, height = 4)

# PREDICTED PROBABILITY ----
## Two farm models in several settings ----
focal_crop = "Field"
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
df <- select(df_og, colnames(df.full))

newdf <- rbind(df.full, df)

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
         control.family = list(beta.censor.value = cens,
                               initial = 10, fixed=TRUE), ## no observational noise 
         
         control.predictor=list(link=1)) 


## What is this? Basically it fits the values of these data and then pretends like
## they are the predictors...
r$summary.linear.predictor[c((nrow(newdf)-3):nrow(newdf)), c("mean", "sd",
                                                            "0.025quant", 
                                                            "0.975quant")]
r$summary.fitted.values[c((nrow(newdf)-3):nrow(newdf)), c("mean", "sd",
                                                         "0.025quant", 
                                                         "0.975quant")]
## Notice how for where we actually had data they change
r$summary.linear.predictor[1:5, c("mean", "sd",
                                  "0.025quant", 
                                  "0.975quant")]
r$summary.fitted.values[1:5, c("mean", "sd",
                               "0.025quant", 
                               "0.975quant")]

pred_df <- df_og %>% 
                  cbind(r$summary.fitted.values[c((nrow(newdf)-(nrow(df_og)-1)):nrow(newdf)), 
                                              c("mean", "sd",
                                                "0.025quant", 
                                                "0.975quant")])
pred_df  %>% 
  mutate(survey_year = case_when(survey_year == 2011 ~ 2010, 
                                 survey_year == 2016 ~ 2015, T ~2020)) %>% 
  ggplot(aes(x = model, y = mean, color = factor(Region, 
                                                 c("North Central", "Northeast",
                                                   "South", "West")))) +
  geom_point(size = 4, alpha = .8,) +
  geom_linerange(aes(ymin = `0.025quant`, ymax = `0.975quant`)) +
  facet_wrap(~survey_year) +
  theme_minimal() +
  scale_color_viridis_d(option = "D", begin = .1, end = .9) +
  theme(axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        text = element_text(size = 14, family = "Times"),
        panel.grid.major.x = element_line(linetype = "solid")) + 
  labs(x = "", shape = "", color = "", y = "Predicted acreage planted to conventional seed") +
  ylim(c(0,1))
