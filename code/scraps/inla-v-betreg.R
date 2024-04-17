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

summary(m)

library(betareg)
b <- betareg(CVseed_percent_maincrop_prop ~ 
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
summary(b)

# Running the check on what "change in ratio of proportion" means from SO
# https://stats.stackexchange.com/questions/297659/interpretation-of-betareg-coef
df_select <- df.full[df.full$Region == "North Central" & df.full$maincrop == "Field" &
                df.full$survey_year == 2011,]
df <- df_select[-1,]
# Set everything to its mean
df[,1:10] <- 0
df$cropcat_n_s <- mean(df_select$cropcat_n_s)
df$acreage_s <- mean(df_select$acreage_s)
# Then adjust
df$legitimation_user_s[1:2] <- c(-0.5, .5)

## Now check as they advise
df$prop <- predict(b, newdata = df)
df$proportion_ratio <- df$prop/(1 - df$prop)
exp(coef(b))[2] # this should be same as below
df$proportion_ratio[2]/df$proportion_ratio[1]
# Checking so yes, I think this makes sense

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

## With betreg
fitted_values <- predict(b, type = "response")
observed_values <- df.full_rounded$CVseed_percent_maincrop_prop
residuals <- observed_values - fitted_values
qqnorm(residuals)
qqline(residuals)
cor(observed_values, fitted_values)

plot(residuals ~ fitted_values, main = "Residual Plot", xlab = "Fitted Values", ylab = "Residuals")
df$prop <- predict(b, newdata = df, se.fit = T)
df$model <- ifelse(df$legitimation_user_s < 0, "Practical", "Principled")

df  %>% 
  mutate(survey_year = case_when(survey_year == 2011 ~ 2010, 
                                 survey_year == 2016 ~ 2015, T ~2020)) %>% 
  ggplot(aes(x = model, y = prop, color = factor(Region, 
                                                 c("North Central", "Northeast",
                                                   "South", "West")))) +
  geom_point(size = 4, alpha = .8,) +
  geom_line(aes(group = factor(Region, 
                               c("North Central", "Northeast",
                                 "South", "West")))) +
  facet_wrap(~survey_year) +
  theme_minimal() +
  scale_color_viridis_d(option = "D", begin = .1, end = .9) +
  theme(legend.position = "bottom",
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        text = element_text(size = 14, family = "Times"),
        panel.grid.major.x = element_line(linetype = "solid")) + 
  labs(x = "", shape = "", color = "", y = "Predicted acreage planted\nto conventional seed") +
  ylim(c(0,1))

## With inla 
# https://www.r-inla.org/faq#h.821k2r53fvx3
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
r$summary.fitted.values[1:10, c("mean", "sd", "0.025quant", "0.975quant")]


r$summary.linear.predictor[c((nrow(newdf)-3):nrow(newdf)), c("mean", "sd",
                                                             "0.025quant", 
                                                             "0.975quant")]
r$summary.fitted.values[c((nrow(newdf)-3):nrow(newdf)), c("mean", "sd",
                                                          "0.025quant", 
                                                          "0.975quant")]
# So then add on the predicted values
pred_df <- df_og %>% 
  cbind(r$summary.fitted.values[c((nrow(newdf)-
                                     (nrow(df_og)-1)):nrow(newdf)), 
                                c("mean", "sd",
                                  "0.025quant", 
                                  "0.975quant")])
