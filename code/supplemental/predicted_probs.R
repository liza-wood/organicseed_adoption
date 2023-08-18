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

df.full_rounded <- df.full
df.full_rounded$CVseed_percent_maincrop_prop <- ifelse(df.full_rounded$CVseed_percent_maincrop_prop == 1, 0.99, ifelse(df.full_rounded$CVseed_percent_maincrop_prop == 0, 0.01, df.full_rounded$CVseed_percent_maincrop_prop))

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
df <- select(df_og, colnames(df.full_rounded))

newdf <- rbind(df.full_rounded, df)

r = inla(formula, data = newdf,
         family = "beta",
         control.family = list(initial = 10, fixed=TRUE), ## no observational noise 
         control.predictor=list(link=1)) 

pred_df <- df_og %>% 
  cbind(r$summary.fitted.values[c((nrow(newdf)-
                                     (nrow(df_og)-1)):nrow(newdf)), 
                                c("mean", "sd",
                                  "0.025quant", 
                                  "0.975quant")])

plot_predicted_probs(pred_df)
ggsave('figures/sm_predicted_probs_field.png', width = 7, height = 4)

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

focal_crop = "Forage"
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

r = inla(formula, data = newdf,
         family = "beta",
         control.family = list(initial = 10, fixed=TRUE), ## no observational noise 
         control.predictor=list(link=1)) 

pred_df <- df_og %>% 
  cbind(r$summary.fitted.values[c((nrow(newdf)-
                                     (nrow(df_og)-1)):nrow(newdf)), 
                                c("mean", "sd",
                                  "0.025quant", 
                                  "0.975quant")])

plot_predicted_probs(pred_df)
ggsave('figures/sm_predicted_probs_forage.png', width = 7, height = 4)

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
