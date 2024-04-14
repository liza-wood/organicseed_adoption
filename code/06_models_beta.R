library(dplyr)
library(tidyr)
library(betareg)
library(ggplot2)
library(cowplot)
library(jtools)
source('code/functions.R')

df.full <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled.rds") %>% 
  select(OGseed_percent_maincrop_prop,
         OG_value_s,
         CertifierRequestn,
         noncommercial_seedsource_s,
         acreage_log_s,
         cropcat_n_s,
         #barrier_buyerReq_s,
         barrier_insuffQuantity_s,
         barrier_lackdesiredTraits_s,
         barrier_availability_s,
         maincrop,
         Region,
         survey_year) %>% 
  na.omit()

df.full_rounded <- df.full
df.full_rounded$OGseed_percent_maincrop_prop <- ifelse(df.full_rounded$OGseed_percent_maincrop_prop == 1, 0.999, ifelse(df.full_rounded$OGseed_percent_maincrop_prop == 0, 0.001, df.full_rounded$OGseed_percent_maincrop_prop))

# MODEL ----
set.seed(19)
m <- betareg(OGseed_percent_maincrop_prop ~ 
               barrier_availability_s +
               barrier_lackdesiredTraits_s +
               barrier_insuffQuantity_s +
               survey_year +
               OG_value_s + 
               acreage_log_s + 
               cropcat_n_s +
               CertifierRequestn +
               noncommercial_seedsource_s +
               maincrop +
               Region, 
             data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m)
mdf <- prep_betreg_data(m)
plot_betareg_models(mdf, 1)
ggsave("figures/figure2.png", width = 8, height = 3.9)
saveRDS(m, 'tables/append_fullmodel.RDS')

tablea3 <- mdf[,c('term', 'mean', 'se')]
tablea3$mean <- round(tablea3$mean, 3)
tablea3$se <- round(tablea3$se, 3)
tablea3 <- tablea3 %>% 
  mutate(term = factor(term, rev(coef_order))) %>% 
  arrange(rev(term))
write.csv(tablea3, 'tables/append_fullmodel.csv', row.names = F)

# So, the idea is to exponentiate the coefficient because it is reported as log odds
# https://stats.stackexchange.com/questions/297659/interpretation-of-betareg-coef
# I have checked in this in appendices, bonus beta check
# As SO says, "It's just not a change in "odds" (= ratio of probabilities) but a change in a ratio of proportions.
## "If the temperature coef is .05 so exp(.05) = 1.05 that would say a one unit increase in temp leads to a 1.05 increase in what?"
## Response: "it represents an increase in the ratio of proportion species observed to proportion of species not observed. The odds is just the ratio between positive and negative classes (p/1-p), so rather than saying "odds" you can just describe the ratio explicitly."
## "the interpretation would be: one unit increase in temp leads to 5% increase in the ratio of proportion species observed to proportion of species not observed. or simply, one unit increase in temp leads to 5% increase in the ratio of proportion species observed"

m_summ <- summary(m)
exp(m_summ$coefficients$mean)-1

# PREDICTED PROBABILITY ----
## Vegetables ----
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
df_og$OGseed_percent_maincrop_prop <- rep(NA, nrow(df_og))
df <- select(df_og, colnames(df.full_rounded))

df$mean <- predict(m, newdata = df)
df$model <- ifelse(df$OG_value_s < 0, "Practical", "Principled")
plot_predicted_probs(df, .35, .8)

ggsave('figures/figure3.png', width = 7, height = 4)

# Text
wide11 <- df %>%
  select(survey_year, Region, model, mean) %>% 
  filter(survey_year == 2011) %>% 
  pivot_wider(names_from = model,
              values_from = mean) 

sort(wide11$Principled-wide11$Practical)

wide16 <- df %>%
  select(survey_year, Region, model, mean) %>% 
  filter(survey_year == 2016) %>% 
  pivot_wider(names_from = model,
              values_from = mean) 
sort(wide16$Principled-wide16$Practical)

wide21 <- df %>%
  select(survey_year, Region, model, mean) %>% 
  filter(survey_year == 2021) %>% 
  pivot_wider(names_from = model,
              values_from = mean) 
sort(wide21$Principled-wide21$Practical)

## Field ----
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
df_og$OGseed_percent_maincrop_prop <- rep(NA, nrow(df_og))
df <- select(df_og, colnames(df.full_rounded))

df$mean <- predict(m, newdata = df)
df$model <- ifelse(df$OG_value_s < 0, "Practical", "Principled")
plot_predicted_probs(df, .2, .8)
ggsave('figures/append_field_pp.png', width = 7, height = 4)

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

## Forage ----
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
df_og$OGseed_percent_maincrop_prop <- rep(NA, nrow(df_og))
df <- select(df_og, colnames(df.full_rounded))

df$mean <- predict(m, newdata = df)
df$model <- ifelse(df$OG_value_s < 0, "Practical", "Principled")
df$model <- ifelse(df$Region == "South" & df$survey_year == 2016 &
                     df$OG_value_s > -0.5, "Principled", 
                   ifelse(df$Region == "South" & df$survey_year == 2021 &
                            df$OG_value_s < 0.3, "Practical", df$model))
plot_predicted_probs(df, .2, .8)

ggsave('figures/append_forage_pp.png', width = 7, height = 4)

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

