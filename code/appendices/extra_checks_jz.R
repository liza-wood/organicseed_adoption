# Checking some details from Jared's notes
## 1. Using acre vs log acre because we have so few large farms; don't want the mean to be unrealistic
## Answer: use log acres, centered around the mean but not re-scaled again since scaling is already done with the log transformation
## 2. Likewise, scaling the acre variable: what is a "unit" in this scale?
## 3. Checking a result like 90% more conventional for forage crops compared to baseline (2010, Field Crops, North Central)

library(dplyr)
## 1. Should I use acre or log acre?
## SUMMARY AFTER INVESTIGATION: I am log scaling but not centering because for me that confuses the interpretation. the unit is a log unit, but that menas the acreage span changes over time. I also decided not to scale crop diversity
df.full <- readRDS("~/Box/organicseed_adoption/df.rds") %>% 
  mutate(log_acre = log(TotalAcreage)) %>% 
  select(TotalAcreage,
         log_acre,
         survey_year) %>% 
  na.omit()

#"Logarithmic transformation is a convenient means of transforming a highly skewed variable into a more normalized dataset"
ggplot(df.full, aes(TotalAcreage)) + geom_histogram()
ggplot(df.full, aes(log_acre)) + geom_histogram()

df.full %>% 
  group_by(survey_year) %>% 
  summarize(mean = mean(TotalAcreage, na.rm = T),
            median = median(TotalAcreage, na.rm = T),
            logmean = mean(log_acre, na.rm = T),
            logmedian = median(log_acre, na.rm = T))

summary(df.full$TotalAcreage)
summary(df.full$log_acre)
# So instead of the mean representing 294 acres, when we log it everything shifts down
# and the mean is actually:
exp(3.745) # 42 acres
# https://ourcodingclub.github.io/tutorials/data-scaling/

df.full <- df.full %>% 
  mutate(total_acre_scaled = c(scale(TotalAcreage)),
         log_acre_scaled = c(scale(log_acre)),
         log_acre_centered = c(scale(log_acre, scale = F)))

# I don't think re-scaling makes sense once it has been logged, so log_centered makes most sense
df.full %>% 
  group_by(survey_year) %>% 
  summarize(total = mean(TotalAcreage, na.rm = T),
            log = mean(log_acre, na.rm = T),
            total_scaled = mean(total_acre_scaled, na.rm = T),
            log_scaled = mean(log_acre_scaled, na.rm = T),
            log_centered = mean(log_acre_centered, na.rm = T))


summary(df.full$total_acre_scaled)
summary(df.full$log_acre_scaled)
summary(df.full$log_acre_centered)



#What is a unit of scaled acreage?
## Total vs scaled
mean_to_max_acres <- max(df.full$TotalAcreage) - mean(df.full$TotalAcreage)
mean_to_min_acres <- abs(min(df.full$TotalAcreage) - mean(df.full$TotalAcreage))
mean_to_max_acres_scaled <- max(df.full$total_acre_scaled) - mean(df.full$total_acre_scaled)
mean_to_min_acres_scaled <- abs(min(df.full$total_acre_scaled) - mean(df.full$total_acre_scaled))
## So does this make sense: when scaling, a unit is almost 1600 acres?
mean_to_max_acres/mean_to_max_acres_scaled
mean_to_min_acres/mean_to_min_acres_scaled

## Log vs log centered
mean_to_max_log <- max(df.full$log_acre) - mean(df.full$log_acre)
mean_to_max_log_centered <- max(df.full$log_acre_centered) - mean(df.full$log_acre_centered)
## When scaling the log, a unit is 1 acres logged -- THIS MAKES MOST SENSE THEN. Each unit is one log unit
mean_to_max_log/mean_to_max_log_centered

# But in real terms...

exp(max(df.full$log_acre)) # Max is 70,000 acres
exp(mean(df.full$log_acre)) # Mean is 42
# SO a unit is this range divided by how many units there are between mean and max
(exp(max(df.full$log_acre)) - exp(mean(df.full$log_acre)))/max(df.full$log_acre_centered)
# But is this true? I thnk the units adjust with the log...
summary(df.full$log_acre)
exp(-4) # Min
exp(-3) 
exp(-2) 
exp(-1) 
exp(0) 
exp(1) 
exp(2) #Q1

# So if mean = 0 is really 3.745
exp(3.745)
# one unit is reallt just one unit so
exp(4.75)

exp(4)
exp(5)
exp(6)
exp(7)

# SO I am struggling here. Then I look at other papers to see what they do. Previously I have log transformed without centering; others have categorized

# So what is I categorized it. That is what Hammong Wagner and Daxini do?
summary(df.full$TotalAcreage)
df.full <- readRDS("~/Box/organicseed_adoption/df_scaled.rds") %>% 
  mutate(acreage_s = case_when(
    TotalAcreage %in% 0:9 ~ 1,
    TotalAcreage %in% 10:49 ~ 2,
    TotalAcreage %in% 50:179 ~ 3,
    TotalAcreage %in% 180:499 ~ 4,
    TotalAcreage %in% 500:999 ~ 5,
    TotalAcreage >= 1000 ~ 6,
  )) %>% 
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
         survey_year) 
summary(df.full$acreage_s)
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

mdf <- prep_inla_data(m)

plot_inla_models(mdf, 1)


# 3. What about the forage result
## Does this descriptively make sense?
df.full <- readRDS("~/Box/organicseed_adoption/df_scaled.rds") %>% 
  mutate(acreage_s = c(scale(acreage_log, center = T, scale = F))) %>% 
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
         survey_year) 
summary(df.full$legitimation_user_s)
add <- .05

# Baseline is 28% for 2011, North Central, Field... but this is across all types of farmers
check <- df.full %>% 
  group_by(survey_year, Region, maincrop) %>% 
  summarize(cv_planted_mean = mean(CVseed_percent_maincrop_prop))

# But we don't have a "baseline" farmer in the data
rg <- 1
df.full %>% 
  filter(round(legitimation_user_s, 2) %in% seq(-rg, rg, by = 0.01),
         round(CertifierRequestn_s) == 0,
         round(noncommercial_seedsource_s, 2) %in% seq(-rg, rg, by = 0.01),
         round(acreage_s, 2) %in% seq(-rg, rg, by = 0.01))%>% 
  group_by(survey_year, Region, maincrop) %>% 
  summarize(cv_planted_mean = mean(CVseed_percent_maincrop_prop))

# So we have to try to intuit
explore <- df.full %>% 
  group_by(survey_year, Region, maincrop, legitimation_user_s, round(acreage_s)) %>% 
  summarize(cv_planted_mean = mean(CVseed_percent_maincrop_prop)) %>% 
  ungroup() %>% 
  filter(round(explore$legitimation_user_s, 3) == -0.226,
         `round(acreage_s)` == 0)

