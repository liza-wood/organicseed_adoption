library(dplyr)
library(tidyr)
library(lme4)
library(brms)

# This data frame is the result of the merging.R and variable_creation.R scripts. The former merged together the three waves of surveys and the latter added in funding data (average annual funding between 2006-2010, 2011-2015, 2016-2020), and added data about producers at the regional level. 
df <- readRDS("~/Box/organicseed/df_onelevel.rds") %>% 
  rename(n = n_companyproducer)
df.long <- readRDS("~/Box/organicseed/df_long_onelevel.rds") %>% 
  rename(n = n_companyproducer)

# The general idea of the model is as follows:
# Percent organic seed used  ~ demand + supply + farm details ; over time
## Unit of analysis is farmer with their "main" crop OR we can repeat farmers with each observation being each crop a producer grows
## Demand 
  ## Pro = Belief that OG seed is important + certifier requested improvement/certifier strictness?
  ## Barrier = seed quality comments as a sense of what holds producers back
## Supply
  ## Funding amt by year range (5 years prior) – could also maybe by crop
  ## Seed production regional "success"
      ## What should the proxy be: For now, number of seed producers
##  Demographic
  ## Acreage, main crops, region
## Time = survey year

# Levels: (level 2 are cross-nested, not heirarchical)
## 1. Individual
## 2a. Year x 3
## 2b. Region x 4

# Cleaning and getting the data down to complete cases ----
table(df$median_rev_survey, df$Region)
df.full <- df %>% 
  filter(Region != "International") %>% 
  select(RespondentID, OGseed_percent_maincrop, maincrop, TotalAcreage, 
         OGSeedOGIntegrityn, CertifierRequestn, funding_avg_annual_amt, 
         n, prop_n, median_rev_survey, survey_year, Region, cropcat_n) %>% 
  mutate(Region = factor(Region, levels = c("North Central", "West", 
                                            "Northeast", "South"))) %>% 
  mutate(TotalAcreage_ls = as.numeric(scale(log(TotalAcreage))), 
         funding_avg_annual_amt_ls = as.numeric(scale(log(funding_avg_annual_amt))),
         #funding_sum_5yr_ls = scale(log(funding_sum_5yr)),
         n_s = as.numeric(scale(n)),
         n_prop_s = as.numeric(scale(prop_n)),
         median_rev_s = as.numeric(scale(median_rev_survey)),
         OGSeedOGIntegrityn_s = as.numeric(scale(OGSeedOGIntegrityn))) %>% 
  filter(complete.cases(.)) %>% 
  mutate(survey_year_f = factor(survey_year))

df.full.long <- df.long %>% 
  filter(Region != "International") %>% 
  select(RespondentID, OGseed_percent, OGcrop, TotalAcreage, 
         OGSeedOGIntegrityn, CertifierRequestn, funding_avg_annual_amt, 
         n, prop_n, median_rev_survey, survey_year, Region, cropcat_n) %>% 
  mutate(Region = factor(Region, levels = c("North Central", "West",
                                            "Northeast", "South"))) %>% 
  mutate(TotalAcreage_ls = as.numeric(scale(log(TotalAcreage))), 
         funding_avg_annual_amt_ls = as.numeric(scale(log(funding_avg_annual_amt))),
         #funding_sum_5yr_ls = scale(log(funding_sum_5yr)),
         n_s = as.numeric(scale(n)),
         n_prop_s = as.numeric(scale(prop_n)),
         median_rev_s = as.numeric(scale(median_rev_survey)),
         OGSeedOGIntegrityn_s = as.numeric(scale(OGSeedOGIntegrityn))) %>% 
  filter(complete.cases(.)) %>% 
  mutate(survey_year_f = factor(survey_year))

xtabs(~ Region + maincrop + survey_year, df.full)
xtabs(~ Region + OGcrop + survey_year, df.full.long)

# Funding is by year, region, and crop
df.full %>% 
  ggplot(aes(x = survey_year_f, y = funding_avg_annual_amt)) +
  geom_boxplot() +
  geom_point(aes(color = maincrop)) +
  facet_wrap(~Region)
# N is by year only, otherwise I lose too much data
df.full %>% 
  ggplot(aes(x = survey_year_f, y = n)) +
  geom_boxplot() +
  geom_point(aes(color = maincrop)) +
  facet_wrap(~Region)
# median revenue is by year and region and crop
df.full %>% 
  ggplot(aes(x = survey_year_f, y = median_rev_survey)) +
  geom_boxplot() +
  geom_point(aes(color = maincrop)) +
  facet_wrap(~Region)

# Full models ----
## Let's run a stepwise process just to see what the model likes
lmfit_maincrop_null <- lm(OGseed_percent_maincrop ~ 1, df.full)
lmfit_maincrop_full <- lm(OGseed_percent_maincrop ~ 
                            maincrop + 
                            TotalAcreage_ls + 
                            OGSeedOGIntegrityn_s + 
                            CertifierRequestn + 
                            cropcat_n +
                            funding_avg_annual_amt_ls + # This has amount differences by year and region and crop
                            n_prop_s + # differences by year and region
                            median_rev_s + # differences by year and region
                            Region +
                            survey_year_f, 
                          data = df.full)
summary(lmfit_maincrop_full) 
# Too much collinearity here

step(object = lmfit_maincrop_null, 
     scope = list(upper = lmfit_maincrop_full), direction = "forward", 
     k = log(nrow(df.full)), trace = TRUE)
## This stepwise process suggests removing those regional level attributes

maincrop_simple <- lm(OGseed_percent_maincrop ~ 
                      maincrop + 
                       TotalAcreage_ls + # demographics
                       OGSeedOGIntegrityn_s + 
                       CertifierRequestn +  # demand
                       funding_avg_annual_amt_ls + 
                       n_s +
                       median_rev_s +
                       survey_year_f +
                       Region , # supply
                    data = df.full)
summary(maincrop_simple) 
AIC(maincrop_simple) # 20235
# Check for collinearity
par(mfrow = c(2, 2))
plot(maincrop_simple) # these look okay, I think?
# But these test results are bad
lmtest::bptest(maincrop_simple) # violates homoskedastic null
skedastic::white_lm(maincrop_simple)

#Looked online for two-way fixed effects, but this is often used for time series
# Tried to use plm but I really think this is meant for panel data and groups without repeat
# The other way is to look at the covariance and then remove it from the model 
vcov_tw <- multiwayvcov::cluster.vcov(lmfit_maincrop_full, 
                                      cbind(df.full$Region, 
                                            df.full$survey_year_f),
                                      use_white = F, 
                                      df_correction = F)
# But that does nothing, so I'm bailing on this fixed effects thing
lmtest::coeftest(lmfit_maincrop_full, vcov_tw)

# Let's get back to looking at models.
maincrop_crossed <- lmer(OGseed_percent_maincrop ~ 
                           #cropcat_n + 
                           TotalAcreage_ls + # demographics
                           OGSeedOGIntegrityn_s + 
                           CertifierRequestn +  # demand
                           funding_avg_annual_amt_ls + 
                           n_prop_s +
                           median_rev_s +
                           (1|maincrop) +
                           (1|survey_year_f) +
                           (1|Region),
                         data = df.full)
summary(maincrop_crossed) 
AIC(maincrop_crossed)
AIC(maincrop_simple)
BIC(maincrop_crossed)
BIC(maincrop_simple)
# I don't hate this; so either maincrop simple or maincrop crossed

# Break this down little by little
maincrop_crossed_1 <- lm(OGseed_percent_maincrop ~ 
                           #maincrop + 
                           TotalAcreage_ls + # demographics
                           OGSeedOGIntegrityn_s + 
                           CertifierRequestn +  # demand
                           funding_avg_annual_amt_ls + 
                           n_prop_s +
                           median_rev_s,
                         data = df.full)
summary(maincrop_crossed_1) # Without fixing regions effects we see n negative and rev positive

maincrop_crossed_2 <- lmer(OGseed_percent_maincrop ~ 
                           TotalAcreage_ls + # demographics
                           OGSeedOGIntegrityn_s + 
                           CertifierRequestn +  # demand
                           funding_avg_annual_amt_ls + 
                           n_prop_s +
                           median_rev_s +
                           (1|Region),
                         data = df.full)
summary(maincrop_crossed_2)

maincrop_crossed_3 <- lmer(OGseed_percent_maincrop ~ 
                             TotalAcreage_ls + # demographics
                             OGSeedOGIntegrityn_s + 
                             CertifierRequestn +  # demand
                             funding_avg_annual_amt_ls + 
                             n_prop_s +
                             median_rev_s +
                             (1|maincrop) +
                             (1|Region),
                           data = df.full)
summary(maincrop_crossed_3)

BIC(maincrop_crossed_1)
BIC(maincrop_crossed_2)
BIC(maincrop_crossed_3)
BIC(maincrop_crossed)

# To do this analysis with the "long" dataframe, we need to have an individual value to control for repetition, so it needs to be multi-level
allcrop_simple <- lmer(OGseed_percent ~ 
                           OGcrop + 
                           TotalAcreage_ls + # demographics
                           OGSeedOGIntegrityn_s + 
                           CertifierRequestn +  # demand
                           funding_avg_annual_amt_ls + 
                           n_s +
                           median_rev_s +
                           Region +
                           survey_year_f + 
                           (1|RespondentID),
                         data = df.full.long)
summary(allcrop_simple)

allcrop_crossed <- lmer(OGseed_percent ~ 
                           #OGcrop + 
                           TotalAcreage_ls + # demographics
                           OGSeedOGIntegrityn_s + # actor-focused demand
                           CertifierRequestn +  # intermediary demand
                           funding_avg_annual_amt_ls + # IS support
                           n_s +
                           median_rev_s +
                           #survey_year_f +
                           (1|OGcrop) +
                           (1|survey_year_f) +
                           (1|RespondentID) +
                           (1|Region),
                         data = df.full.long)
summary(allcrop_crossed) # 

AIC(allcrop_crossed)
AIC(allcrop_simple)
BIC(allcrop_crossed)
BIC(allcrop_simple)


# Exploring the two best fitting models ----
ranef(maincrop_crossed)$Region$`(Intercept)` # This will tell us how far the Region is away from the overall mean
ranef(maincrop_crossed_b)$Region
coef(maincrop_crossed)$Region$`(Intercept)` # Random effects plus fixed effects for Region
levels(df.full$Region) # They are presented in the factor order
reg_variation <- data.frame(dist.from.mean = ranef(lmerfit_maincrop)$Region$`(Intercept)`,
                            Region = levels(df.full.long$Region))
ranef(maincrop_crossed)$survey_year$`(Intercept)` # This will tell us how far the survey_year is away from the overall mean
coef(maincrop_crossed)$survey_year$`(Intercept)` # Random effects plus fixed effects for survey_year
year_variation <- data.frame(dist.from.mean = ranef(lmerfit_maincrop)$survey_year$`(Intercept)`,
                             survey_year = c(2011, 2016, 2021))


# Comparing the two models' predictive power ----

## Need to create a training and testing set for the data. This is easy for the not long data because there is no respondent ID grouping, so the models with fixed effects like region and year can be compared 
set.seed(200)
df.full.train <- df.full[sample(nrow(df.full), nrow(df.full)/2), ]
df.full.test <- anti_join(df.full, df.full.train)

## However, since the long data looks at the random effect of the respondent, then I need to do some work with this so that I can use the model to predict a testing data set. I do this by "matching" the training and test set IDs so that we can overwrite the test IDs. In this case the IDs don't actually mean anything so I can remove them
cropn <- df.full.long %>% group_by(RespondentID) %>% count()
crop3 <- filter(cropn, n == 3)
crop2 <- filter(cropn, n == 2)
crop1 <- filter(cropn, n == 1)

placeholder <- data.frame(RespondentID = 0)

trainID3 <- crop3[sample(nrow(crop3), nrow(crop3)/2),-2]
#trainID3 <- rbind(trainID3, placeholder)
testID3 <- anti_join(crop3, trainID3) %>% select(-n)
trainID2 <- crop2[sample(nrow(crop2), nrow(crop2)/2),-2]
#trainID2 <- rbind(trainID2, placeholder)
testID2 <- anti_join(crop2, trainID2) %>% select(-n)
trainID1 <- crop1[sample(nrow(crop1), nrow(crop1)/2),-2] 
#trainID1 <- rbind(trainID1, placeholder)
testID1 <- anti_join(crop1, trainID1) %>% select(-n)

crop3 <- data.frame(trainID3, testID3)
colnames(crop3) <- c("trainID", "testID")
crop2 <- data.frame(trainID2, testID2)
colnames(crop2) <- c("trainID", "testID")
crop1 <- data.frame(trainID1, testID1)
colnames(crop1) <- c("trainID", "testID")
cropmatchlist <- rbind(crop3, crop2) %>% rbind(crop1)

df.full.long.train <- df.full.long[df.full.long$RespondentID %in% c(trainID3$RespondentID, trainID2$RespondentID, trainID1$RespondentID), ]
df.full.long.test <- df.full.long[df.full.long$RespondentID %in% c(testID3$RespondentID, testID2$RespondentID, testID1$RespondentID), ]
for(j in 1:nrow(df.full.long.test)){
for(i in 1:nrow(cropmatchlist)){
    if(df.full.long.test$RespondentID[j] == cropmatchlist$testID[i]){
      df.full.long.test$RespondentID[j] <- cropmatchlist$trainID[i]
    } else {next}
  }
}

df.full.long.test <- filter(df.full.long.test, RespondentID != 0)


# Which do we think performs better? compare the models on the test

maincrop_simple_test <- lm(OGseed_percent_maincrop ~ 
                             maincrop + 
                             TotalAcreage_ls +
                             OGSeedOGIntegrityn_s + 
                             CertifierRequestn + 
                             funding_avg_annual_amt_ls + 
                             n_s +
                             median_rev_s +
                             survey_year_f +
                             Region ,
                    data = df.full.test)

allcrop_simple_test <- lmer(OGseed_percent ~ 
                              OGcrop + 
                              TotalAcreage_ls +
                              OGSeedOGIntegrityn_s + 
                              CertifierRequestn + 
                              funding_avg_annual_amt_ls + 
                              n_s +
                              median_rev_s +
                              Region +
                              survey_year_f + 
                              (1|RespondentID),
                     data = df.full.long.test)

maincrop_crossed_test <- lmer(OGseed_percent_maincrop ~ 
                                maincrop + 
                                TotalAcreage_ls + # demographics
                                OGSeedOGIntegrityn_s + 
                                CertifierRequestn +  # demand
                                funding_avg_annual_amt_ls + 
                                n_s +
                                median_rev_s +
                                survey_year_f +
                                (1|Region),
                             data = df.full.test)

allcrop_crossed_test <- lmer(OGseed_percent ~ 
                               OGcrop + 
                               TotalAcreage_ls + 
                               OGSeedOGIntegrityn_s + 
                               CertifierRequestn + 
                               funding_avg_annual_amt_ls + 
                               n_s +
                               median_rev_s +
                               #(1|survey_year_f) +
                               (1|RespondentID) +
                               (1|Region),
                              data = df.full.long.test)

# Comparing the predictive power of the two, and generally it is the allcrop (long data) with the multiple levels
mean((predict(maincrop_simple_test, newdata=df.full.train) - df.full.train$OGseed_percent_maincrop)^2)
mean((predict(allcrop_simple_test, newdata=df.full.long.train) - df.full.long.train$OGseed_percent)^2)
mean((predict(maincrop_crossed_test, newdata=df.full.train) - df.full.train$OGseed_percent_maincrop)^2) #**
mean((predict(allcrop_crossed_test, newdata=df.full.long.train) - df.full.long.train$OGseed_percent)^2)

# Predictive power is highest of lmerfit2_allcrop but...

summary(maincrop_simple) # n matters, funding does not, year does not, region does
summary(maincrop_crossed) # n matters, funding does not, year does not, region does
summary(allcrop_simple) # n matters, funding does not, year does not, region does
summary(allcrop_crossed) # n matters, funding does not, year DOES, region does
# Testing this as a Bayesian model ----

maincrop_b <- brm(OGseed_percent_maincrop ~ 
                           #maincrop + 
                           TotalAcreage_ls + # demographics
                           OGSeedOGIntegrityn_s + # actor-focused demand
                           CertifierRequestn +  # intermediary demand
                           funding_avg_annual_amt_ls + # IS support
                           n_s +
                           median_rev_s +
                           (1|maincrop) +
                           (1|survey_year_f) +
                           (1|Region),
                         data = df.full)
summary(maincrop_b) # 

## Looking at the model by each level (year) ----

survey11 <- df.full %>% filter(survey_year == 2011)
survey16 <- df.full %>% filter(survey_year == 2016)
survey21 <- df.full %>% filter(survey_year == 2021)
survey11.long <- df.full.long %>% filter(survey_year == 2011)
survey16.long <- df.full.long %>% filter(survey_year == 2016)
survey21.long <- df.full.long %>% filter(survey_year == 2021)

fit11_maincrop <- lm(OGseed_percent_maincrop ~ 
                       maincrop + 
                       TotalAcreage_ls + # demographics
                       OGSeedOGIntegrityn_s + 
                       CertifierRequestn +  # demand
                       funding_avg_annual_amt_ls + 
                       Region, 
                      data = survey11)
summary(fit11_maincrop)

fit11_mainlmer <- lmer(OGseed_percent_maincrop ~ 
                         maincrop + 
                         TotalAcreage_ls + # demographics
                         OGSeedOGIntegrityn_s + 
                         CertifierRequestn +  # demand
                         funding_avg_annual_amt_ls + 
                         n_s +
                         median_rev_factiva_s +
                         (1|Region), 
                       data = survey11)
summary(fit11_mainlmer)

fit11_allcrop <- lmer(OGseed_percent ~ 
                           OGcrop + 
                           TotalAcreage_ls + # demographics
                           OGSeedOGIntegrityn_s + # actor-focused demand
                           CertifierRequestn +  # intermediary demand
                           funding_avg_annual_amt_ls + # IS support
                           n_s +
                           median_rev_factiva_s +
                           (1|RespondentID) +
                           (1|Region),
                         data = survey11.long)
summary(fit11_allcrop) # 

fit16_maincrop <- lm(OGseed_percent_maincrop ~ 
                       maincrop + 
                       TotalAcreage_ls + # demographics
                       OGSeedOGIntegrityn_s + 
                       CertifierRequestn +  # demand
                       funding_avg_annual_amt_ls + 
                       Region, data = survey16)
summary(fit16_maincrop)

fit16_mainlmer <- lmer(OGseed_percent_maincrop ~ 
                         maincrop + 
                         TotalAcreage_ls + # demographics
                         OGSeedOGIntegrityn_s + 
                         CertifierRequestn +  # demand
                         funding_avg_annual_amt_ls + 
                         n_s +
                         median_rev_factiva_s +
                         (1|Region), 
                       data = survey16)
summary(fit16_mainlmer)

fit16_allcrop <- lmer(OGseed_percent ~ 
                        OGcrop + 
                        TotalAcreage_ls + # demographics
                        OGSeedOGIntegrityn_s + # actor-focused demand
                        CertifierRequestn +  # intermediary demand
                        funding_avg_annual_amt_ls + # IS support
                        n_s +
                        median_rev_factiva_s +
                        (1|RespondentID) +
                        (1|Region),
                      data = survey16.long)
summary(fit16_allcrop) # 

fit21_maincrop <- lm(OGseed_percent_maincrop ~
                       maincrop + 
                       TotalAcreage_ls + # demographics
                       OGSeedOGIntegrityn_s + 
                       CertifierRequestn +  # demand
                       funding_avg_annual_amt_ls + 
                       Region, 
                       data = survey21)
summary(fit21_maincrop)

fit21_mainlmer <- lmer(OGseed_percent_maincrop ~ 
                         maincrop + 
                         TotalAcreage_ls + # demographics
                         OGSeedOGIntegrityn_s + 
                         CertifierRequestn +  # demand
                         funding_avg_annual_amt_ls + 
                         n_s +
                         median_rev_factiva_s +
                         (1|Region), 
                       data = survey21)
summary(fit21_mainlmer)

fit21_allcrop <- lmer(OGseed_percent ~ 
                        OGcrop + 
                        TotalAcreage_ls + # demographics
                        OGSeedOGIntegrityn_s + # actor-focused demand
                        CertifierRequestn +  # intermediary demand
                        funding_avg_annual_amt_ls + # IS support
                        n_s +
                        median_rev_factiva_s +
                        (1|RespondentID) +
                        (1|Region),
                      data = survey21.long)
summary(fit21_allcrop) # 

# Focus on 2021 and the barriers

df.full.barr <- df.long %>% 
  filter(survey_year %in% c(2016,2021)) %>% 
  filter(Region != "International") %>% 
  select(RespondentID, OGseed_percent_maincrop, maincrop, TotalAcreage, 
         OGSeedOGIntegrityn,
         CertifierRequestn, 
         funding_avg_annual_amt, n, survey_year, 
         Region, contains("barrier")) %>% 
  mutate(Region = factor(Region, levels = c("West", "North Central", 
                                            "Northeast", "South"))) %>% 
  mutate(funding_avg_annual_amt_ls = scale(log(funding_avg_annual_amt)),
        TotalAcreage_ls = scale(log(TotalAcreage)), 
         n_s = scale(n),
         OGSeedOGIntegrityn_s = scale(OGSeedOGIntegrityn)) %>% 
  filter(complete.cases(.))


fit_maincrop_barriers <- lmer(OGseed_percent_maincrop ~ 
                              maincrop + 
                              TotalAcreage_ls + # demographics
                             OGSeedOGIntegrityn_s + 
                             CertifierRequestn +  
                              funding_avg_annual_amt_ls +
                              n_s +
                               median_revenue_s +
                             barrier_availability +
                             barrier_price +
                             barrier_lackdesiredTraits +
                             barrier_buyerReq +
                               (1|RespondentID) +
                               (1|Region), data = df.full.barr)
summary(fit_maincrop_barriers)


## Looking at the model by each level (Region) ----

west <- filter(df.full, Region == "West")
ncentral <- filter(df.full, Region == "North Central")
south <- filter(df.full, Region == "South")
neast <- filter(df.full, Region == "Northeast")

fitwest <- lm(OGseed_percent_maincrop ~ maincrop + TotalAcreage_ls + 
                OGSeedOGIntegrityn_s + CertifierRequestn + 
                factor(survey_year), data = west)
summary(fitwest)


fitncentral <- lm(OGseed_percent_maincrop ~ maincrop + TotalAcreage_ls + 
                    OGSeedOGIntegrityn_s + CertifierRequestn + 
                    factor(survey_year), data = ncentral)
summary(fitncentral)

fitneast <- lm(OGseed_percent_maincrop ~ maincrop + TotalAcreage_ls + 
                 OGSeedOGIntegrityn_s + CertifierRequestn + 
                 factor(survey_year), data = neast)
summary(fitneast)

fitsouth <- lm(OGseed_percent_maincrop ~ maincrop + TotalAcreage_ls + 
                 OGSeedOGIntegrityn_s + CertifierRequestn + 
                 factor(survey_year), data = south)
summary(fitsouth)


req <- df %>% select(CertifierRequestn, OGseed_percent_maincrop, survey_year) %>% 
  filter(CertifierRequestn == 1) 
noreq <- df %>% select(CertifierRequestn, OGseed_percent_maincrop, survey_year) %>% 
  filter(CertifierRequestn == 0) 

t.test(req$OGseed_percent_maincrop, noreq$OGseed_percent_maincrop)



# DiD experiment -- have not gotten here yet ----
# https://rpubs.com/phle/r_tutorial_difference_in_differences

df.full %>% 
  filter(survey_year == 2011) %>% 
  group_by(CertifierRequestn) %>% 
  summarise(OGseed_percent_maincrop = mean(OGseed_percent_maincrop, na.rm = T),
            TotalAcreage = mean(TotalAcreage, na.rm = T),
            OGSeedOGIntegrityn = mean(OGSeedOGIntegrityn, na.rm = T))

df.full %>% 
  filter(survey_year == 2016) %>% 
  group_by(CertifierRequestn) %>% 
  summarise(OGseed_percent_maincrop = mean(OGseed_percent_maincrop, na.rm = T),
            TotalAcreage = mean(TotalAcreage, na.rm = T),
            OGSeedOGIntegrityn = mean(OGSeedOGIntegrityn, na.rm = T))

df.full %>% 
  filter(survey_year == 2021) %>% 
  group_by(CertifierRequestn) %>% 
  summarise(OGseed_percent_maincrop = mean(OGseed_percent_maincrop, na.rm = T),
            TotalAcreage = mean(TotalAcreage, na.rm = T),
            OGSeedOGIntegrityn = mean(OGSeedOGIntegrityn, na.rm = T))

hist11 <- df.full %>%
  filter(survey_year == 2011) %>% 
  ggplot(aes(x = OGseed_percent_maincrop, fill = factor(CertifierRequestn))) +
  geom_histogram(alpha=0.5, position = "dodge") +
  labs() +
  theme_linedraw()

hist16 <- df.full %>%
  filter(survey_year == 2016) %>% 
  ggplot(aes(x = OGseed_percent_maincrop, fill = factor(CertifierRequestn))) +
  geom_histogram(alpha=0.5, position = "dodge") +
  labs() +
  theme_linedraw()

hist21 <- df.full %>%
  filter(survey_year == 2021) %>% 
  ggplot(aes(x = OGseed_percent_maincrop, fill = factor(CertifierRequestn))) +
  geom_histogram(alpha=0.5, position = "dodge") +
  labs() +
  theme_linedraw()


ggarrange(hist11, hist16, hist21, hist.nov, ncol = 1, 
          common.legend = TRUE, legend = "bottom")

differences <- df.full %>%
  group_by(survey_year, CertifierRequestn) %>%
  summarise(OGseed_percent_maincrop = mean(OGseed_percent_maincrop, na.rm = TRUE))

# Treatment group (NJ) before treatment
njfeb <- differences[1,3]

# Control group (PA) before treatment
pafeb <- differences[2,3]

# Treatment group (NJ) after treatment
njnov <- differences[3,3]

# Control group (PA) after treatment
panov <- differences[4,3]