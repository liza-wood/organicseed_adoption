library(dplyr)
library(tidyr)
library(lme4)
library(brms)

# This data frame is the result of the merging.R and variable_creation.R scripts. The former merged together the three waves of surveys and the latter added in funding data (average annual funding between 2006-2010, 2011-2015, 2016-2020), and added data about producers at the regional level. 
df <- readRDS("~/Box/organicseed/df.rds")
df.long <- readRDS("~/Box/organicseed/df_long.rds")

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

df.full <- df %>% 
  filter(Region != "International") %>% 
  select(RespondentID, OGseed_percent_maincrop, maincrop, TotalAcreage, 
         OGSeedOGIntegrityn, CertifierRequestn, funding_avg_annual_amt, 
         producer_n, median_producer_revenue, survey_year, Region) %>% 
  mutate(Region = factor(Region, levels = c("West", "North Central", 
                                            "Northeast", "South"))) %>% 
  mutate(TotalAcreage_ls = scale(log(TotalAcreage)), 
         funding_avg_annual_amt_ls = scale(log(funding_avg_annual_amt)),
         producer_n_s = scale(producer_n),
         producer_revenue_s = scale(median_producer_revenue),
         OGSeedOGIntegrityn_s = scale(OGSeedOGIntegrityn)) %>% 
  filter(complete.cases(.)) 


df.full.long <- df.long %>% 
  filter(Region != "International") %>% 
  select(RespondentID, OGseed_percent, OGcrop, TotalAcreage, 
         OGSeedOGIntegrityn, CertifierRequestn, funding_avg_annual_amt, 
         producer_n, median_producer_revenue, survey_year, Region) %>% 
  mutate(Region = factor(Region, levels = c("West", "North Central", 
                                            "Northeast", "South"))) %>% 
  mutate(TotalAcreage_ls = scale(log(TotalAcreage)), 
         funding_avg_annual_amt_ls = scale(log(funding_avg_annual_amt)),
         producer_n_s = scale(producer_n),
         producer_revenue_s = scale(median_producer_revenue),
         OGSeedOGIntegrityn_s = scale(OGSeedOGIntegrityn)) %>% 
  filter(complete.cases(.))

# Full models ----

## Linear model with all fixed effects
## dependent variable is percent of organic seed on their "main crop"
lmfit_maincrop_null <- lm(OGseed_percent_maincrop ~ 1, df.full)
lmfit_maincrop_full <- lm(OGseed_percent_maincrop ~ maincrop + 
                            TotalAcreage_ls + # demographics
                            OGSeedOGIntegrityn_s + CertifierRequestn +  # demand
                            funding_avg_annual_amt_ls + # This has amount differences by year and region
                            #producer_n_s, #+ I can't have producer_n because it is the same as Region
                            Region +
                            factor(survey_year), 
                          data = df.full)
summary(lmfit_maincrop_full) 

step(object = lmfit_maincrop_null, 
     scope = list(upper = lmfit_maincrop_full), direction = "forward", 
     k = log(nrow(df.full)), trace = TRUE)
## This stepwise process suggests that the best fit includes everything except funding, but the difference in AIC is 4 points, which is not massive if we think this funding piece have value

lmfit_maincrop <- lm(OGseed_percent_maincrop ~ 
                      maincrop + TotalAcreage_ls + # demographics
                      OGSeedOGIntegrityn_s + CertifierRequestn +  # demand
                      funding_avg_annual_amt_ls + Region + # supply
                      factor(survey_year), 
                    data = df.full)
summary(lmfit_maincrop) 

# To do this analysis with the "long" dataframe, we need to have an individual value to control for repetition, so it needs to be multi-level
lmerfit1_allcrop <- lmer(OGseed_percent ~ 
                           OGcrop + TotalAcreage_ls + # demographics
                           OGSeedOGIntegrityn_s + CertifierRequestn +  # demand
                           funding_avg_annual_amt_ls + 
                           Region +
                           #producer_revenue_s +
                           #producer_n_s + 
                           factor(survey_year) + 
                           (1|RespondentID),
                      data = df.full.long)
AIC(lmerfit1_allcrop)
summary(lmerfit1_allcrop)

# Here I can have both the year and the region as random effects and then multi-level attributes, funding average annual amount and producer n, to help determine if these are important drives at these higher scales
lmerfit_maincrop <- lmer(OGseed_percent_maincrop ~ maincrop + 
                           TotalAcreage_ls + # demographics
                           OGSeedOGIntegrityn_s + CertifierRequestn +  # demand
                           funding_avg_annual_amt_ls + 
                           producer_n_s + # supply
                           (1|survey_year) + (1|Region), 
                         data = df.full)
summary(lmerfit_maincrop) # the results here are the same as the lm, but we don't have the mixed

lmerfit2_allcrop <- lmer(OGseed_percent ~ 
                           OGcrop + 
                           TotalAcreage_ls + # demographics
                           OGSeedOGIntegrityn_s + # actor-focused demand
                           CertifierRequestn +  # intermediary demand
                           funding_avg_annual_amt_ls + # IS support
                           #producer_n_s + # supply
                           #producer_revenue_s +
                           (1|RespondentID) +
                           (1|Region) +
                           (1|survey_year),
                         data = df.full.long)
AIC(lmerfit2_allcrop)
summary(lmerfit2_allcrop) # 

lmerfit2_allcrop <- lmer(OGseed_percent ~ 
                           OGcrop + 
                           TotalAcreage_ls + # demographics
                           OGSeedOGIntegrityn_s + # actor-focused demand
                           CertifierRequestn +  # intermediary demand
                           funding_avg_annual_amt_ls + # IS support
                           #producer_n_s + # supply
                           median_producer_revenue +
                           (1|Region) +
                           (1|RespondentID) +
                           (1|survey_year),
                         data = df.full.long)
AIC(lmerfit2_allcrop)
summary(lmerfit2_allcrop) 


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
trainID3 <- rbind(trainID3, placeholder)
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

lmfit_maincrop_test <- lm(OGseed_percent_maincrop ~ maincrop + TotalAcreage_ls + # demographics
                      OGSeedOGIntegrityn_s + CertifierRequestn +  # demand
                      #funding_avg_annual_amt_ls + # This has amount different by year and region
                      #producer_n_s, #+ I can't have producer_n because it is the same as Region
                      Region +
                      factor(survey_year), 
                    data = df.full.test)

lmerfit1_allcrop_test <- lmer(OGseed_percent ~ OGcrop + TotalAcreage_ls + # demographics
                            OGSeedOGIntegrityn_s + CertifierRequestn +  # demand
                            #funding_avg_annual_amt_ls + 
                            #producer_n_s + 
                            Region +
                            factor(survey_year) + 
                            (1|RespondentID),
                     data = df.full.long.test)

lmerfit_maincrop_test <- lmer(OGseed_percent_maincrop ~ maincrop + TotalAcreage_ls + # demographics
                             OGSeedOGIntegrityn_s + CertifierRequestn +  # demand
                             funding_avg_annual_amt_ls + producer_n_s + # supply
                             (1|survey_year) + (1|Region), data = df.full.test)

lmerfit2_allcrop_test <- lmer(OGseed_percent ~ OGcrop + TotalAcreage_ls + # demographics
                            OGSeedOGIntegrityn_s + 
                            CertifierRequestn +  # demand
                            funding_avg_annual_amt_ls + 
                            producer_n_s + # supply
                            (1|RespondentID) +
                            (1|survey_year) + 
                            (1|Region), data = df.full.long.test)

# Comparing the predictive power of the two, and generally it is the allcrop (long data) with the multiple levels
mean((predict(lmfit_maincrop_test, newdata=df.full.train) - df.full.train$OGseed_percent_maincrop)^2)
mean((predict(lmerfit1_allcrop_test, newdata=df.full.long.train) - df.full.long.train$OGseed_percent)^2)
mean((predict(lmerfit_maincrop_test, newdata=df.full.train) - df.full.train$OGseed_percent_maincrop)^2) #**
mean((predict(lmerfit2_allcrop_test, newdata=df.full.long.train) - df.full.long.train$OGseed_percent)^2)

AIC(lmfit_maincrop)
AIC(lmerfit_maincrop)
AIC(lmerfit1_allcrop)
AIC(lmerfit2_allcrop)


# Exploring the two best fitting models ----
ranef(lmerfit_maincrop)$Region$`(Intercept)` # This will tell us how far the Region is away from the overall mean
coef(lmerfit_maincrop)$Region$`(Intercept)` # Random effects plus fixed effects for Region
levels(df.full$Region) # They are presented in the factor order
reg_variation <- data.frame(dist.from.mean = ranef(lmerfit_maincrop)$Region$`(Intercept)`,
                              Region = levels(df.full.long$Region))
ranef(lmerfit_maincrop)$survey_year$`(Intercept)` # This will tell us how far the survey_year is away from the overall mean
coef(lmerfit_maincrop)$survey_year$`(Intercept)` # Random effects plus fixed effects for survey_year
year_variation <- data.frame(dist.from.mean = ranef(lmerfit_maincrop)$survey_year$`(Intercept)`,
                            survey_year = c(2011, 2016, 2021))


# Testing this as a Bayesian model ----
lmerfit_maincrop_b <- brm(OGseed_percent_maincrop ~ maincrop + TotalAcreage_ls + # demographics
                         OGSeedOGIntegrityn_s + CertifierRequestn +  # demand
                         funding_avg_annual_amt_ls + producer_n_s + # supply
                         (1|survey_year) + (1|Region), data = df.full)
summary(lmerfit_maincrop_b)
plot(lmerfit_maincrop_b)

lmerfit2_allcrop_b <- brm(OGseed_percent ~ OGcrop + TotalAcreage_ls + # demographics
                           OGSeedOGIntegrityn_s + 
                           CertifierRequestn +  # demand
                           funding_avg_annual_amt_ls + 
                           producer_n_s + # supply
                           (1|RespondentID) +
                           (1|survey_year) + 
                           (1|Region), data = df.full.long)
summary(lmerfit2_allcrop_b)

## Looking at the model by each level (year) ----

survey11 <- df.full %>% filter(survey_year == 2011)
survey16 <- df.full %>% filter(survey_year == 2016)
survey21 <- df.full %>% filter(survey_year == 2021)

fit11_maincrop <- lm(OGseed_percent_maincrop ~ maincrop + TotalAcreage_ls + # demographics
                       OGSeedOGIntegrityn_s + CertifierRequestn +  # demand
                       Region, data = survey11)
summary(fit11_maincrop)

fit16_maincrop <- lm(OGseed_percent_maincrop ~ maincrop + TotalAcreage_ls + # demographics
                       OGSeedOGIntegrityn_s + CertifierRequestn +  # demand
                       Region, data = survey16)
summary(fit16_maincrop)

fit21_maincrop <- lm(OGseed_percent_maincrop ~ maincrop + TotalAcreage_ls + # demographics
                       OGSeedOGIntegrityn_s + CertifierRequestn +  # demand
                       Region, data = survey21)
summary(fit21_maincrop)

# Focus on 2021 and the barriers

df.full.21 <- df %>% 
  filter(survey_year == 2021) %>% 
  filter(Region != "International") %>% 
  select(RespondentID, OGseed_percent_maincrop, maincrop, TotalAcreage, OGSeedOGIntegrityn,
         CertifierRequestn, funding_avg_annual_amt, producer_n, survey_year, Region, contains("barrier")) %>% 
  mutate(Region = factor(Region, levels = c("West", "North Central", 
                                            "Northeast", "South"))) %>% 
  mutate(TotalAcreage_ls = scale(log(TotalAcreage)), 
         producer_n_s = scale(producer_n),
         OGSeedOGIntegrityn_s = scale(OGSeedOGIntegrityn)) 


fit21_maincrop_barriers <- lm(OGseed_percent_maincrop ~ maincrop + TotalAcreage_ls + # demographics
                             OGSeedOGIntegrityn_s + 
                             CertifierRequestn +  
                             barrier_availability +
                             barrier_price +
                             barrier_lackdesiredTraits +
                             barrier_buyerReq +
                             Region, data = df.full.21)
summary(fit21_maincrop_barriers)

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