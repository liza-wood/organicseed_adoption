library(dplyr)
library(tidyr)

# This data frame is the result of the merging.R and variable_creation.R scripts. The former merged together the three waves of surveys and the latter added in funding data (average annual funding between 2006-2010, 2011-2015, 2016-2020), and added data about producers at the regional level. 
df <- readRDS("~/Box/organicseed/df_onelevel.rds")
df.long <- readRDS("~/Box/organicseed/df_long_onelevel.rds")
# Cleaning and getting the data down to complete cases ----
df$IncreasedOGSeed_f <- factor(df$IncreasedOGSeed , levels = c("No", "Yes", "Already at 100%"))
table(df$IncreasedOGSeed_f)
df$IncreasedOGSeed_binom <- ifelse(df$IncreasedOGSeed_f == "No", 0,
                                        ifelse(df$IncreasedOGSeed_f == "Yes", 1, NA))
table(df.long$IncreasedOGSeed)
df.long$IncreasedOGSeed_f <- factor(df.long$IncreasedOGSeed , levels = c("No", "Yes", "Already at 100%"))
table(df.long$IncreasedOGSeed_f)
df.long$IncreasedOGSeed_binom <- ifelse(df.long$IncreasedOGSeed_f == "No", 0,
                                   ifelse(df.long$IncreasedOGSeed_f == "Yes", 1, NA))

df.full <- df %>% 
  filter(Region != "International") %>% 
  select(RespondentID, IncreasedOGSeed_f, IncreasedOGSeed_binom, maincrop, 
         TotalAcreage, 
         OGSeedOGIntegrityn, CertifierRequestn, funding_avg_annual_amt, 
         #n, prop_n, 
         ngrowers, nsupplychainactors,
         sum_rev_dnb, 
         survey_year, Region, cropcat_n) %>% 
  mutate(prop_n = ngrowers/nsupplychainactors) %>% 
  mutate(Region = factor(Region, levels = c("North Central", "West", 
                                            "Northeast", "South"))) %>% 
  mutate(TotalAcreage_ls = as.numeric(scale(log(TotalAcreage))), 
         funding_avg_annual_amt_ls = as.numeric(scale(log(funding_avg_annual_amt))),
         #funding_sum_5yr_ls = scale(log(funding_sum_5yr)),
         #n_s = as.numeric(scale(n)),
         n_prop_s = as.numeric(scale(prop_n)),
         sum_rev_s = as.numeric(scale(sum_rev_dnb)),
         OGSeedOGIntegrityn_s = as.numeric(scale(OGSeedOGIntegrityn))) %>% 
  filter(complete.cases(.)) %>% 
  mutate(survey_year_f = factor(survey_year))

df.full.long <- df.long %>% 
  filter(Region != "International") %>% 
  select(RespondentID, IncreasedOGSeed_f, IncreasedOGSeed_binom, OGcrop, 
         TotalAcreage, 
         OGSeedOGIntegrityn, CertifierRequestn, funding_avg_annual_amt, 
         #n, prop_n, 
         ngrowers, nsupplychainactors,
         sum_rev_dnb, 
         survey_year, Region, cropcat_n) %>% 
  mutate(Region = factor(Region, levels = c("North Central", "West",
                                            "Northeast", "South"))) %>% 
  mutate(prop_n = ngrowers/nsupplychainactors) %>% 
  mutate(TotalAcreage_ls = as.numeric(scale(log(TotalAcreage))), 
         funding_avg_annual_amt_ls = as.numeric(scale(log(funding_avg_annual_amt))),
         #funding_sum_5yr_ls = scale(log(funding_sum_5yr)),
         #n_s = as.numeric(scale(n)),
         n_prop_s = as.numeric(scale(prop_n)),
         sum_rev_s = as.numeric(scale(sum_rev_dnb)),
         OGSeedOGIntegrityn_s = as.numeric(scale(OGSeedOGIntegrityn))) %>% 
  filter(complete.cases(.)) %>% 
  mutate(survey_year_f = factor(survey_year))


maincrop_simple <- glm(IncreasedOGSeed_binom ~ 
                           TotalAcreage_ls + 
                           OGSeedOGIntegrityn_s + 
                           CertifierRequestn +  
                           funding_avg_annual_amt_ls + 
                           n_prop_s +
                           sum_rev_s +
                           maincrop + 
                           survey_year_f +
                           Region,
                       family = "binomial",
                         data = df.full)
summary(maincrop_simple)
table(df.full$Region, df.full$IncreasedOGSeed_binom)
maincrop_crossed1 <- glmer(IncreasedOGSeed_binom ~ 1 +
                         TotalAcreage_ls + 
                         OGSeedOGIntegrityn_s + 
                         CertifierRequestn +  
                         funding_avg_annual_amt_ls + 
                         sum_rev_s +
                         (1|maincrop) +
                         (1|survey_year_f) +
                         (1|Region),
                       family = "binomial",
                       data = df.full)
class(df.full$Region)
class(df.full$survey_year_f)
summary(maincrop_crossed1)
summary(maincrop_crossed0)
ranef(maincrop_crossed0)
ranef(maincrop_crossed1)


tapply(df.full$IncreasedOGSeed_binom, df.full$Region, summary)
table(is.na(df.full$Region))
?isSingular

maincrop_b <- brm(IncreasedOGSeed_binom ~ 
                    #maincrop + 
                    TotalAcreage_ls + 
                    OGSeedOGIntegrityn_s + 
                    CertifierRequestn +  
                    funding_avg_annual_amt_ls + 
                    n_s +
                    sum_rev_s +
                    (1|maincrop) +
                    (1|survey_year_f) +
                    (1|Region),
                  family = bernoulli(link = "logit"),
                  data = df.full)
summary(maincrop_b) # 

# Exploring the two best fitting models ----
ranef(maincrop_b)$Region # This will tell us how far the Region is away from the overall mean
coef(maincrop_b)$Region # Random effects plus fixed effects for Region
levels(df.full$Region) # They are presented in the factor order
reg_variation <- data.frame(dist.from.mean = ranef(lmerfit_maincrop)$Region$`(Intercept)`,
                            Region = levels(df.full.long$Region))
ranef(lmerfit_maincrop)$survey_year$`(Intercept)` # This will tell us how far the survey_year is away from the overall mean
coef(lmerfit_maincrop)$survey_year$`(Intercept)` # Random effects plus fixed effects for survey_year
year_variation <- data.frame(dist.from.mean = ranef(lmerfit_maincrop)$survey_year$`(Intercept)`,
                             survey_year = c(2011, 2016, 2021))


# This does not converge I think because there is no variation in the crop, so it looks like there are a ton of repeated values
allcrops_simple <- glmer(IncreasedOGSeed_binom ~ 
                           OGcrop + 
                           TotalAcreage_ls +
                           OGSeedOGIntegrityn_s + 
                           CertifierRequestn + 
                           funding_avg_annual_amt_ls  + 
                           n_s +
                           sum_rev_s +
                           survey_year_f +
                           Region +
                           (1|RespondentID),
                         family = "binomial",
                         data = df.full.long)
summary(allcrops_simple)

## -- these currently don't make sense because you can't have a random effect in multinom and haven't looked into polr
# https://stats.stackexchange.com/questions/51148/unable-to-provide-random-parameter-with-mlogit
# https://stackoverflow.com/questions/22020050/multinomial-mixed-logit-model-mlogit-r-package

multifit_allcrops <- nnet::multinom(IncreasedOGSeed_f ~ OGcrop + 
                                      TotalAcreage_ls + # demographics
                                      OGSeedOGIntegrityn_s + 
                                      CertifierRequestn +  # demand
                                      funding_avg_annual_amt_ls + # This has amount differents by year and region
                                      #producer_n_s, #+ I can't have producer_n because it is the same as Region
                                      Region +
                                      factor(survey_year),
                                      (1|RespondentID),
                                    data = df.full.long)
summary(multifit_allcrops)

polrfit_allcrops <- MASS::polr(IncreasedOGSeed_f ~ OGcrop + 
                                 TotalAcreage_ls + # demographics
                                 OGSeedOGIntegrityn_s + 
                                 CertifierRequestn +  # demand
                                 funding_avg_annual_amt_ls + # This has amount differents by year and region
                                 #producer_n_s, #+ I can't have producer_n because it is the same as Region
                                 Region +
                                 factor(survey_year), 
                               (1|RespondentID),
                               data = df.full.long)
summary(polrfit_allcrops)

glmfit_allcrops <- glmer(IncreasedOGSeed_binom ~ OGcrop + 
                         TotalAcreage_ls + # demographics
                         OGSeedOGIntegrityn_s + CertifierRequestn +  # demand
                         funding_avg_annual_amt_ls + 
                         Region + 
                         factor(survey_year), family = "binomial",
                       data = df.full.long)
summary(glmfit_allcrops)



