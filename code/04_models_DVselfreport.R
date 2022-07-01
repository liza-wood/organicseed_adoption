library(dplyr)
library(tidyr)

# This data frame is the result of the merging.R and variable_creation.R scripts. The former merged together the three waves of surveys and the latter added in funding data (average annual funding between 2006-2010, 2011-2015, 2016-2020), and added data about producers at the regional level. 
df <- readRDS("~/Box/organicseed/df.rds")
df.long <- readRDS("~/Box/organicseed/df_long.rds")
# Cleaning and getting the data down to complete cases ----
table(df$IncreasedOGSeed)
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
         TotalAcreage, OGSeedOGIntegrityn,CertifierRequestn, 
         funding_avg_annual_amt, survey_year, Region) %>% 
  mutate(Region = factor(Region, levels = c("West", "North Central", 
                                            "Northeast", "South"))) %>% 
  mutate(TotalAcreage_ls = scale(log(TotalAcreage)), 
         funding_avg_annual_amt_ls = scale(log(funding_avg_annual_amt)),
         OGSeedOGIntegrityn_s = scale(OGSeedOGIntegrityn)) 
table(df$IncreasedOGSeed_f)
table(df.full$IncreasedOGSeed_f)

df.full.long <- df.long %>% 
  filter(Region != "International") %>% 
  select(RespondentID, IncreasedOGSeed_f, IncreasedOGSeed_binom, OGcrop, 
         TotalAcreage, 
         OGSeedOGIntegrityn, CertifierRequestn, funding_avg_annual_amt, 
         survey_year, Region) %>% 
  mutate(Region = factor(Region, levels = c("West", "North Central", 
                                            "Northeast", "South"))) %>% 
  mutate(TotalAcreage_ls = scale(log(TotalAcreage)), 
         funding_avg_annual_amt_ls = scale(log(funding_avg_annual_amt)),
         OGSeedOGIntegrityn_s = scale(OGSeedOGIntegrityn))

multifit_maincrop <- nnet::multinom(IncreasedOGSeed_f ~ maincrop + 
                                  TotalAcreage_ls + # demographics
                                  OGSeedOGIntegrityn_s + 
                                  CertifierRequestn +  # demand
                                  #funding_avg_annual_amt_ls + # This has amount differents by year and region
                                  #producer_n_s, #+ I can't have producer_n because it is the same as Region
                                  Region +
                                  factor(survey_year),
                                data = df.full)
summary(multifit_maincrop)

polrfit_maincrop <- MASS::polr(IncreasedOGSeed_f ~ maincrop + 
                              TotalAcreage_ls + # demographics
                              OGSeedOGIntegrityn_s + 
                              CertifierRequestn +  # demand
                              #funding_avg_annual_amt_ls + # This has amount differents by year and region
                              #producer_n_s, #+ I can't have producer_n because it is the same as Region
                              Region +
                              factor(survey_year), 
                              data = df.full)
summary(polrfit_maincrop)

glmfit_maincrop <- glm(IncreasedOGSeed_binom ~ maincrop + 
                      TotalAcreage_ls + # demographics
                       OGSeedOGIntegrityn_s + CertifierRequestn +  # demand
                       funding_avg_annual_amt_ls + 
                       #producer_n_s + # supply
                       Region + 
                       factor(survey_year), family = "binomial",
                     data = df.full)
summary(glmfit_maincrop)

multifit_allcrops <- nnet::multinom(IncreasedOGSeed_f ~ OGcrop + 
                                      TotalAcreage_ls + # demographics
                                      OGSeedOGIntegrityn_s + 
                                      CertifierRequestn +  # demand
                                      #funding_avg_annual_amt_ls + # This has amount differents by year and region
                                      #producer_n_s, #+ I can't have producer_n because it is the same as Region
                                      Region +
                                      factor(survey_year),
                                    data = df.full.long)
summary(multifit_allcrops)

polrfit_allcrops <- MASS::polr(IncreasedOGSeed_f ~ OGcrop + 
                                 TotalAcreage_ls + # demographics
                                 OGSeedOGIntegrityn_s + 
                                 CertifierRequestn +  # demand
                                 #funding_avg_annual_amt_ls + # This has amount differents by year and region
                                 #producer_n_s, #+ I can't have producer_n because it is the same as Region
                                 Region +
                                 factor(survey_year), 
                               data = df.full.long)
summary(polrfit_allcrops)

glmfit_allcrops <- glm(IncreasedOGSeed_binom ~ OGcrop + 
                         TotalAcreage_ls + # demographics
                         OGSeedOGIntegrityn_s + CertifierRequestn +  # demand
                         funding_avg_annual_amt_ls + 
                         #producer_n_s + # supply
                         Region + 
                         factor(survey_year), family = "binomial",
                       data = df.full.long)
summary(glmfit_allcrops)



