library(dplyr)
library(tidyr)

# This data frame is the result of the merging.R and variable_creation.R scripts. The former merged together the three waves of surveys and the latter added in funding data (average annual funding between 2006-2010, 2011-2015, 2016-2020), and added data about producers at the regional level. 
df <- read.csv("~/Box/organicseed/df.csv")

# Cleaning and getting the data down to complete cases ----
df <-  filter(df, IncreasedOGseed != "Already at 100%")
df$IncreasedOGseed_f <- ifelse(df$IncreasedOGseed == "Yes",1, 0)

df.full <- df %>% 
  filter(Region != "International") %>% 
  select(RespondentID, IncreasedOGseed_f, maincrop, TotalAcreage, OGSeedOGIntegrityn,
         CertifierRequestn, funding_avg_annual_amt, producer_n, survey_year, Region) %>% 
  mutate(Region = factor(Region, levels = c("West", "North Central", 
                                            "Northeast", "South"))) %>% 
  mutate(TotalAcreage_ls = scale(log(TotalAcreage)), 
         funding_avg_annual_amt_ls = scale(log(funding_avg_annual_amt)),
         producer_n_s = scale(producer_n),
         OGSeedOGIntegrityn_s = scale(OGSeedOGIntegrityn)) %>% 
  filter(complete.cases(.)) 

fit1_maincrop <- glm(IncreasedOGseed_f ~ maincrop + TotalAcreage_ls + # demographics
                      OGSeedOGIntegrityn_s + CertifierRequestn +  # demand
                      #funding_avg_annual_amt_ls + # This has amount differents by year and region
                      #producer_n_s, #+ I can't have producer_n because it is the same as Region
                      Region +
                      factor(survey_year), family = "binomial",
                    data = df.full)
summary(fit1_maincrop)

fit2_maincrop <- glmer(IncreasedOGseed_f ~ maincrop + TotalAcreage_ls + # demographics
                        OGSeedOGIntegrityn_s + CertifierRequestn +  # demand
                        funding_avg_annual_amt_ls + 
                        producer_n_s + # supply
                        (1|survey_year) + (1|Region), family = "binomial",
                       data = df.full)

summary(fit2_maincrop)

df.full.21 <- df %>% 
  filter(survey_year == 2021) %>% 
  filter(Region != "International") %>% 
  select(RespondentID, IncreasedOGseed_f, maincrop, TotalAcreage, OGSeedOGIntegrityn,
         CertifierRequestn, funding_avg_annual_amt, producer_n, survey_year, Region, contains("barrier")) %>% 
  mutate(Region = factor(Region, levels = c("West", "North Central", 
                                            "Northeast", "South"))) %>% 
  mutate(TotalAcreage_ls = scale(log(TotalAcreage)), 
         producer_n_s = scale(producer_n),
         OGSeedOGIntegrityn_s = scale(OGSeedOGIntegrityn)) 


fit21_maincrop_barriers <- glm(IncreasedOGseed_f ~ maincrop + TotalAcreage_ls + # demographics
                                OGSeedOGIntegrityn_s + 
                                CertifierRequestn +  
                                barrier_availability +
                                barrier_price +
                                barrier_lackdesiredTraits +
                                barrier_buyerReq +
                                Region, data = df.full.21,
                               family = "binomial")
summary(fit21_maincrop_barriers)
