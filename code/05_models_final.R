library(dplyr)
library(tidyr)
library(stargazer)
library(lme4)

df <- readRDS("~/Box/organicseed/df.rds")
df.long <- readRDS("~/Box/organicseed/df_long.rds")

# Considering acreage as the dependent variable ----
df.full <- df %>% 
  filter(Region != "International") %>% 
  select(RespondentID, OGseed_percent_maincrop, maincrop, 
         TotalAcreage, OGSeedOGIntegrityn,CertifierRequestn, 
         funding_avg_annual_amt, survey_year, Region) %>% 
  mutate(Region = factor(Region, levels = c("West", "North Central", 
                                            "Northeast", "South"))) %>% 
  mutate(TotalAcreage_ls = scale(log(TotalAcreage)), 
         funding_avg_annual_amt_ls = scale(log(funding_avg_annual_amt)),
         OGSeedOGIntegrityn_s = scale(OGSeedOGIntegrityn)) %>% 
  filter(complete.cases(.)) 

acreage <- lm(OGseed_percent_maincrop ~ maincrop + 
                        TotalAcreage_ls + # demographics
                        OGSeedOGIntegrityn_s + CertifierRequestn +  # demand
                        funding_avg_annual_amt_ls + 
                        Region +
                        factor(survey_year), 
              data = df.full)

df.full.21 <- df %>% 
  filter(survey_year == 2021) %>% 
  filter(Region != "International") %>% 
  select(RespondentID, OGseed_percent_maincrop, maincrop, TotalAcreage, 
         OGSeedOGIntegrityn, CertifierRequestn, funding_avg_annual_amt, 
         survey_year, Region, contains("barrier")) %>% 
  mutate(Region = factor(Region, levels = c("West", "North Central", 
                                            "Northeast", "South"))) %>% 
  mutate(TotalAcreage_ls = scale(log(TotalAcreage)), 
         OGSeedOGIntegrityn_s = scale(OGSeedOGIntegrityn)) 

acreage_barriers <- lm(OGseed_percent_maincrop ~ maincrop + 
                         TotalAcreage_ls + # demographics
                         OGSeedOGIntegrityn_s + 
                         CertifierRequestn +  
                         barrier_availability +
                         barrier_price +
                         barrier_lackdesiredTraits +
                         barrier_buyerReq +
                         Region, data = df.full.21)

df.full.long <- df.long %>% 
  filter(Region != "International") %>% 
  select(RespondentID, OGseed_percent, OGcrop, TotalAcreage, 
         OGSeedOGIntegrityn, CertifierRequestn, funding_avg_annual_amt, 
         survey_year, Region) %>% 
  mutate(Region = factor(Region, levels = c("West", "North Central", 
                                            "Northeast", "South"))) %>% 
  mutate(TotalAcreage_ls = scale(log(TotalAcreage)), 
         funding_avg_annual_amt_ls = scale(log(funding_avg_annual_amt)),
         OGSeedOGIntegrityn_s = scale(OGSeedOGIntegrityn)) %>% 
  filter(complete.cases(.))

acreage.long <- lmer(OGseed_percent ~ 
                           OGcrop + TotalAcreage_ls + # demographics
                           OGSeedOGIntegrityn_s + CertifierRequestn +  # demand
                           funding_avg_annual_amt_ls + 
                           Region +
                           #producer_revenue_s +
                           #producer_n_s + 
                           factor(survey_year) + 
                           (1|RespondentID),
                         data = df.full.long)

summary(acreage)
summary(acreage_barriers)
summary(acreage.long)

# Self-reported change ----
df$IncreasedOGSeed_f <- factor(df$IncreasedOGSeed , 
                               levels = c("No", "Yes", "Already at 100%"))

df.fulli <- df %>% 
  filter(Region != "International") %>% 
  select(RespondentID, IncreasedOGSeed_f, maincrop, 
         TotalAcreage, OGSeedOGIntegrityn,
         CertifierRequestn, funding_avg_annual_amt, 
         survey_year, Region) %>% 
  mutate(Region = factor(Region, levels = c("West", "North Central", 
                                            "Northeast", "South"))) %>% 
  mutate(TotalAcreage_ls = scale(log(TotalAcreage)), 
         funding_avg_annual_amt_ls = scale(log(funding_avg_annual_amt)),
         OGSeedOGIntegrityn_s = scale(OGSeedOGIntegrityn)) %>% 
  filter(complete.cases(.)) 

fit1_increase <- nnet::multinom(IncreasedOGSeed_f ~ maincrop + 
                       TotalAcreage_ls + # demographics
                       OGSeedOGIntegrityn_s + 
                       CertifierRequestn +  # demand
                       #funding_avg_annual_amt_ls + # This has amount differents by year and region
                       #producer_n_s, #+ I can't have producer_n because it is the same as Region
                       Region +
                       factor(survey_year),
                     data = df.fulli)
summary(fit1_increase)

fit2_increase <- MASS::polr(IncreasedOGSeed_f ~ maincrop + 
                         TotalAcreage_ls + # demographics
                         OGSeedOGIntegrityn_s + 
                         CertifierRequestn +  # demand
                         #funding_avg_annual_amt_ls + # This has amount differents by year and region
                         #producer_n_s, #+ I can't have producer_n because it is the same as Region
                         Region +
                         factor(survey_year), data = df.fulli)
summary(fit2_increase)
fit3_increase <- glm(IncreasedOGSeed_f ~ maincrop + TotalAcreage_ls + # demographics
                         OGSeedOGIntegrityn_s + CertifierRequestn +  # demand
                         funding_avg_annual_amt_ls + 
                         #producer_n_s + # supply
                         Region + 
                         factor(survey_year), family = "binomial",
                       data = df.fulli)

df.full.21i <- dfi %>% 
  filter(survey_year == 2021) %>% 
  filter(Region != "International") %>% 
  select(RespondentID, IncreasedOGSeed_f, maincrop, TotalAcreage, OGSeedOGIntegrityn,
         CertifierRequestn, funding_avg_annual_amt, producer_n, survey_year, Region, contains("barrier")) %>% 
  mutate(Region = factor(Region, levels = c("West", "North Central", 
                                            "Northeast", "South"))) %>% 
  mutate(TotalAcreage_ls = scale(log(TotalAcreage)), 
         producer_n_s = scale(producer_n),
         OGSeedOGIntegrityn_s = scale(OGSeedOGIntegrityn)) 


fit21_increase_barriers <- glm(IncreasedOGSeed_f ~ maincrop + TotalAcreage_ls + # demographics
                                 OGSeedOGIntegrityn_s + 
                                 CertifierRequestn +  
                                 barrier_availability +
                                 barrier_price +
                                 barrier_lackdesiredTraits +
                                 barrier_buyerReq +
                                 Region, data = df.full.21i,
                               family = "binomial")


summary(fit1_acreage)
summary(fit2_acreage)
summary(fit1_increase)
summary(fit2_increase)
summary(fit21_acreage_barriers)
summary(fit21_increase_barriers)

stargazer(fit1_acreage, fit2_acreage, type = "html", 
          title = "% acres of main crop planted to organic seed",
          out = "~/Documents/Davis/R-Projects/organicseed/tables/acreage_models.html")
stargazer(fit1_increase, fit2_increase, type = "html", 
          title = "Self-reported increase of organic seed usage over last 3 years",
          out = "~/Documents/Davis/R-Projects/organicseed/tables/increase_models.html")

stargazer(fit1_acreage, fit1_increase, type = "html", 
          title = "Linear model comparison",
          dep.var.labels = c("% Acreage", "Increased OG seed use"),
          out = "~/Documents/Davis/R-Projects/organicseed/tables/lm_models.html")
stargazer(fit2_acreage, fit2_increase, type = "html", 
          title = "Mulit-level linear model comparison",
          dep.var.labels = c("% Acreage", "Increased OG seed use"),
          out = "~/Documents/Davis/R-Projects/organicseed/tables/glmer_models.html")

stargazer(fit21_acreage_barriers, fit21_increase_barriers, type = "html", 
          dep.var.labels = c("% Acreage", "Increased OG seed use"),
          out = "~/Documents/Davis/R-Projects/organicseed/tables/barrier21_models.html")

library(emmeans)
emmeans(fit2_increase, specs = ~ CertifierRequestn)


pp_df1 <- data.frame(maincrop = "Field",
                     TotalAcreage_ls = median(df.full$TotalAcreage_ls), 
                     OGSeedOGIntegrityn_s = median(df.full$OGSeedOGIntegrityn_s, na.rm = T), 
                     CertifierRequestn = c(0,1),
                     Region = "West",
                     survey_year = 2016)

pp_df2 <- data.frame(maincrop = "Field",
                    TotalAcreage_ls = median(df.full$TotalAcreage_ls), 
                    OGSeedOGIntegrityn_s = median(df.full$OGSeedOGIntegrityn_s, na.rm = T), 
                    CertifierRequestn = c(0,1),
                    Region = "West",
                    survey_year = 2016,
                    funding_avg_annual_amt_ls = unique(df.full$funding_avg_annual_amt_ls[df.full$Region ==
                                                                                                "West" &
                                                                                         df.full$survey_year ==
                                                                                           2016]),
                    producer_n_s = unique(df.full$producer_n_s[df.full$Region == "West" &
                                                                 df.full$survey_year == 2016]))

# Using predict function to get predicted probabilities, with standard errors
pp_pred <- cbind(pp_df1, predict(fit1_acreage, newdata = pp_df1, se = T))
pp_pred <- cbind(pp_df2, predict(fit2_acreage, newdata = pp_df2, se = T))
pp_pred <- cbind(pp_df1, predict(fit1_increase, newdata = pp_df1, se = T))
pp_pred <- cbind(pp_df2, predict(fit2_increase, newdata = pp_df2, se = T))
