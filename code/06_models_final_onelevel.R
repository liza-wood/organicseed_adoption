library(dplyr)
library(tidyr)
library(stargazer)
library(lme4)
library(ggplot2)
library(equatiomatic)
library(cowplot)
library(brms)
setwd("~/Documents/Davis/R-Projects/organicseed/")
df <- readRDS("~/Box/organicseed/df_onelevel.rds") 

df$IncreasedOGSeed_f <- factor(df$IncreasedOGSeed , 
                               levels = c("No", "Yes", "Already at 100%"))
df$IncreasedOGSeed_binom <- ifelse(df$IncreasedOGSeed_f == "No", 0,
                                   ifelse(df$IncreasedOGSeed_f == "Yes", 1, NA))
# Considering acreage as the dependent variable ----
## Use c() when scaling numbers because it "strips" the dimension (i.e. scaling converts them into some kind of matrix and Stack Overflow told me that c would strip this)
df.full <- df %>% 
  filter(Region != "International", maincrop != "Other") %>% 
  select(RespondentID, OGseed_percent_maincrop, IncreasedOGSeed_binom, maincrop, 
         TotalAcreage, OGSeedOGIntegrityn,CertifierRequestn,
         funding_avg_annual_amt, 
         #n, prop_n, 
         ngrowers, nsupplychainactors,
         median_rev_survey, survey_year, Region, cropcat_n,
         contains("barrier")) %>% 
  mutate(prop_n = ngrowers/nsupplychainactors) %>%
  mutate(Region = factor(Region, levels = c("North Central", "West", 
                                            "Northeast", "South"))) %>% 
  mutate(TotalAcreage_ls = c(scale(log(TotalAcreage))), 
         funding_avg_annual_amt_ls = c(scale(log(funding_avg_annual_amt))),
         OGSeedOGIntegrityn_s = c(scale(OGSeedOGIntegrityn)),
         prop_n_s = as.numeric(scale(prop_n)),
         median_rev_s = as.numeric(scale(median_rev_survey)),
         barrier_availability_s = c(scale(barrier_availability)),
         barrier_price_s = c(scale(barrier_price)),
         barrier_distrustQuality_s = c(scale(barrier_distrustQuality)),
         barrier_buyerReq_s = c(scale(barrier_buyerReq))
  ) %>% 
  mutate(survey_year_f = factor(survey_year))

df1 <- select(df.full, OGseed_percent_maincrop, maincrop,
              CertifierRequestn,
              TotalAcreage_ls, 
              funding_avg_annual_amt_ls,
              OGSeedOGIntegrityn_s,
              prop_n_s, median_rev_s,
              Region, survey_year_f) %>% 
  filter(complete.cases(.))

df2 <- select(df.full, colnames(df1), 
              barrier_availability_s,
              barrier_price_s,
              barrier_buyerReq_s) %>% 
  filter(complete.cases(.))

acreage <- lm(OGseed_percent_maincrop ~ 
                  OGSeedOGIntegrityn_s + 
                  TotalAcreage_ls + 
                  CertifierRequestn + 
                  funding_avg_annual_amt_ls + 
                  prop_n_s +
                  median_rev_s +
                  maincrop +
                  survey_year_f +
                  Region,
                data = df1)
summary(acreage)

# Self-reported change ----

df3 <- select(df.full, IncreasedOGSeed_binom, maincrop,
              CertifierRequestn,
              TotalAcreage_ls, 
              funding_avg_annual_amt_ls,
              OGSeedOGIntegrityn_s,
              prop_n_s, median_rev_s,
              Region, survey_year_f) %>% 
  filter(complete.cases(.))

increase <- glm(IncreasedOGSeed_binom ~ 
                    OGSeedOGIntegrityn_s + 
                    TotalAcreage_ls + 
                    CertifierRequestn + 
                    funding_avg_annual_amt_ls + 
                    prop_n_s +
                    median_rev_s +
                    maincrop +
                    survey_year_f +
                    Region,
                  family = "binomial",
                  data = df3)
summary(increase)

acreage_plot <- sjPlot::plot_model(acreage, 
                                     show.values = T, 
                                     value.size = 4,
                                     bpe.style = "dot",
                                     bpe.color = "black",
                                     vline.color = "black",
                                     line.size = .5,
                                     group.terms = c(1,1,2,2,2,2,3,3,3,3,3,3,3),
                                     colors = c("darkblue", "darkgreen", "darkred"),
                                     axis.labels = 
                                       rev(c("Values organic seed",  
                                             "Total Acres (log)",
                                             "Certifier request", 
                                             "Research funding\n(annual-avg.)", 
                                             "Ratio of supply chain actors to growers",
                                             "Median revenue of\nseed-supply chain",
                                             "Crop type: forage (vs field)",
                                             "Crop type: vegetable (vs field)",
                                             "Year: 2015 (vs 2010)",
                                             "Year: 2020 (vs 2010)",
                                             "Region: West (vs North Central)",
                                             "Region: Northeast (vs North Central)",
                                             "Region: South (vs North Central)")))  + 
  theme_linedraw() +
  labs(title = "", y = "") ; acreage_plot

increase_plot <- sjPlot::plot_model(increase, 
                                     show.values = T, 
                                     value.size = 4,
                                     bpe.style = "dot",
                                     bpe.color = "black",
                                     vline.color = "black",
                                     line.size = .5,
                                     group.terms = c(1,1,2,2,2,2,3,3,3,3,3,3,3),
                                     colors = c("darkblue", "darkgreen", "darkred"),
                                     axis.labels = 
                                       rev(c("Values organic seed",  
                                             "Total Acres (log)",
                                             "Certifier request", 
                                             "Research funding\n(annual-avg.)", 
                                             "Ratio of supply chain actors to growers",
                                             "Median revenue of\nseed-supply chain",
                                             "Crop type: forage (vs field)",
                                             "Crop type: vegetable (vs field)",
                                             "Year: 2015 (vs 2010)",
                                             "Year: 2020 (vs 2010)",
                                             "Region: West (vs North Central)",
                                             "Region: Northeast (vs North Central)",
                                             "Region: South (vs North Central)")))  + 
  theme_linedraw() +
  labs(title = "", y = "") ; increase_plot
