library(VIM)
library(tidyverse)

# This is the dataframe where I have identified main crops of producers, so each observation is only one producer
df <- readRDS("~/Box/organicseed/df.rds")
# This is the dataframe where I have lengthened out each crop for the grower, so any grower may have up to 3 crops
df.long <- readRDS("~/Box/organicseed/df_long.rds")

palette <- RColorBrewer::brewer.pal(8, "YlGnBu")
# How many crops are each grower growing?
df.long %>% 
  group_by(RespondentID) %>% 
  count() %>% 
  group_by(n) %>% 
  count()

# Let's narrow this down to the variables of interest
df <- df %>% 
  filter(Region != "International", maincrop != "Unknown") %>% 
  select(maincrop, OGseed_percent_maincrop, IncreasedOGSeed,
         VegiAcreage, FieldAcreage, ForageAcreage, # Total acres for each crop
         OGVegiSeed, OGFieldSeed, OGForageSeed, # % organic seed used
         TotalAcreage, 
         OGSeedOGIntegrityf, OGSeedOGIntegrityn, # Belief that organic seed is important
         CertifierRequestn, # Certifier has requested
         funding_avg_annual_amt, survey_year, year_range, # year variables (funding)
         producer_n, median_producer_revenue, Region, # regional variables (region and seed production)
         contains("barrier")) # barriers

# Let's narrow this down to the variables of interest
df.long <- df.long %>% 
  filter(Region != "International") %>% 
  select(OGcrop, OGseed_percent, IncreasedOGSeed,
         TotalAcreage, 
         OGSeedOGIntegrityf, OGSeedOGIntegrityn, # Belief that organic seed is important
         CertifierRequestn, # Certifier has requested
         funding_avg_annual_amt, survey_year, year_range, # year variables (funding)
         producer_n, Region, # regional variables (region and seed production)
         contains("barrier")) # barriers

# Where are the NAs in the data?
aggr(df, col=c('navyblue','red'), 
     numbers=TRUE, sortVars=TRUE, 
     labels=names(df), cex.axis=.7, gap=3, 
     ylab=c("Histogram of missing data","Pattern"))

# Summary of dependent variable ----
summary(df$OGseed_percent_maincrop)

df %>% 
  ggplot(aes(x = factor(survey_year), y = OGseed_percent_maincrop)) +
  geom_boxplot(aes(color = maincrop),
               position =  position_dodge2(preserve = "single")) +
  labs(x = "", y = "Percent organic seed used", 
       color = "Main crop") +
  scale_color_manual(values = palette[c(2,4,6,8)]) +
  theme_linedraw()

summary(df.long$OGseed_percent)

df.long %>% 
  ggplot(aes(x = factor(survey_year), y = OGseed_percent)) +
  geom_boxplot(aes(color = OGcrop)) +
  labs(x = "", y = "Percent organic seed used", 
       color = "Crop") +
  scale_color_manual(values = palette[c(3,6,8)]) +
  theme_linedraw()

table(df$IncreasedOGSeed)
table(is.na(df$IncreasedOGSeed), df$survey_year)
df$IncreasedOGSeed <- ifelse(df$OGseed_percent_maincrop == 100 & 
                               is.na(df$IncreasedOGSeed), "Already at 100%", 
                             df$IncreasedOGSeed)
df %>% 
  group_by(survey_year, maincrop, IncreasedOGSeed) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(survey_year, maincrop) %>% 
  mutate(Percent = 100*(n/sum(n))) %>% 
  mutate(IncreasedOGseed = case_when(is.na(IncreasedOGSeed) ~ "No response", 
                                     T ~ IncreasedOGSeed)) %>% 
  ggplot(aes(x = factor(survey_year), 
             y = Percent,
             fill = factor(IncreasedOGSeed, levels = c("No response", "No", 
                                                       "Yes", "Already at 100%")))) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Percent respondents", 
       fill = "Increased organic seed use") +
  scale_fill_manual(values = palette[c(2,4,6,8)]) +
  theme_linedraw() +
  facet_wrap(~maincrop)

df.long %>% 
  group_by(survey_year, OGcrop, IncreasedOGSeed) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(survey_year, OGcrop) %>% 
  mutate(Percent = 100*(n/sum(n))) %>% 
  mutate(IncreasedOGseed = case_when(is.na(IncreasedOGSeed) ~ "No response", 
                                     T ~ IncreasedOGSeed)) %>% 
  ggplot(aes(x = factor(survey_year), 
             y = Percent,
             fill = factor(IncreasedOGSeed, levels = c("No response", "No", 
                                                       "Yes", "Already at 100%")))) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Percent respondents", 
       fill = "Increased organic seed use") +
  scale_fill_manual(values = palette[c(2,4,6,8)]) +
  theme_linedraw() +
  facet_wrap(~OGcrop)


# Summary of "demand" variables ----
## Demand of the grower, based on percieved importance, and demand of the regulator
table(df$OGSeedOGIntegrityf)
table(is.na(df$OGSeedOGIntegrityf))
summary(df$OGSeedOGIntegrityn)

table(df$CertifierRequest)
summary(df$CertifierRequestn)

# Summary of "supply" variables ----
## Funding, at the time-panel level
df %>% 
  select(year_range, survey_year, Region, funding_avg_annual_amt) %>% 
  unique()

## Strength of seed producing regions, at the regional level for one time period
df %>% 
  select(Region, producer_n) %>% 
  unique()

## Strength of seed producing regions, at the regional level for one time period
df %>% 
  select(Region, median_producer_revenue) %>% 
  unique()
# 7 = "$300,000-$1 million"
# 6 = "$75,000-$99,999",
# 5 = "$100,000-$149,999",
# 4 = "$150,000-$299,999",

## Barriers to sourcing organic seed
## This is only available for 2021 survey so maybe run a separate model for this
barriers <- df %>% 
  filter(survey_year == 2021) %>% 
  rename(
    "Lacks seed treatment" = barrier_seedTreatlackf,
    "Save own seed" = barrier_saveOwnf,
    "Price" = barrier_pricef,
    "Lacks desired traits" = barrier_lackdesiredTraitsf,
    "Insufficient quantity" = barrier_insuffQuantityf,
    "Distrust quality" = barrier_distrustQualityf,
    "Buyer requires certain variety" = barrier_buyerReqf,
    "Availability" = barrier_availabilityf
  ) %>% 
  pivot_longer(`Lacks seed treatment`:`Availability`, 
               names_to = "barrier", values_to = "score") %>% 
  filter(!is.na(score)) %>% 
  group_by(barrier, score) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(barrier) %>% 
  mutate(Percent = round(100*(n/sum(n)), 2)) %>% 
  ungroup()

order <- barriers %>% 
  filter(score == "Significant factor" | score == "Moderate factor") %>% 
  group_by(barrier) %>% 
  summarize(n = sum(n)) %>% 
  arrange(n) 


barriers %>% 
  ggplot(aes(y = Percent, x = factor(barrier, levels = order$barrier), 
             fill = score)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(x = "", fill = "") +
  theme_linedraw()

table(df$barrier_seedTreatlack)
table(df$barrier_buyerReq)
table(df$barrier_insuffQuantity)
table(df$barrier_saveOwn)
table(df$barrier_distrustQuality)
table(df$barrier_price)
table(df$barrier_availability)
table(df$barrier_lackdesiredTraits)

# Summary of demographic variables:
table(df$maincrop)
table(df$maincrop, df$survey_year)
table(df.long$OGcrop)
table(df.long$OGcrop, df.long$survey_year)
summary(df$TotalAcreage)
summary(df.long$TotalCropAcreage)

# Last year how many organic acres did you have in annual vegetables. Please fill in number
summary(df$VegiAcreage)
summary(df$FieldAcreage)
summary(df$ForageAcreage)
df$VegiAcreage <- ifelse(df$VegiAcreage < 0, NA, df$VegiAcreage)
df$VegiAcreage <- ifelse(is.na(df$VegiAcreage), 0 , df$VegiAcreage)
df$FieldAcreage <- ifelse(is.na(df$FieldAcreage), 0 , df$FieldAcreage)
df$ForageAcreage <- ifelse(is.na(df$ForageAcreage), 0 , df$ForageAcreage)

# Last year what approximate percentage of total acreage of annual vegetables was planted with certified organic seed. Please fill in number
summary(df$OGVegiSeed)
summary(df$OGFieldSeed)
summary(df$OGForageSeed)

summary(df$VegiAcreage/df$TotalAcreage)
summary(df$FieldAcreage/df$TotalAcreage)
summary(df$ForageAcreage/df$TotalAcreage)

# Certifiers across the US ----
nop <- read.csv("~/Box/organicseed/NOP_USorganicCrops_6.28.22.csv") %>% 
  slice(-(1:2)) %>% 
  filter(Physical.Address..Country == "United States of America (the)")

nop %>% 
  group_by(Certifier.Name) %>% 
  count() %>% 
  arrange(-n)
