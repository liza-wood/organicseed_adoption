library(VIM)
library(tidyverse)

df <- read.csv("~/Box/organicseed/df.csv")
df.long <- read.csv("~/Box/organicseed/df_long.csv")

# Let's narrow this down to the variables of interest
df <- df %>% 
  filter(Region != "International") %>% 
  select(OGseed_percent_maincrop, maincrop, VegiAcreage, FieldAcreage, ForageAcreage, 
         OGVegiSeed, OGFieldSeed, OGForageSeed, TotalAcreage, 
         OGSeedOGIntegrityf, OGSeedOGIntegrityn, CertifierRequestn, 
         funding_avg_annual_amt, producer_n, survey_year, year_range, Region,
         contains("barrier")) 

# Where are the NAs in the data?
aggr(df, col=c('navyblue','red'), 
     numbers=TRUE, sortVars=TRUE, 
     labels=names(df), cex.axis=.7, gap=3, 
     ylab=c("Histogram of missing data","Pattern"))

# Summary of dependent variable ----
summary(df$OGseed_percent_maincrop)
plot(df$OGseed_percent_maincrop)

df %>% 
  ggplot(aes(x = factor(survey_year), y = OGseed_percent_maincrop)) +
  geom_boxplot(aes(color = maincrop))

df.long %>% 
  ggplot(aes(x = factor(survey_year), y = OGseed_percent)) +
  geom_boxplot()

summary(df.long$OGseed_percent)
plot(df.long$OGseed_percent)

# Summary of "demand" variables ----
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

## Barriers to sourcing organic seed
## This is only available for 2021 survey so maybe run a separate model for this
table(df$barrier_seedTreatlack)
table(df$barrier_buyerReq)
table(df$barrier_insuffQuantity)
table(df$barrier_saveOwn)
table(df$barrier_distrustQuality)
table(df$barrier_price)
table(df$barrier_availability)
table(df$barrier_lackdesiredTraits)

# Summary of demographic variables:

summary(df$TotalAcreage)

# Last year how many organic acres did you have in annual vegetables. Please fill in number
summary(df$VegiAcreage)
df$VegiAcreage <- ifelse(df$VegiAcreage < 0, NA, df$VegiAcreage)
summary(df$FieldAcreage)
summary(df$ForageAcreage)

df$VegiAcreage <- ifelse(is.na(df$VegiAcreage), 0 , df$VegiAcreage)
df$FieldAcreage <- ifelse(is.na(df$FieldAcreage), 0 , df$FieldAcreage)
df$ForageAcreage <- ifelse(is.na(df$ForageAcreage), 0 , df$ForageAcreage)

# Last year what approximate percentage of total acreage of annual vegetables was planted with certified organic seed. Please fill in number
summary(df$OGVegiSeed)
summary(df$OGFieldSeed)
summary(df$OGForageSeed)

df$OGVegiSeed <- ifelse(is.na(df$OGVegiSeed), 0 , df$OGVegiSeed)
df$OGFieldSeed <- ifelse(is.na(df$OGFieldSeed), 0 , df$OGFieldSeed)
df$OGForageSeed <- ifelse(is.na(df$OGForageSeed), 0 , df$OGForageSeed)

summary(df$VegiAcreage/df$TotalAcreage)
summary(df$FieldAcreage/df$TotalAcreage)
summary(df$ForageAcreage/df$TotalAcreage)

# Barriers
df %>% 
  filter(survey_year == 2021) %>% 
  pivot_longer(cols = barrier_availability:barrier_seedTreatlack, 
               names_to = "barrier_type", values_to = "barrier_rank") %>% 
  filter(!is.na(barrier_rank)) %>% 
  ggplot(aes(x = barrier_type, fill = factor(barrier_rank))) +
  geom_bar() +
  coord_flip()

df %>% 
  filter(survey_year == 2021) %>% 
  pivot_longer(cols = barrier_availability:barrier_seedTreatlack, 
               names_to = "barrier_type", values_to = "barrier_rank") %>% 
  filter(!is.na(barrier_rank)) %>% 
  ggplot(aes(x = barrier_type, y = barrier_rank, fill = maincrop)) +
  geom_boxplot() +
  coord_flip()
