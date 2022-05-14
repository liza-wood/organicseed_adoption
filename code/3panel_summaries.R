library(dplyr)
library(tidyr)
??pivot_longer
df <- read.csv("~/Box/organicseed/merged_df_3panel.csv")
table(is.na(df$Region)) # Need to go back and assess this

# It is important
df$OGVarietiesSuccess
df$OGSeedOGIntegrityf <- factor(df$OGSeedOGIntegrity, levels = c("Strongly disagree",
                                                                 "Disagree",
                                                                 "Neutral",
                                                                 "Agree",
                                                                 "Strongly agree"))
df$OGSeedOGIntegrityn <- as.numeric(df$OGSeedOGIntegrityf)
summary(df$OGSeedOGIntegrityf)
summary(df$OGSeedOGIntegrityn)

# Demographic
df$TotalAcreage

# Last year how many organic acres did you have in annual vegetables. Please fill in number
summary(df$VegiAcreage)
# Last year what approximate percentage of total acreage of annual vegetables was planted with certified organic seed. Please fill in number
summary(df$OGVegiSeed)

# Not sure what this is then
df$OGVegSeedAcres
# It isn't this:
df$VegiAcreage[2]*(df$OGVegiSeed[2]/100)
df$OGVegSeedAcres[2]

# Acres in field crops
summary(df$FieldAcreage)
# Percent 
summary(df$OGFieldSeed)

# Acres in forage crops
summary(df$ForageAcreage)
# Percent 
summary(df$OGForageSeed)

summary(df$VegiAcreage/df$TotalAcreage)
summary(df$FieldAcreage/df$TotalAcreage)
summary(df$ForageAcreage/df$TotalAcreage)

# I SHOULDNT REDUCE TO ONE CROP -- I think because we have acreage by crop and percenatge of that, then we can maybe lengthen the data and h
?pivot_longer


df.long <- pivot_longer(df, cols = c(OGVegiSeed, OGFieldSeed, OGForageSeed),
                        names_to = "OGSeed_type", values_to = "OGSeed_percent") %>% 
  filter(!is.na(OGSeed_percent))


df <- df %>% 
  mutate(maincrop = case_when(
    df$VegiAcreage/df$TotalAcreage >= .5 ~ "Vegetables",
    df$FieldAcreage/df$TotalAcreage >= .5 ~ "Field",
    df$ForageAcreage/df$TotalAcreage >= .5 ~ "Forage",
    T ~ "Mix"
  )) %>% 
  mutate(OGseed_percent_maincrop = case_when(
    maincrop == "Vegetables" ~ OGVegiSeed,
    maincrop == "Field" ~ OGFieldSeed,
    maincrop == "Forage" ~ OGForageSeed,
    T ~ (OGVegiSeed + OGFieldSeed + OGForageSeed)/3, 
  ))

table(df$maincrop)
summary(df$OGseed_percent_maincrop)

table(df$CertifierRequest)

#OGseed_percent_maincrop ~ maincrop + TotalAcreage + OGSeedOGIntegrityn + survey_year + (1|region) CertifierRequest

library(lme4)
library(brms)
?brm
fitb <- brm(OGseed_percent_maincrop ~ maincrop + TotalAcreage + OGSeedOGIntegrityn + CertifierRequest + survey_year + Region, data = df)
summary(fitb)
plot(fitb)

fitb <- brm(OGSeed_percent ~ OGSeed_type + TotalAcreage + OGSeedOGIntegrityn + CertifierRequest + survey_year + Region, data = df.long)
summary(fitb)

summary(df$OGFieldSeed) # highest mean and median
summary(df$OGVegiSeed)
summary(df$OGForageSeed)

fit0 <- lm(OGseed_percent_maincrop ~ maincrop + TotalAcreage + OGSeedOGIntegrityn + CertifierRequest + survey_year + Region, data = df)
summary(fit0)

fit0 <- lm(OGSeed_percent ~ OGSeed_type + TotalAcreage + OGSeedOGIntegrityn + CertifierRequest + survey_year + Region, data = df.long)
summary(fit0)

west <- filter(df, Region == "West")
ncentral <- filter(df, Region == "North Central")
south <- filter(df, Region == "South")
neast <- filter(df, Region == "Northeast")

fitwest <- lm(OGseed_percent_maincrop ~ maincrop + TotalAcreage + OGSeedOGIntegrityn + CertifierRequest + survey_year, data = west)
summary(fitwest)
fitncentral <- lm(OGseed_percent_maincrop ~ maincrop + TotalAcreage + OGSeedOGIntegrityn + CertifierRequest + survey_year, data = ncentral)
summary(fitncentral)
fitneast <- lm(OGseed_percent_maincrop ~ maincrop + TotalAcreage + OGSeedOGIntegrityn + CertifierRequest + survey_year, data = neast)
summary(fitneast)
fitsouth <- lm(OGseed_percent_maincrop ~ maincrop + TotalAcreage + OGSeedOGIntegrityn + CertifierRequest + survey_year, data = south)
summary(fitsouth)



fit <- lmer(OGseed_percent_maincrop ~ maincrop + TotalAcreage + OGSeedOGIntegrityn + CertifierRequest + survey_year + (1|Region), data = df)
summary(fit)



coef(fit)$Region$`(Intercept)` # Random effects plus fixed effects; # extract the coefficient but different for each state-- this gives us intercepts bc we have no x and therefore no slope. 
unique(df$Region)
ranef(fit)$Region$`(Intercept)` # This will tell us how far the state is away from the overall mean
state_variation <- data.frame(dist.from.mean = ranef(glmer_agree)$state$`(Intercept)`,
                              state = unique(df.full$state))



