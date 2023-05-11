# Can I see where the correlation is, at all
library(dplyr)
library(lme4)
setwd("~/Documents/Davis/R-Projects/organicseed_adoption/")
df <- readRDS("~/Box/organicseed_adoption/df_multilevel.rds") 

# Select data ----
# Considering acreage as the dependent variable 
## Use c() when scaling numbers because it "strips" the dimension (i.e. scaling converts them into some kind of matrix and Stack Overflow told me that c would strip this)
df.full <- df %>% 
  filter(Region != "International", maincrop != "Other") %>% 
  select(RespondentID, 
         OGseed_percent_maincrop, #dependent variable
         OGVarietiesSuccessn, # legitimation
         CertifierRequestn, # guidance of the search
         total_awards, # knowledge development/resource mobilization
         ratio, # entrepreneurial activities 
         organicip_n, # knowledge development
         # sales would be market formation?
         maincrop, survey_year, Region, # system level features
         TotalAcreage, cropcat_n # operation controls
  ) %>% 
  mutate(Region = factor(Region, levels = c("North Central", "West", 
                                            "Northeast", "South"))) %>% 
  mutate(acreage_log = c(scale(log(TotalAcreage), scale = F)), 
         cropcat_n = c(scale(cropcat_n, scale = F)),
         funding_million = c(scale(total_awards/1000000, scale = F)),
         legitimation = c(scale(OGVarietiesSuccessn, scale = F)),
         ratio = c(scale(ratio, scale = F)), # entrepreneurial activities 
         organicip = c(scale(organicip_n, scale = F)), # knowledge development
         survey_year = factor(survey_year))

funds <- read.csv('~/Box/quantifying_seed_innovation/organicseed_funding_year_region_crop.csv') %>% 
  filter(year >= 2002)

joined <- full_join(df.full, funds) 
df2021 <- filter(joined, survey_year == 2021)

timescale_check <- function(data){
  model <- lm(OGseed_percent_maincrop ~ 
              legitimation +
              CertifierRequestn +
              award_dollars +
              ratio + 
              #organicip +
              #Region +
              #maincrop +
              acreage_log + 
              cropcat_n, data = data)
  summary <- summary(model)
  awards <- data.frame(summary$coefficients)[4,]
  return(awards)
}

range <- 2002:2019
coef.df <- data.frame()
for(i in range){
  data <- joined[joined$year == i,]
  outputs <- timescale_check(data)
  outputs$year = i
  coef.df <- rbind(coef.df, outputs)
}

ggplot(coef.df, aes(x = year, y = `t.value`)) +
  geom_col()
ggplot(coef.df, aes(x = year, y = Estimate)) +
  geom_col() 
