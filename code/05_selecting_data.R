library(dplyr)
df <- readRDS("~/Box/organicseed_adoption/df_multilevel.rds")

# 1. Select and scale data ----
# Considering acreage as the dependent variable 
## Use c() when scaling numbers because it "strips" the dimension (i.e. scaling converts them into some kind of matrix and Stack Overflow told me that c would strip this)

df.full <- df %>% 
  filter(Region != "International", maincrop != "Other") %>% 
  select(RespondentID, 
         OGseed_percent_maincrop, #dependent variable
         OGVarietiesSuccessn, # legitimation
         CertifierRequestn, # guidance of the search
         annual_average, # knowledge development/resource mobilization
         ratio_building, # entrepreneurial activities 
         supplychain_n_avg,
         dnb_regional_sales, # sales would be market formation?
         organicip_n, # knowledge development
         n_nosb_recs,
         maincrop, survey_year, Region, # system level features
         TotalAcreage, cropcat_n, # operation controls
         contains('barrier_'),
         SeedSourceOwn,
         noncommercial_seedsource,
         YearsCertified,
         og_sale_increase
  ) %>% 
  mutate(Region = factor(Region, levels = c("North Central", "West", 
                                            "Northeast", "South"))) %>% 
  mutate(OGseed_percent_maincrop_prop = OGseed_percent_maincrop/100,
         OGseed_percent_maincrop_binary = ifelse(OGseed_percent_maincrop >= 50, 1, 
                                                 ifelse(OGseed_percent_maincrop < 50, 0, NA)),
         acreage_log = c(scale(log(TotalAcreage), scale = F)), 
         cropcat_n_untransformed = cropcat_n,
         cropcat_n = c(scale(cropcat_n, scale = F)),
         resources_funding_100thous = c(scale(annual_average/100000, scale = F)),
         legitimation_user = c(scale(OGVarietiesSuccessn, scale = F)),
         ratio = c(scale(ratio_building, scale = F)), # entrepreneurial activities 
         entrants_supplychain_n_avg_s = c(scale(supplychain_n_avg, scale = F)),
         market_sales_mill = c(scale(dnb_regional_sales/1000000, scale = F)),
         knowledge_organicip_10 = c(scale(organicip_n/10, scale = F)), # knowledge development
         guidance_n_nosb_recs_s = c(scale(n_nosb_recs, scale = F)),
         survey_year = factor(survey_year),
         barrier_availability_s = c(scale(barrier_availability, scale = F)),
         barrier_distrustQuality_s = c(scale(barrier_distrustQuality, scale = F)),
         barrier_buyerReq_s = c(scale(barrier_buyerReq, scale = F)),
         barrier_saveOwn_s  = c(scale(barrier_saveOwn, scale = F)),
         barrier_lackdesiredTraits_s = c(scale(barrier_lackdesiredTraits, scale = F)),
         barrier_insuffQuantity_s = c(scale(barrier_insuffQuantity, scale = F)),
         SeedSourceOwn_s = c(scale(SeedSourceOwn/10, scale = F)),
         noncommercial_seedsource_s = c(scale(noncommercial_seedsource/10, scale = F)),
         years_certified_s = c(scale(YearsCertified, scale = F)),
         og_sale_increase_s = c(scale(og_sale_increase, scale = F)))

summary(df.full$acreage_log)
summary(df.full$cropcat_n)
summary(df.full$resources_funding_100thous)
summary(df.full$legitimation_user)
summary(df.full$ratio)
summary(df.full$entrants_supplychain_n_avg_s)
summary(df.full$noncommercial_seedsource_s)
summary(df.full$years_certified_s)

# 3. Select complete cases ----
dfm1 <- select(df.full, 
               OGseed_percent_maincrop, 
               OGseed_percent_maincrop_prop, 
               OGseed_percent_maincrop_binary,
               maincrop,
               CertifierRequestn,
               acreage_log, 
               #cropcat_n,
               resources_funding_100thous,
               legitimation_user,
               #n_nosb_recs_s,
               ratio, 
               #entrants_supplychain_n_avg_s,
               #sales_mill,
               knowledge_organicip_10,
               #barrier_availability_s,
               #barrier_distrustQuality_s,
               #barrier_buyerReq_s,
               #barrier_saveOwn_s,
               #barrier_lackdesiredTraits_s,
               #barrier_insuffQuantity_s,
               Region, 
               survey_year,
               #SeedSourceOwn_s
               noncommercial_seedsource_s,
               years_certified_s) %>% 
  filter(complete.cases(.))

saveRDS(df.full, "~/Box/organicseed_adoption/df_full.rds")
saveRDS(dfm1, "~/Box/organicseed_adoption/dfm1.rds")
