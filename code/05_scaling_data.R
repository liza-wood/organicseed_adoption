library(dplyr)
df <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds")

# Scale data ----
# Considering acreage as the dependent variable 
## Use c() when scaling numbers because it "strips" the dimension (i.e. scaling converts them into some kind of matrix and Stack Overflow told me that c would strip this)

df <- df %>% 
  filter(Region != "International", maincrop != "Other") %>% 
  select(RespondentID, 
         OGseed_percent_maincrop, 
         barrier_availability,
         barrier_insuffQuantity,
         barrier_lackdesiredTraits,
         OG_value,
         CertifierRequestn, 
         maincrop, survey_year, Region, 
         TotalAcreage, cropcat_n,
         noncommercial_seedsource
  ) %>% 
  na.omit() %>%  # do this now so that it scales around the sample we are using
  mutate(Region = factor(Region, levels = c("North Central", "West", 
                                            "Northeast", "South")),
         survey_year = factor(survey_year, c(2016, 2011, 2021))) %>% 
  mutate(OGseed_percent_maincrop_prop = OGseed_percent_maincrop/100,
         CVseed_percent_maincrop_prop = 1-OGseed_percent_maincrop_prop,
         acreage_log = log(TotalAcreage),
         acreage_log_s = c(scale(acreage_log, scale = F)),
         #acreage_s = c(scale(TotalAcreage, scale = T)), 
         cropcat_n_s = c(scale(cropcat_n, scale = T)),
         #CertifierRequestn_s = c(scale(CertifierRequestn, scale = T)),
         OG_value_s = c(scale(OG_value, scale = T)),
         survey_year = factor(survey_year),
         barrier_availability_s = c(scale(barrier_availability, scale = T)),
         barrier_lackdesiredTraits_s = c(scale(barrier_lackdesiredTraits, scale = T)),
         barrier_insuffQuantity_s = c(scale(barrier_insuffQuantity, scale = T)),
         noncommercial_seedsource_s = c(scale(noncommercial_seedsource, 
                                              scale = T)),
         #barrier_distrustQuality_s = c(scale(barrier_distrustQuality, scale = T)),
         #barrier_buyerReq_s = c(scale(barrier_buyerReq, scale = T)),
         #barrier_saveOwn_s  = c(scale(barrier_saveOwn, scale = T)),
         #barrier_price_s = c(scale(barrier_price, scale = T)),
         #years_certified_s = c(scale(YearsCertified, scale = T))
         )

# log scale and mean centered
lsmc <- c(scale(log(df$TotalAcreage), scale = F))
summary(log(df$TotalAcreage))
summary(lsmc)
# mean centered and log scaled -- cant' do this
mcls <- log(c(scale(df$TotalAcreage, scale = F)))
summary(df$acreage_log_s)

saveRDS(df, "~/Box/organicseed_adoption/df_scaled.rds")
saveRDS(df, "~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled.rds")
