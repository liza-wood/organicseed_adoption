library(dplyr)
library(betareg)
df <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds")

# Scale data per year ----

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
  mutate(Region = factor(Region, levels = c("North Central", "West", 
                                            "Northeast", "South")),
         OGseed_percent_maincrop_prop = OGseed_percent_maincrop/100) 

df.full_rounded <- df
df.full_rounded$OGseed_percent_maincrop_prop <- ifelse(df.full_rounded$OGseed_percent_maincrop_prop == 1, 0.999, ifelse(df.full_rounded$OGseed_percent_maincrop_prop == 0, 0.001, df.full_rounded$OGseed_percent_maincrop_prop))

df11 <- filter(df.full_rounded, survey_year == 2011) %>% 
  na.omit() %>% 
  mutate(acreage_log = log(TotalAcreage),
         acreage_log_s = c(scale(acreage_log, scale = F)),
         cropcat_n_s = c(scale(cropcat_n, scale = T)),
         OG_value_s = c(scale(OG_value, scale = T)),
         barrier_availability_s = c(scale(barrier_availability, scale = T)),
         barrier_lackdesiredTraits_s = c(scale(barrier_lackdesiredTraits, scale = T)),
         barrier_insuffQuantity_s = c(scale(barrier_insuffQuantity, scale = T)),
         noncommercial_seedsource_s = c(scale(noncommercial_seedsource, 
                                              scale = T))
  )

df16 <- filter(df.full_rounded, survey_year == 2016) %>% 
  mutate(acreage_log = log(TotalAcreage),
         acreage_log_s = c(scale(acreage_log, scale = F)),
         cropcat_n_s = c(scale(cropcat_n, scale = T)),
         OG_value_s = c(scale(OG_value, scale = T)),
         barrier_availability_s = c(scale(barrier_availability, scale = T)),
         barrier_lackdesiredTraits_s = c(scale(barrier_lackdesiredTraits, scale = T)),
         barrier_insuffQuantity_s = c(scale(barrier_insuffQuantity, scale = T)),
         noncommercial_seedsource_s = c(scale(noncommercial_seedsource, 
                                              scale = T))
  )
df21 <- filter(df.full_rounded, survey_year == 2021) %>% 
  mutate(acreage_log = log(TotalAcreage),
         acreage_log_s = c(scale(acreage_log, scale = F)),
         cropcat_n_s = c(scale(cropcat_n, scale = T)),
         OG_value_s = c(scale(OG_value, scale = T)),
         barrier_availability_s = c(scale(barrier_availability, scale = T)),
         barrier_lackdesiredTraits_s = c(scale(barrier_lackdesiredTraits, scale = T)),
         barrier_insuffQuantity_s = c(scale(barrier_insuffQuantity, scale = T)),
         noncommercial_seedsource_s = c(scale(noncommercial_seedsource, 
                                              scale = T))
  )

set.seed(19)
m11 <- betareg(OGseed_percent_maincrop_prop ~ 
               barrier_availability_s +
               barrier_lackdesiredTraits_s +
               barrier_insuffQuantity_s +
               OG_value_s + 
               acreage_log_s + 
               cropcat_n_s +
               CertifierRequestn +
               noncommercial_seedsource_s +
               maincrop +
               Region, 
             data = df11, link = "logit", link.phi = NULL, type = "ML")

set.seed(19)
m16 <- betareg(OGseed_percent_maincrop_prop ~ 
                 barrier_availability_s +
                 barrier_lackdesiredTraits_s +
                 barrier_insuffQuantity_s +
                 OG_value_s + 
                 acreage_log_s + 
                 cropcat_n_s +
                 CertifierRequestn +
                 noncommercial_seedsource_s +
                 maincrop +
                 Region, 
               data = df16, link = "logit", link.phi = NULL, type = "ML")

set.seed(19)
m21 <- betareg(OGseed_percent_maincrop_prop ~ 
                 barrier_availability_s +
                 barrier_lackdesiredTraits_s +
                 barrier_insuffQuantity_s +
                 OG_value_s + 
                 acreage_log_s + 
                 cropcat_n_s +
                 CertifierRequestn +
                 noncommercial_seedsource_s +
                 maincrop +
                 Region, 
               data = df21, link = "logit", link.phi = NULL, type = "ML")

summary(m11)
summary(m16)
summary(m21)
m_list <- list(m11, m16, m21)
saveRDS(m_list, 'tables/append_models_over_time.rds')
