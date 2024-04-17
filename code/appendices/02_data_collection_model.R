# Model with random versus convenience check, 2021
library(dplyr)
library(ggplot2)
library(betareg)
source('code/functions.R')
# Model using only certifier data
df.full <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds") %>% 
  filter(Region != "International", maincrop != "Other") %>% 
  select(RespondentID, SurveyID,
         OGseed_percent_maincrop, 
         OG_value,
         CertifierRequestn, 
         maincrop, survey_year, Region, 
         TotalAcreage, cropcat_n,
         barrier_insuffQuantity,
         barrier_lackdesiredTraits,
         barrier_availability,
         barrier_seedTreatlack,
         noncommercial_seedsource,
         data_coll_method
  ) %>% 
  na.omit() %>%  # do this now so that it scales around the sample we are using
  mutate(OGseed_percent_maincrop_prop = OGseed_percent_maincrop/100) %>% 
  mutate(Region = factor(Region, levels = c("North Central", "West", 
                                            "Northeast", "South")),
         survey_year = factor(survey_year, c(2016, 2011, 2021))) %>% 
  mutate(OGseed_percent_maincrop_prop = OGseed_percent_maincrop/100,
         acreage_log = log(TotalAcreage),
         acreage_log_s = c(scale(acreage_log, scale = T)), 
         cropcat_n_s = c(scale(cropcat_n, scale = T)),
         OG_value_s = c(scale(OG_value, scale = T)),
         barrier_availability_s = c(scale(barrier_availability, scale = T)),
         barrier_lackdesiredTraits_s = c(scale(barrier_lackdesiredTraits, scale = T)),
         barrier_insuffQuantity_s = c(scale(barrier_insuffQuantity, scale = T)),
         barrier_seedTreatlack_s = c(scale(barrier_seedTreatlack, scale = T)),
         noncommercial_seedsource_s = c(scale(noncommercial_seedsource, 
                                              scale = T)),
         #years_certified_s = c(scale(YearsCertified, scale = T))
  ) 

# Compare random versus convenience ----
df.full16_21 <- df.full %>% 
  filter(survey_year %in% c(2016,2021))

df.full_rounded16_21 <- df.full16_21
df.full_rounded16_21$OGseed_percent_maincrop_prop <- ifelse(df.full_rounded16_21$OGseed_percent_maincrop_prop == 1, 0.999, ifelse(df.full_rounded16_21$OGseed_percent_maincrop_prop == 0, 0.001, df.full_rounded16_21$OGseed_percent_maincrop_prop))

set.seed(19)
df.full_rounded16_21$maincrop <- factor(df.full_rounded16_21$maincrop, 
                                        c("Field", "Forage", "Vegetables"))
df.full_rounded16_21$Region <- factor(df.full_rounded16_21$Region, 
                                      c("North Central", "West", "Northeast", "South"))
m16_21_datacoll <- betareg(OGseed_percent_maincrop_prop ~ 
                             barrier_availability_s +
                             barrier_lackdesiredTraits_s +
                             barrier_insuffQuantity_s +
                             barrier_seedTreatlack_s +
                             survey_year +
                             OG_value_s + 
                             acreage_log_s + 
                             cropcat_n_s +
                             CertifierRequestn +
                             noncommercial_seedsource_s +
                             maincrop +
                             Region + 
                             data_coll_method, 
             data = df.full_rounded16_21, link = "logit", link.phi = NULL, type = "ML")
summary(m16_21_datacoll)
m16_21_datacoll_df <- prep_betreg_data(m16_21_datacoll)
plot_betareg_models(m16_21_datacoll_df, 1)

m16_21 <- betareg(OGseed_percent_maincrop_prop ~ 
                    barrier_availability_s +
                    barrier_lackdesiredTraits_s +
                    barrier_insuffQuantity_s +
                    barrier_seedTreatlack_s +
                    survey_year +
                    OG_value_s + 
                    acreage_log_s + 
                    cropcat_n_s +
                    CertifierRequestn +
                    noncommercial_seedsource_s +
                    maincrop +
                    Region, 
                    data = df.full_rounded16_21, link = "logit", link.phi = NULL, type = "ML")
summary(m16_21)
m16_21_df <- prep_betreg_data(m16_21)
plot_betareg_models(m16_21_df, 1)

# Compare OSA versus other versus random ----
df.full11_16 <- filter(df.full, survey_year != 2021)
sort(unique(df.full11_16$SurveyID))

df.full11_16 <- df.full11_16 %>% 
  mutate(distributor = case_when(
    SurveyID %in% c("Draft Volunteers", "OSA", "OSA (general)", "OSA2",
                    "Email Invitation 1", "Email Invitation 2", "Sample Group Manual Entry") ~ "OSA",
    T ~ "Other"
  ))

table(df.full11_16$distributor, df.full11_16$survey_year)

df.full_rounded11_16 <- df.full11_16
df.full_rounded11_16$OGseed_percent_maincrop_prop <- ifelse(df.full_rounded11_16$OGseed_percent_maincrop_prop == 1, 0.999, ifelse(df.full_rounded11_16$OGseed_percent_maincrop_prop == 0, 0.001, df.full_rounded11_16$OGseed_percent_maincrop_prop))

df.full_rounded11_16$maincrop <- factor(df.full_rounded11_16$maincrop, 
                                        c("Field", "Forage", "Vegetables"))
df.full_rounded11_16$Region <- factor(df.full_rounded11_16$Region, 
                                      c("North Central", "West", "Northeast", "South"))
df.full_rounded11_16$distributor <- factor(df.full_rounded11_16$distributor, 
                                           c("Other", "OSA"))
m11_16_dist <- betareg(OGseed_percent_maincrop_prop ~ 
                         barrier_availability_s +
                         barrier_lackdesiredTraits_s +
                         barrier_insuffQuantity_s +
                         barrier_seedTreatlack_s +
                         survey_year +
                         OG_value_s + 
                         acreage_log_s + 
                         cropcat_n_s +
                         CertifierRequestn +
                         noncommercial_seedsource_s +
                         maincrop +
                         Region +
                         distributor, 
                        data = df.full_rounded11_16, link = "logit", link.phi = NULL, type = "ML")
summary(m11_16_dist)
m11_16_dist_df <- prep_betreg_data(m11_16_dist)
plot_betareg_models(m11_16_dist_df, 1)

m11_16 <- betareg(OGseed_percent_maincrop_prop ~ 
                    barrier_availability_s +
                    barrier_lackdesiredTraits_s +
                    barrier_insuffQuantity_s +
                    barrier_seedTreatlack_s +
                    survey_year +
                    OG_value_s + 
                    acreage_log_s + 
                    cropcat_n_s +
                    CertifierRequestn +
                    noncommercial_seedsource_s +
                    maincrop +
                    Region, 
               data = df.full_rounded11_16, link = "logit", link.phi = NULL, type = "ML")
summary(m11_16)
m11_16_df <- prep_betreg_data(m11_16)
plot_betareg_models(m11_16_df, 1)

saveRDS(list(m16_21_datacoll, m16_21, m11_16_dist, m11_16), 'tables/append_sample_models.RDS')
