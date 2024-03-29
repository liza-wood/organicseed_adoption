# Model with random versus convenience check, 2021
library(dplyr)
library(ggplot2)
source('code/functions.R')
# Model using only certifier data
df.full <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds") %>% 
  filter(Region != "International", maincrop != "Other") %>% 
  select(RespondentID, SurveyID,
         OGseed_percent_maincrop, #dependent variable
         #OGVarietiesSuccessn, # legitimation
         OGSeedOGIntegrityn,
         CertifierRequestn, # guidance of the search
         maincrop, survey_year, Region, # system level features
         TotalAcreage, cropcat_n, # operation controls
         #contains('barrier_'),
         barrier_buyerReq,
         barrier_insuffQuantity,
         barrier_lackdesiredTraits,
         barrier_availability,
         noncommercial_seedsource,
         data_coll_method#,
         #YearsCertified
  ) %>% 
  na.omit() %>%  # do this now so that it scales around the sample we are using
  mutate(OGseed_percent_maincrop_prop = OGseed_percent_maincrop/100,
         CVseed_percent_maincrop_prop = 1-OGseed_percent_maincrop_prop) %>%  # do this now so that it
  mutate(Region = factor(Region, levels = c("North Central", "West", 
                                            "Northeast", "South"))) %>% 
  mutate(OGseed_percent_maincrop_prop = OGseed_percent_maincrop/100,
         CVseed_percent_maincrop_prop = 1-OGseed_percent_maincrop_prop,
         acreage_log = log(TotalAcreage),
         acreage_log_s = c(scale(acreage_log, scale = T)), 
         acreage_s = c(scale(TotalAcreage, scale = T)), 
         cropcat_n_s = c(scale(cropcat_n, scale = T)),
         CertifierRequestn_s = c(scale(CertifierRequestn, scale = T)),
         legitimation_user_s = c(scale(OGSeedOGIntegrityn, scale = T)),
         survey_year = factor(survey_year),
         #barrier_price_s = c(scale(barrier_price, scale = T)),
         barrier_availability_s = c(scale(barrier_availability, scale = T)),
         #barrier_distrustQuality_s = c(scale(barrier_distrustQuality, scale = T)),
         barrier_buyerReq_s = c(scale(barrier_buyerReq, scale = T)),
         #barrier_saveOwn_s  = c(scale(barrier_saveOwn, scale = T)),
         barrier_lackdesiredTraits_s = c(scale(barrier_lackdesiredTraits, scale = T)),
         barrier_insuffQuantity_s = c(scale(barrier_insuffQuantity, scale = T)),
         noncommercial_seedsource_s = c(scale(noncommercial_seedsource, 
                                              scale = T)),
         #years_certified_s = c(scale(YearsCertified, scale = T))
  ) 

# Compare random versus convenience ----
df.full21 <- df.full %>% 
  filter(survey_year %in% c(2016,2021))

## Descriptively, can we see anything obvious?
df.full21 %>% 
  ggplot(aes(y = CVseed_percent_maincrop_prop, color = data_coll_method)) +
  geom_density() +
  theme_minimal()

df.full_rounded21 <- df.full21
df.full_rounded21$CVseed_percent_maincrop_prop <- ifelse(df.full_rounded21$CVseed_percent_maincrop_prop == 1, 0.999, ifelse(df.full_rounded21$CVseed_percent_maincrop_prop == 0, 0.001, df.full_rounded21$CVseed_percent_maincrop_prop))

library(betareg)
set.seed(19)
df.full_rounded21$maincrop <- factor(df.full_rounded21$maincrop, c("Field", "Forage", "Vegetables"))
df.full_rounded21$Region <- factor(df.full_rounded21$Region, c("North Central", "West", "Northeast", "South"))
m21_datacoll <- betareg(CVseed_percent_maincrop_prop ~ 
                          barrier_availability_s +
                          barrier_buyerReq_s +
                          barrier_lackdesiredTraits_s +
                          barrier_insuffQuantity_s +
                          #survey_year +
                          legitimation_user_s + 
                          acreage_s + 
                          cropcat_n_s +
                          noncommercial_seedsource_s +
                          CertifierRequestn_s +
                          maincrop +
                          Region +
                         data_coll_method, 
             data = df.full_rounded21, link = "logit", link.phi = NULL, type = "ML")
summary(m21_datacoll)
m21_datacoll_df <- prep_betreg_data(m21_datacoll)
plot_inla_models(m21_datacoll_df, 1)

m21 <- betareg(CVseed_percent_maincrop_prop ~ 
                          barrier_availability_s +
                          barrier_buyerReq_s +
                          barrier_lackdesiredTraits_s +
                          barrier_insuffQuantity_s +
                          #survey_year +
                          legitimation_user_s + 
                          acreage_s + 
                          cropcat_n_s +
                          noncommercial_seedsource_s +
                          CertifierRequestn_s +
                          maincrop +
                          Region, 
                        data = df.full_rounded21, link = "logit", link.phi = NULL, type = "ML")
summary(m21)
m21_df <- prep_betreg_data(m21)
plot_inla_models(m21_df, 1)

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

library(betareg)
set.seed(19)

df.full_rounded11_16 <- df.full11_16
df.full_rounded11_16$CVseed_percent_maincrop_prop <- ifelse(df.full_rounded11_16$CVseed_percent_maincrop_prop == 1, 0.999, ifelse(df.full_rounded11_16$CVseed_percent_maincrop_prop == 0, 0.001, df.full_rounded11_16$CVseed_percent_maincrop_prop))

df.full_rounded11_16$maincrop <- factor(df.full_rounded11_16$maincrop, c("Field", "Forage", "Vegetables"))
df.full_rounded11_16$Region <- factor(df.full_rounded11_16$Region, c("North Central", "West", "Northeast", "South"))
df.full_rounded11_16$distributor <- factor(df.full_rounded11_16$distributor, c("Other", "OSA"))
m11_16_dist <- betareg(CVseed_percent_maincrop_prop ~ 
                          barrier_availability_s +
                          barrier_buyerReq_s +
                          barrier_lackdesiredTraits_s +
                          barrier_insuffQuantity_s +
                          survey_year +
                          legitimation_user_s + 
                          acreage_s + 
                          cropcat_n_s +
                          noncommercial_seedsource_s +
                          CertifierRequestn_s +
                          maincrop +
                          Region +
                         distributor, 
                        data = df.full_rounded11_16, link = "logit", link.phi = NULL, type = "ML")
summary(m11_16_dist)
m11_16_dist_df <- prep_betreg_data(m11_16_dist)
plot_inla_models(m11_16_dist_df, 1)

m11_16 <- betareg(CVseed_percent_maincrop_prop ~ 
                 barrier_availability_s +
                 barrier_buyerReq_s +
                 barrier_lackdesiredTraits_s +
                 barrier_insuffQuantity_s +
                 survey_year +
                 legitimation_user_s + 
                 acreage_s + 
                 cropcat_n_s +
                 noncommercial_seedsource_s +
                 CertifierRequestn_s +
                 maincrop +
                 Region, 
               data = df.full_rounded11_16, link = "logit", link.phi = NULL, type = "ML")
summary(m11_16)
m11_16_df <- prep_betreg_data(m11_16)
plot_inla_models(m11_16_df, 1)

saveRDS(list(m21_datacoll, m21, m11_16_dist, m11_16), 'tables/tablesa5_models.RDS')



ggplot(df.full11_16, aes(x = survey_year, fill = distributor)) +
  geom_bar(position = "dodge")

ggplot(df.full11_16, aes(x = survey_year, 
                         y = CVseed_percent_maincrop_prop, 
                         fill = distributor)) +
  geom_boxplot()

ggplot(df.full11_16, aes(x = survey_year, 
                         y = OGSeedOGIntegrityn, 
                         fill = distributor)) +
  geom_boxplot()

stderror <- function(x) sd(x)/sqrt(length(x))

df.full11_16 %>% 
  group_by(survey_year, distributor) %>% 
  summarise(mean = mean(CVseed_percent_maincrop_prop, na.rm = T),
            sd = sd(CVseed_percent_maincrop_prop, na.rm = T),
            se = stderror(CVseed_percent_maincrop_prop)) %>% 
  ggplot(aes(x=survey_year, y=mean, colour=distributor)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  geom_line() +
  geom_point()

df.full11_16 %>% 
  group_by(survey_year, distributor) %>% 
  summarise(mean = mean(OGSeedOGIntegrityn, na.rm = T),
            sd = sd(OGSeedOGIntegrityn, na.rm = T),
            se = stderror(OGSeedOGIntegrityn)) %>% 
  ggplot(aes(x=survey_year, y=mean, colour=distributor)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  geom_line() +
  geom_point()
