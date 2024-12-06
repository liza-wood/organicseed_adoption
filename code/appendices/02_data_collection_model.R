# Model with random versus convenience check, 2021
library(dplyr)
library(ggplot2)
library(betareg)
source('code/functions.R')
# Model using only certifier data
df <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds")

scale_data_app <- function(dat, dv, yr = c('2011', '2016', '2021')){
  dat2 <- dat %>% 
    filter(Region != "International", maincrop != "Unknown", survey_year %in% yr) %>% 
    select(RespondentID, 
           {{dv}}, 
           barrier_availability,
           barrier_insuffQuantity,
           barrier_lackdesiredTraits,
           barrier_seedTreatlack,
           barrier_price,
           barrier_buyerReq,
           OG_value,
           CertifierRequestn, 
           # CertifierRequestf, 
           maincrop, survey_year, Region, 
           TotalAcreage, cropcat_n,
           noncommercial_seedsource,
           YearsCertified,
           SurveyID,
           data_coll_method
    ) %>% 
    mutate(barrier_allowable = purrr::pmap_dbl(list(barrier_availability,
                                                    barrier_insuffQuantity,
                                                    barrier_lackdesiredTraits,
                                                    barrier_seedTreatlack), max),
           barrier_allowable_rev = ifelse(barrier_allowable == 1, 4,
                                          ifelse(barrier_allowable == 2, 3,
                                                 ifelse(barrier_allowable == 3, 2,
                                                        ifelse(barrier_allowable == 4, 1, NA))))) %>% 
    mutate(barrier_allowable_sum = purrr::pmap_dbl(list(barrier_availability,
                                                        barrier_insuffQuantity,
                                                        barrier_lackdesiredTraits,
                                                        barrier_seedTreatlack), sum)) %>% 
    mutate(barrier_nonallowable = purrr::pmap_dbl(list(barrier_price,
                                                       barrier_buyerReq), max), 
           barrier_nonallowable_rev = ifelse(barrier_nonallowable == 1, 4,
                                             ifelse(barrier_nonallowable == 2, 3,
                                                    ifelse(barrier_nonallowable == 3, 2,
                                                           ifelse(barrier_nonallowable == 4, 1, NA))))) %>% 
    mutate(barrier_all = purrr::pmap_dbl(list(barrier_availability,
                                              barrier_insuffQuantity,
                                              barrier_lackdesiredTraits,
                                              barrier_seedTreatlack,
                                              barrier_price,
                                              barrier_buyerReq), max), 
           barrier_all_rev = ifelse(barrier_all == 1, 4,
                                    ifelse(barrier_all == 2, 3,
                                           ifelse(barrier_all == 3, 2,
                                                  ifelse(barrier_all == 4, 1, NA))))) %>% 
    mutate(Region = factor(Region, levels = c("North Central", "West", 
                                              "Northeast", "South")),
           survey_year = factor(survey_year, c(2016, 2011, 2021))) %>% #,
    #CertifierRequestf = factor(CertifierRequestf, levels = c('No', 'Yes', 'Fully compliant'))) %>% 
    na.omit() # do this now so that it scales around the sample we are using
  
  if('IncreasedOGSeed' %in% colnames(dat2)){
    dat3 <- dat2 %>% 
      mutate(IncreasedOGSeed_f = factor(IncreasedOGSeed,
                                        levels = c("No: About the same % or decreased", 
                                                   "Yes: Increased the %", 
                                                   "Already at 100%")))
  } else {
    dat3 <- dat2 %>% 
      mutate(OGseed_percent_maincrop_prop = OGseed_percent_maincrop/100,
             CVseed_percent_maincrop_prop = 1-OGseed_percent_maincrop_prop)
  }
  dat3 %>% 
    mutate(acreage_log = log(TotalAcreage),
           acreage_log_s = c(scale(acreage_log, scale = F)),
           cropcat_n_s = cropcat_n - 1,
           OG_value_s = OG_value - 1,
           noncommercial_seedsource_s = c(scale(noncommercial_seedsource, 
                                                scale = T)),
           barrier_all_rev_s = barrier_all_rev-1,
           years_certified_s = c(scale(YearsCertified, scale = T))
    )
}
df.full <- scale_data_app(df, OGseed_percent_maincrop)
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
                             barrier_all_rev_s +
                             OG_value_s + 
                             barrier_all_rev_s*OG_value_s + 
                             CertifierRequestn +
                             acreage_log_s + 
                             cropcat_n_s +
                             noncommercial_seedsource_s +
                             years_certified_s + 
                             maincrop +
                             Region + 
                             survey_year +
                             data_coll_method, 
             data = df.full_rounded16_21, link = "logit", link.phi = NULL, type = "ML")
summary(m16_21_datacoll)
m16_21_datacoll_df <- prep_betreg_data(m16_21_datacoll)
plot_betareg_models(m16_21_datacoll_df, 1)

m16_21 <- betareg(OGseed_percent_maincrop_prop ~ 
                    barrier_all_rev_s +
                    OG_value_s + 
                    barrier_all_rev_s*OG_value_s + 
                    CertifierRequestn +
                    acreage_log_s + 
                    cropcat_n_s +
                    noncommercial_seedsource_s +
                    years_certified_s + 
                    maincrop +
                    Region + 
                    survey_year,
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
                         barrier_all_rev_s +
                         OG_value_s + 
                         barrier_all_rev_s*OG_value_s + 
                         CertifierRequestn +
                         acreage_log_s + 
                         cropcat_n_s +
                         noncommercial_seedsource_s +
                         years_certified_s + 
                         maincrop +
                         Region + 
                         survey_year +
                         distributor, 
                        data = df.full_rounded11_16, link = "logit", link.phi = NULL, type = "ML")
summary(m11_16_dist)
m11_16_dist_df <- prep_betreg_data(m11_16_dist)
plot_betareg_models(m11_16_dist_df, 1)

m11_16 <- betareg(OGseed_percent_maincrop_prop ~ 
                    barrier_all_rev_s +
                    OG_value_s + 
                    barrier_all_rev_s*OG_value_s + 
                    CertifierRequestn +
                    acreage_log_s + 
                    cropcat_n_s +
                    noncommercial_seedsource_s +
                    years_certified_s + 
                    maincrop +
                    Region + 
                    survey_year, 
               data = df.full_rounded11_16, link = "logit", link.phi = NULL, type = "ML")
summary(m11_16)
m11_16_df <- prep_betreg_data(m11_16)
plot_betareg_models(m11_16_df, 1)

saveRDS(list(m16_21_datacoll, m16_21, m11_16_dist, m11_16), 'tables/append_sample_models.RDS')
