# Other barriers
library(dplyr)
df <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds")

# Scale data ----
# Considering acreage as the dependent variable 
## Use c() when scaling numbers because it "strips" the dimension (i.e. scaling converts them into some kind of matrix and Stack Overflow told me that c would strip this)

df.full <- df %>% 
    filter(Region != "International", maincrop != "Unknown") %>% 
    select(RespondentID, 
           OGseed_percent_maincrop, 
           barrier_availability,
           barrier_insuffQuantity,
           barrier_lackdesiredTraits,
           barrier_seedTreatlack,
           barrier_price,
           barrier_buyerReq,
           OG_value,
           CertifierRequestn, 
           maincrop, survey_year, Region, 
           TotalAcreage, cropcat_n,
           noncommercial_seedsource,
           YearsCertified
    ) %>% 
    mutate(barrier_allowable = purrr::pmap_dbl(list(barrier_availability,
                                                    barrier_insuffQuantity,
                                                    barrier_lackdesiredTraits,
                                                    barrier_seedTreatlack), max),
           barrier_allowable_rev = ifelse(barrier_allowable == 1, 4,
                                   ifelse(barrier_allowable == 2, 3,
                                   ifelse(barrier_allowable == 3, 2,
                                   ifelse(barrier_allowable == 4, 1, NA))))) %>% 
    mutate(barrier_nonallowable = purrr::pmap_dbl(list(barrier_price,
                                                       barrier_buyerReq), max), 
           barrier_nonallowable_rev = ifelse(barrier_nonallowable == 1, 4,
                                      ifelse(barrier_nonallowable == 2, 3,
                                      ifelse(barrier_nonallowable == 3, 2,
                                      ifelse(barrier_nonallowable == 4, 1, NA))))) %>% 
    mutate(Region = factor(Region, levels = c("North Central", "West", 
                                              "Northeast", "South")),
           survey_year = factor(survey_year, c(2016, 2011, 2021))) %>% 
    na.omit() %>% 
    mutate(OGseed_percent_maincrop_prop = OGseed_percent_maincrop/100,
           CVseed_percent_maincrop_prop = 1-OGseed_percent_maincrop_prop) %>% 
    mutate(acreage_log = log(TotalAcreage),
           acreage_log_s = c(scale(acreage_log, scale = F)),
           OG_value_s = OG_value - 1,
           cropcat_n_s = cropcat_n - 1,
           noncommercial_seedsource_s = c(scale(noncommercial_seedsource, 
                                                scale = T)),
           barrier_allowable_rev_s = barrier_allowable_rev-1,
           barrier_nonallowable_rev_s = barrier_nonallowable_rev-1,
           barrier_allowable_s = barrier_allowable-1,
           barrier_nonallowable_s = barrier_nonallowable-1,
           years_certified_s = c(scale(YearsCertified, scale = T)),
           years_certified_log = log((YearsCertified+1))
    )

df.full_rounded <- df.full
df.full_rounded$OGseed_percent_maincrop_prop <- ifelse(df.full_rounded$OGseed_percent_maincrop_prop == 1, 0.999, ifelse(df.full_rounded$OGseed_percent_maincrop_prop == 0, 0.001, df.full_rounded$OGseed_percent_maincrop_prop))

cor(df.full$barrier_allowable_rev,
    df.full$barrier_nonallowable_rev)
ggplot(df.full, aes(barrier_allowable_rev, barrier_nonallowable_rev)) +
  geom_point(alpha = 0.2, position = 'jitter') + geom_smooth(method = "lm")

library(betareg)
set.seed(19)
m <- betareg(OGseed_percent_maincrop_prop ~ 
               barrier_allowable_rev_s +
               barrier_nonallowable_rev_s +
               OG_value_s + 
               CertifierRequestn + 
               acreage_log_s + 
               cropcat_n_s +
               noncommercial_seedsource_s +
               years_certified_s + 
               maincrop +
               Region +
               survey_year, 
             data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m)

m_ix1 <- betareg(OGseed_percent_maincrop_prop ~ 
               barrier_allowable_rev_s +
               barrier_nonallowable_rev_s +
               OG_value_s + 
               barrier_allowable_rev_s*OG_value_s +
               #barrier_nonallowable_rev_s*OG_value_s +
               CertifierRequestn + 
               acreage_log_s + 
               cropcat_n_s +
               noncommercial_seedsource_s +
               years_certified_s + 
               maincrop +
               Region +
               survey_year, 
             data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m_ix1)

m_ix2 <- betareg(OGseed_percent_maincrop_prop ~ 
                   barrier_allowable_rev_s +
                   barrier_nonallowable_rev_s +
                   OG_value_s + 
                   #barrier_allowable_rev_s*OG_value_s +
                   barrier_nonallowable_rev_s*OG_value_s +
                   CertifierRequestn + 
                   acreage_log_s + 
                   cropcat_n_s +
                   noncommercial_seedsource_s +
                   years_certified_s + 
                   maincrop +
                   Region +
                   survey_year, 
                 data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m_ix2)

m_ix3 <- betareg(OGseed_percent_maincrop_prop ~ 
                   barrier_allowable_rev_s +
                   barrier_nonallowable_rev_s +
                   OG_value_s + 
                   barrier_allowable_rev_s*OG_value_s +
                   barrier_nonallowable_rev_s*OG_value_s +
                   CertifierRequestn + 
                   acreage_log_s + 
                   cropcat_n_s +
                   noncommercial_seedsource_s +
                   years_certified_s + 
                   maincrop +
                   Region +
                   survey_year, 
                 data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m_ix3)

saveRDS(list(m, m_ix1, m_ix2, m_ix3), 'tables/append_models_barriers.rds')
