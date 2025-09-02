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
           barrier_allowable_rev_s2 = barrier_allowable_rev-1,
           barrier_allowable_rev_s = c(scale(barrier_allowable_rev, 
                                                 scale = T)),
           barrier_nonallowable_rev_s2 = barrier_nonallowable_rev-1,
           barrier_nonallowable_rev_s = c(scale(barrier_nonallowable_rev, 
                                                 scale = T)),
           barrier_allowable_s = barrier_allowable-1,
           barrier_nonallowable_s = barrier_nonallowable-1,
           barrier_availability_s = c(scale((5-barrier_availability), scale = T)),
           barrier_insuffQuantity_s  = c(scale((5-barrier_insuffQuantity), scale = T)),
           barrier_lackdesiredTraits_s  = c(scale((5-barrier_lackdesiredTraits), scale = T)),
           barrier_seedTreatlack_s  = c(scale((5-barrier_seedTreatlack), scale = T)),
           barrier_price_s  = c(scale((5-barrier_price), scale = T)),
           barrier_buyerReq_s  = c(scale((5-barrier_buyerReq), scale = T)),
           years_certified_s = c(scale(YearsCertified, scale = T)),
           years_certified_log = log((YearsCertified+1))
    )

df.full_rounded <- df.full

s = 0.5
n = nrow(df.full)
x = df.full_rounded$OGseed_percent_maincrop_prop
rounded = (x*(n-1)+s)/n
summary(rounded)

df.full_rounded$OGseed_percent_maincrop_prop <- (df.full_rounded$OGseed_percent_maincrop_prop * (n-1) + s)/n


cor(df.full$barrier_allowable_rev,
    df.full$barrier_nonallowable_rev)


library(betareg)
set.seed(19)
m1 <- betareg(OGseed_percent_maincrop_prop ~ 
               barrier_allowable_rev_s +
               #barrier_nonallowable_rev_s +
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
summary(m1)

m2 <- betareg(OGseed_percent_maincrop_prop ~ 
                #barrier_allowable_rev_s +
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
summary(m2)

m3 <- betareg(OGseed_percent_maincrop_prop ~ 
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
summary(m3)

m4 <- betareg(OGseed_percent_maincrop_prop ~ 
                barrier_allowable_rev_s +
                barrier_nonallowable_rev_s +
                barrier_allowable_rev_s*barrier_nonallowable_rev_s +
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
summary(m4)

m5 <- betareg(OGseed_percent_maincrop_prop ~ 
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
summary(m5)

m6 <- betareg(OGseed_percent_maincrop_prop ~ 
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
summary(m6)

m7 <- betareg(OGseed_percent_maincrop_prop ~ 
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
summary(m7)

saveRDS(list(m3, m5, m6, m7), 'tables/append_models_barriers.rds')

m8 <- betareg(OGseed_percent_maincrop_prop ~ 
                barrier_availability_s +
                barrier_lackdesiredTraits_s +
                barrier_insuffQuantity_s +
                barrier_seedTreatlack_s +
                barrier_price_s +
                barrier_buyerReq_s +
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
summary(m8)


saveRDS(m8, 'tables/append_models_barriers_disaggregated.rds')

m9 <- betareg(OGseed_percent_maincrop_prop ~ 
                barrier_availability_s +
                barrier_lackdesiredTraits_s +
                barrier_insuffQuantity_s +
                barrier_seedTreatlack_s +
                barrier_price_s +
                barrier_buyerReq_s +
                OG_value_s + 
                barrier_availability_s*OG_value_s +
                CertifierRequestn + 
                acreage_log_s + 
                cropcat_n_s +
                noncommercial_seedsource_s +
                years_certified_s + 
                maincrop +
                Region +
                survey_year, 
              data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m9)

m10 <- betareg(OGseed_percent_maincrop_prop ~ 
                barrier_availability_s +
                barrier_lackdesiredTraits_s +
                barrier_insuffQuantity_s +
                barrier_seedTreatlack_s +
                barrier_price_s +
                barrier_buyerReq_s +
                OG_value_s + 
                 barrier_lackdesiredTraits_s*OG_value_s +
                CertifierRequestn + 
                acreage_log_s + 
                cropcat_n_s +
                noncommercial_seedsource_s +
                years_certified_s + 
                maincrop +
                Region +
                survey_year, 
              data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m10)

m11 <- betareg(OGseed_percent_maincrop_prop ~ 
                 barrier_availability_s +
                 barrier_lackdesiredTraits_s +
                 barrier_insuffQuantity_s +
                 barrier_seedTreatlack_s +
                 barrier_price_s +
                 barrier_buyerReq_s +
                 OG_value_s + 
                 barrier_insuffQuantity_s*OG_value_s +
                 CertifierRequestn + 
                 acreage_log_s + 
                 cropcat_n_s +
                 noncommercial_seedsource_s +
                 years_certified_s + 
                 maincrop +
                 Region +
                 survey_year, 
               data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m11)

m12 <- betareg(OGseed_percent_maincrop_prop ~ 
                 barrier_availability_s +
                 barrier_lackdesiredTraits_s +
                 barrier_insuffQuantity_s +
                 barrier_seedTreatlack_s +
                 barrier_price_s +
                 barrier_buyerReq_s +
                 OG_value_s + 
                 barrier_seedTreatlack_s*OG_value_s +
                 CertifierRequestn + 
                 acreage_log_s + 
                 cropcat_n_s +
                 noncommercial_seedsource_s +
                 years_certified_s + 
                 maincrop +
                 Region +
                 survey_year, 
               data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m12)

m13 <- betareg(OGseed_percent_maincrop_prop ~ 
                 barrier_availability_s +
                 barrier_lackdesiredTraits_s +
                 barrier_insuffQuantity_s +
                 barrier_seedTreatlack_s +
                 barrier_price_s +
                 barrier_buyerReq_s +
                 OG_value_s + 
                 barrier_price_s*OG_value_s +
                 CertifierRequestn + 
                 acreage_log_s + 
                 cropcat_n_s +
                 noncommercial_seedsource_s +
                 years_certified_s + 
                 maincrop +
                 Region +
                 survey_year, 
               data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m13) # Only this one...

m14 <- betareg(OGseed_percent_maincrop_prop ~ 
                 barrier_availability_s +
                 barrier_lackdesiredTraits_s +
                 barrier_insuffQuantity_s +
                 barrier_seedTreatlack_s +
                 barrier_price_s +
                 barrier_buyerReq_s +
                 OG_value_s + 
                 barrier_buyerReq_s*OG_value_s +
                 CertifierRequestn + 
                 acreage_log_s + 
                 cropcat_n_s +
                 noncommercial_seedsource_s +
                 years_certified_s + 
                 maincrop +
                 Region +
                 survey_year, 
               data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m14)

