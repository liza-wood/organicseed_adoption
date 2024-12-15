library(dplyr)
df <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds")
maxna <- function(...){max(..., na.rm = T)}
scale_data <- function(dat, dv, yr = c('2011', '2016', '2021')){
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
           YearsCertified
    ) %>% 
   # mutate(barrier_allowable = purrr::pmap_dbl(list(barrier_availability,
   #                                                 barrier_insuffQuantity,
   #                                                 barrier_lackdesiredTraits,
   #                                                 barrier_seedTreatlack), max),
   #        barrier_allowable_rev = ifelse(barrier_allowable == 1, 4,
   #                                ifelse(barrier_allowable == 2, 3,
   #                                ifelse(barrier_allowable == 3, 2,
   #                                ifelse(barrier_allowable == 4, 1, NA))))) %>% 
   # mutate(barrier_allowable_sum = purrr::pmap_dbl(list(barrier_availability,
   #                                                 barrier_insuffQuantity,
   #                                                 barrier_lackdesiredTraits,
   #                                                 barrier_seedTreatlack), sum)) %>% 
   # mutate(barrier_nonallowable = purrr::pmap_dbl(list(barrier_price,
   #                                                    barrier_buyerReq), max), 
   #        barrier_nonallowable_rev = ifelse(barrier_nonallowable == 1, 4,
   #                                   ifelse(barrier_nonallowable == 2, 3,
   #                                   ifelse(barrier_nonallowable == 3, 2,
   #                                   ifelse(barrier_nonallowable == 4, 1, NA))))) %>% 
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
    #mutate(barrier_all_sum = purrr::pmap_dbl(list(barrier_availability,
    #                                               barrier_insuffQuantity,
    #                                               barrier_lackdesiredTraits,
    #                                               barrier_seedTreatlack,
    #                                               barrier_price,
    #                                               barrier_buyerReq), sum),
    #       barrier_all_sum = barrier_all_sum-5,
    #       barrier_sum_rev = ifelse(barrier_all_sum == 0, 19,
    #                         ifelse(barrier_all_sum == 1, 18,
    #                         ifelse(barrier_all_sum == 2, 17,
    #                         ifelse(barrier_all_sum == 3, 16,
    #                         ifelse(barrier_all_sum == 4, 15,
    #                         ifelse(barrier_all_sum == 5, 14,
    #                         ifelse(barrier_all_sum == 6, 13,
    #                         ifelse(barrier_all_sum == 7, 12,
    #                         ifelse(barrier_all_sum == 8, 11,
    #                         ifelse(barrier_all_sum == 9, 10,
    #                         ifelse(barrier_all_sum == 10, 9,
    #                         ifelse(barrier_all_sum == 11, 8,
    #                         ifelse(barrier_all_sum == 12, 7,
    #                         ifelse(barrier_all_sum == 13, 6,
    #                         ifelse(barrier_all_sum == 14, 5,
    #                         ifelse(barrier_all_sum == 15, 4,
    #                         ifelse(barrier_all_sum == 16, 3,
    #                         ifelse(barrier_all_sum == 17, 2,
    #                         ifelse(barrier_all_sum == 18, 1, NA))))))))))))))))))))  %>% 
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
           #acreage_s = c(scale(TotalAcreage, scale = T)), 
           cropcat_n_s = c(scale(cropcat_n, scale = T)),
           #cropcat_n_s = cropcat_n - 1,
           #CertifierRequestn_s = c(scale(CertifierRequestn, scale = T)),
           OG_value_s = c(scale(OG_value, scale = T)),
           OG_value_s2 = OG_value - 1,
           #barrier_availability_s = c(scale(barrier_availability, scale = F)),
           #barrier_lackdesiredTraits_s = c(scale(barrier_lackdesiredTraits, scale = F)),
           #barrier_insuffQuantity_s = c(scale(barrier_insuffQuantity, scale = F)),
           noncommercial_seedsource_s = c(scale(noncommercial_seedsource, 
                                                scale = T)),
           #barrier_seedTreatlack_s = c(scale(barrier_seedTreatlack, scale = F)),
           #barrier_allowable_s = c(scale(barrier_allowable, scale = F)),
           #barrier_allowable_rev_s = barrier_allowable_rev-1,
           #barrier_allowable_s = barrier_allowable-1,
           #barrier_allowable_sum_s = barrier_allowable_sum-4,
           #barrier_nonallowable_rev_s = barrier_nonallowable_rev-1,
           #barrier_nonallowable_s = barrier_nonallowable-1,
           barrier_all_rev_s2 = barrier_all_rev-1,
           barrier_all_rev_s = c(scale(barrier_all_rev, 
                   scale = T)),
           #barrier_sum_rev_s = c(scale(barrier_sum_rev, 
           #                            scale = T)),
           #barrier_all_s = barrier_all-1,
           #barrier_all_s = c(scale(barrier_all, 
          #                             scale = T)),
           #barrier_buyerReq_s = c(scale(barrier_buyerReq, scale = F)),
           #barrier_saveOwn_s  = c(scale(barrier_saveOwn, scale = T)),
           #barrier_price_s = c(scale(barrier_price, scale = F))
           years_certified_s = c(scale(YearsCertified, scale = T)),
           years_certified_log = log((YearsCertified+1))
    )
}


# Scale data ----
# Considering acreage as the dependent variable 
## Use c() when scaling numbers because it "strips" the dimension (i.e. scaling converts them into some kind of matrix and Stack Overflow told me that c would strip this)

dfs <- scale_data(df, OGseed_percent_maincrop)
saveRDS(dfs, "~/Box/organicseed_adoption/df_scaled.rds")
saveRDS(dfs, "~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled.rds")

dfs10 <- scale_data(df, OGseed_percent_maincrop, 2011)

saveRDS(dfs10, "~/Box/organicseed_adoption/df_scaled10.rds")
saveRDS(dfs10, "~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled10.rds")

dfs15 <- scale_data(df, OGseed_percent_maincrop, 2016)

saveRDS(dfs15, "~/Box/organicseed_adoption/df_scaled15.rds")
saveRDS(dfs15, "~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled15.rds")

dfs20 <- scale_data(df, OGseed_percent_maincrop, 2021)

saveRDS(dfs20, "~/Box/organicseed_adoption/df_scaled20.rds")
saveRDS(dfs20, "~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled20.rds")

dfs <- scale_data(df, IncreasedOGSeed)

saveRDS(dfs, "~/Box/organicseed_adoption/df_scaled_increase.rds")
saveRDS(dfs, "~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled_increase.rds")

dfs10 <- scale_data(df, IncreasedOGSeed, 2011)

saveRDS(dfs10, "~/Box/organicseed_adoption/df_scaled_increase10.rds")
saveRDS(dfs10, "~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled_increase10.rds")

dfs15 <- scale_data(df, IncreasedOGSeed, 2016)

saveRDS(dfs15, "~/Box/organicseed_adoption/df_scaled_increase15.rds")
saveRDS(dfs15, "~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled_increase15.rds")

dfs20 <- scale_data(df, IncreasedOGSeed, 2021)

saveRDS(dfs20, "~/Box/organicseed_adoption/df_scaled_increase20.rds")
saveRDS(dfs20, "~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled_increase20.rds")

