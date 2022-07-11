library(tidyverse)

df <- read.csv("~/Box/organicseed/df.csv")
df.long <- read.csv("~/Box/organicseed/df_long.csv")

df %>% 
  filter(Region != "International") %>% 
  filter(survey_year == 2021) %>% 
  group_by(maincrop) %>% 
  summarize(avg_avail = mean(barrier_availability, na.rm = T),
            avg_price = mean(barrier_price, na.rm = T),
            avg_quality = mean(barrier_distrustQuality, na.rm = T),
            avg_buyerReq = mean(barrier_buyerReq, na.rm = T))
df$barrier_buyerReq
corn <- df %>% 
  filter(Region != "International") %>% 
  filter(survey_year == 2021) %>% 
  filter(growsField == T) %>% 
  filter(str_detect(FieldCrop1, "[Cc]orn|[Mm]aize") | 
           str_detect(FieldCrop2, "[Cc]orn|[Mm]aize") |
           str_detect(FieldCrop3, "[Cc]orn|[Mm]aize")) 
corn %>% 
  summarize(avg_avail = mean(barrier_availability, na.rm = T),
            avg_price = mean(barrier_price, na.rm = T),
            avg_quality = mean(barrier_distrustQuality, na.rm = T),
            avg_buyerReq = mean(barrier_buyerReq, na.rm = T))

soy <- df %>% 
  filter(Region != "International") %>% 
  filter(survey_year == 2021) %>% 
  filter(growsField == T) %>% 
  filter(str_detect(FieldCrop1, "[Ss]oy") | 
           str_detect(FieldCrop2, "[Ss]oy") |
           str_detect(FieldCrop3, "[Ss]oy")) 
summary(corn$OGFieldSeed)
summary(soy$OGFieldSeed)

summary(corn$barrier_availability)
summary(corn$barrier_price)
summary(soy$barrier_availability)

summary(df$barrier_availability)
summary(df$barrier_price)

df <- df %>% 
  filter(Region != "International") %>% 
  select(OGseed_percent_maincrop, maincrop, VegiAcreage, FieldAcreage, ForageAcreage, 
         OGVegiSeed, OGFieldSeed, OGForageSeed, TotalAcreage, 
         OGSeedOGIntegrityf, OGSeedOGIntegrityn, CertifierRequestn, 
         funding_avg_annual_amt, producer_n, survey_year, year_range, Region,
         contains("barrier"), producer_challenge_sum, producer_n, median_producer_revenue) 

# Regional profiles
regional_count <- df %>% 
  filter(survey_year == 2021) %>% 
  group_by(Region) %>% 
  count()

reg_summary <- df %>% 
  filter(survey_year == 2021) %>% 
  group_by(Region) %>% 
  summarize(org_seed_mean = mean(OGseed_percent_maincrop, na.rm = T),
            org_seed_median = median(OGseed_percent_maincrop, na.rm = T),
            barrier_availability_mean = mean(barrier_availability, na.rm = T),
            barrier_availability_median = median(barrier_availability, na.rm = T),
            barrier_buyerReq_mean = mean(barrier_buyerReq, na.rm = T),
            barrier_buyerReq_median = median(barrier_buyerReq, na.rm = T),
            producer_n = producer_n,
            producer_challenge_sum = producer_challenge_sum,
            median_producer_revenue = median_producer_revenue) %>% 
  left_join(regional_count) %>% 
  unique()

df %>% 
  filter(survey_year == 2021) %>% 
  group_by(Region, maincrop) %>% 
  summarize(mean = mean(OGseed_percent_maincrop, na.rm = T),
            median = median(OGseed_percent_maincrop, na.rm = T))

df.long %>% 
  filter(survey_year == 2021) %>% 
  group_by(Region, OGseed_type) %>% 
  summarize(mean = mean(OGseed_percent, na.rm = T),
            median = median(OGseed_percent, na.rm = T))
write.csv(reg_summary, "~/Box/organicseed/regional_summary.csv", row.names = F)

org <- read.csv("~/Box/organicseed/NOP_USorganicCrops_6.28.22.csv") %>% slice(-c(1:2))

western <- c("CA", "OR", "WA", "ID", "NV", "AZ", "UT", "NM", "CO", "WY", "MT", 
                     "AK", "HI","California", "Oregon", "Washington", "Idaho", "Nevada",
                     "Arizona", "Utah", "New Mexico", "Colorado", "Wyoming", "Montana",
                     "Alaska", "Hawaii")
northcentral <- c("ND", "SD", "KS", "NE", "MN", "IA", "MO", "WI", "IL", "MI", 
                          "IN", "OH", "North Dakota", "South Dakota", "Kansas",
                          "Nebraska", "Minnesota", "Iowa", "Missouri", "Wisconsin", 
                          "Illinois", "Michigan", "Indiana", "Ohio")
southern <- c("TX", "OK", "AR", "LA", "KY", "TN", "MS", "AL", "FL", "GA", "SC",
                      "NC", "VA", "Texas", "Oklahoma", "Arkansas", "Louisiana", 
                      "Kentucky", "Tennessee", "Mississippi", "Alabama", "Florida", 
                      "Georgia", "South Carolina", "North Carolina", "Virginia")
northeast <- c("WV", "MD", "DE", "NJ", "PA", "NY", "CT", "RI", "MA", "VT", 
                       "NH", "ME", "West Virginia", "Maryland", "Delaware", 
                       "New Jersey", "Pennsylvania", "New York", "Connecticut", 
                       "Rhode Island", "Massachusetts", "Vermont", "New Hampshire", "Maine")

df.full <- org %>% 
  mutate(Region = case_when(
    Physical.Address..State.Province %in% western  ~ "West",
    Physical.Address..State.Province %in% northcentral ~ "North Central",
    Physical.Address..State.Province %in% southern ~ "South",
    Physical.Address..State.Province %in% northeast ~ "Northeast",
    T ~ NA_character_))

table(df.full$Region)
