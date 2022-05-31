# Variable creation for funding and seed producers
## Start with the merged producer data
df <- read.csv("~/Box/organicseed/merged_df_3panel.csv")


df$VegiAcreage <- ifelse(df$VegiAcreage < 0, NA, df$VegiAcreage)

df$IncreasedOGVegSeedf <- as.numeric(factor(df$IncreasedOGVegSeed, levels = c("Decreased the %",
                                         "About the same %",
                                         "Increased the %",
                                         "Already at 100%")))
df$IncreasedOGFieldSeedf <- as.numeric(factor(df$IncreasedOGFieldSeed, levels = c("Decreased the %",
                                                                              "About the same %",
                                                                              "Increased the %",
                                                                              "Already at 100%")))
df$IncreasedOGForageSeedf <- as.numeric(factor(df$IncreasedOGForageSeed, levels = c("Decreased the %",
                                                                              "About the same %",
                                                                              "Increased the %",
                                                                              "Already at 100%")))
df$IncreasedOGseed_avg <- round(rowMeans(df[, (ncol(df)-2):ncol(df)], na.rm = T), 0)
df$IncreasedOGseed_avg <- ifelse(df$IncreasedOGseed_avg == "NaN", NA, df$IncreasedOGseed_avg)

# One way to think about main crop is to assign each producer to a single crop, or mix
df <- df %>% 
  mutate(maincrop = case_when(
    df$VegiAcreage == 0 & df$FieldAcreage == 0 & df$ForageAcreage ~ "Unknown",
    df$VegiAcreage/df$TotalAcreage > .33 ~ "Vegetables",
    df$FieldAcreage/df$TotalAcreage > .33 ~ "Field",
    df$ForageAcreage/df$TotalAcreage > .33 ~ "Forage",
    T ~ "Mix"
  )) %>% 
  mutate(OGseed_percent_maincrop = case_when(
    maincrop == "Vegetables" ~ OGVegiSeed,
    maincrop == "Field" ~ OGFieldSeed,
    maincrop == "Forage" ~ OGForageSeed,
    T ~ (OGVegiSeed + OGFieldSeed + OGForageSeed)/3, 
  )) %>% 
  mutate(IncreasedOGseed = case_when(
    survey_year != 2011 & maincrop == "Vegetables" ~ IncreasedOGVegSeed,
    survey_year != 2011 & maincrop == "Field" ~ IncreasedOGFieldSeed,
    survey_year != 2011 & maincrop == "Forage" ~ IncreasedOGForageSeed,
    survey_year != 2011 & maincrop == "Mix" ~ as.character(IncreasedOGseed_avg),
    survey_year == 2011 ~ IncreasedOGSeed,
    T ~ IncreasedOGSeed
  )) %>% 
  mutate(IncreasedOGseed = case_when(
    IncreasedOGseed %in% c("4", "Have been using 100% certified organic seed for at least three years") ~ "Already at 100%",
    IncreasedOGseed %in% c("3", "Increased the %") ~ "Yes",
    IncreasedOGseed %in% c("About the same %", "Decreased the %", "1", "2") ~ "No",
    IncreasedOGseed == "" ~ NA_character_,
    T ~ IncreasedOGseed
  ))
table(df$maincrop)
table(df$IncreasedOGseed)


## Reading in public funding data so that I can merge it with the producer survey
setwd("~/Dropbox/SOSInteractiveSurvey/")
funds <- read.csv("local_data/public-funding-Dec21.csv")
table(funds$proj_crop)
table(funds$location)

fund_summary <- funds %>% 
  mutate(year_range = case_when(
    year %in% 2006:2010 ~ "2006-2010",
    year %in% 2011:2015 ~ "2011-2015",
    year %in% 2016:2020 ~ "2016-2020",
    T ~ NA_character_
  )) %>% 
  mutate(Region = case_when(
    location == "Southern" ~ "South",
    location == "Western" ~ "West",
    location == "North Central" ~ "North Central",
    location == "Northeast" ~ "Northeast",
    T ~ "Multi-region"
  )) %>% 
  #mutate(maincrop = case_when(
  #  proj_crop == "Field Crops" ~ "Field",
  #  proj_crop == "Forage and / or Cover Crops" ~ "Forage",
  #  proj_crop == "Vegetables" ~ "Vegetables",
  #  T ~ "Multiple"
  #)) %>% 
  filter(!is.na(year_range)) %>% 
  group_by(year_range, Region) %>% 
  summarize(funding_avg_annual_amt = sum(funding_amt)/5) 

matched_funds <- fund_summary %>% 
  filter(Region != "Multi-region") #& maincrop != "Multiple")
# unsure what to do with unmatched funds yet
unmatched_funds <- fund_summary %>% 
  filter(Region == "Multi-region") #| maincrop == "Multiple")


df <- df %>% 
  mutate(year_range = case_when(
    survey_year == 2011 ~ "2006-2010",
    survey_year == 2016 ~ "2011-2015",
    survey_year == 2021 ~ "2016-2020",
    T ~ NA_character_
  ))

df <- left_join(df, matched_funds)

## Reading in seed producer data so that I can merge it with the producer survey
producers <- read.csv("~/Box/osa_networks/data_combined_3/surveys_combined.csv") %>% 
  filter(act_seed_producer == T)

ppltn <- read.csv("~/Box/osa_networks/data_producers/00_populatioN_data/total_populatioN.csv") 

countdf <- ppltn %>% 
  mutate(Region = case_when(
    Region == "Canada" ~ "International",
    T ~ Region
  )) %>% 
  filter(Region != "Other") %>% 
  group_by(Region) %>% 
  count()

producers <- producers %>% 
  select(income20, income20_c, contains("pchall_f_"), Region) %>% 
  mutate(Region = case_when(
    Region == "Canada" ~ "International",
    T ~ Region
  )) %>% 
  filter(Region != "Other") %>% 
  mutate(income20_f = case_when(
      income20 == "Under $50,000" ~ "$25,000-$49,999",
      income20 == "$50,000-$149,999" ~ "$75,000-$99,999",
      income20 == "Over $300,000" ~ "$300,000-$1 million",
      income20 == "$300,000-$499,999" ~ "$300,000-$1 million",
      income20 == "$500,000-$1 million" ~ "$300,000-$1 million",
      T ~ income20
  )) %>% 
  mutate(income20_f = factor(income20_f, levels = c("Under $25,000",
                                                    "$25,000-$49,999",
                                                    "$50,000-$74,999",
                                                    "$75,000-$99,999",
                                                    "$100,000-$149,999",
                                                    "$150,000-$299,999",
                                                    "$300,000-$1 million",
                                                    "Over $1 million"))) %>% 
  mutate(income20_f = as.numeric(income20_f)) %>% 
  group_by(Region) %>% 
  summarize(income20_median = median(income20_f, na.rm =T),
            pchall_f_agron_avg = mean(pchall_f_agron, na.rm = T),
            pchall_f_clean_equip_avg = mean(pchall_f_clean_equip, na.rm = T),
            pchall_f_pest_disease_avg = mean(pchall_f_pest_disease, na.rm = T),
            pchall_f_climate_avg = mean(pchall_f_climate, na.rm = T),
            pchall_f_cost_avg = mean(pchall_f_cost, na.rm = T),
            npchall_f_business_avg = mean(npchall_f_business, na.rm = T),
            npchall_f_cert_avg = mean(npchall_f_cert, na.rm = T),
            npchall_f_rltn.ipr_avg = mean(npchall_f_rltn.ipr, na.rm = T),
            npchall_f_market_avg = mean(npchall_f_market, na.rm = T)) %>% 
  rowwise() %>% 
  mutate(pchall_sum = sum(c_across(pchall_f_agron_avg:pchall_f_cost_avg)),
         npchall_sum = sum(c_across(npchall_f_business_avg:npchall_f_market_avg)),
         chall_sum = sum(c_across(pchall_sum:npchall_sum))) %>% 
  left_join(countdf) %>% 
  select(Region, n, income20_median, chall_sum) %>% 
  rename(producer_n = n,
         median_producer_revenue = income20_median,
         producer_challenge_sum = chall_sum)

df <- left_join(df, producers)

## Creating a few new variables
df$OGSeedOGIntegrityf <- factor(df$OGSeedOGIntegrity, levels = c("Strongly disagree",
                                                                 "Disagree",
                                                                 "Neutral",
                                                                 "Agree",
                                                                 "Strongly agree"))
df$OGSeedOGIntegrityn <- as.numeric(df$OGSeedOGIntegrityf)

df$CertifierRequestn <- ifelse(df$CertifierRequest == "No", 0,
                               ifelse(df$CertifierRequest == "Yes", 1, NA))

## Lack of seed treatments, such as pelleting or priming
## Processor (buyer) requires or supplies varieties that are not available organically
## Insufficient quantity of seed
## Save my own seed
## Distrust of organic seed quality
## Price
## Specific variety not available as organic seed
## Lack of desirable genetic traits
rmv <- c("No answer", "partial complete", "skipped due to branching")
levels_b <- c("Not a factor", "Slight factor", "Moderate factor", "Significant factor")

df <- df %>% 
  mutate(barrier_seedTreatlack = ifelse(Q29A %in% rmv, NA, factor(Q29A, levels = levels_b)),
         barrier_buyerReq = ifelse(Q29B %in% rmv, NA, factor(Q29B, levels = levels_b)),
         barrier_insuffQuantity = ifelse(Q29C %in% rmv, NA, factor(Q29C, levels = levels_b)),
         barrier_saveOwn = ifelse(Q29D %in% rmv, NA, factor(Q29D, levels = levels_b)),
         barrier_distrustQuality = ifelse(Q29E %in% rmv, NA, factor(Q29E, levels = levels_b)),
         barrier_price = ifelse(Q29F %in% rmv, NA, factor(Q29F, levels = levels_b)),
         barrier_availability = ifelse(Q29G %in% rmv, NA, factor(Q29G, levels = levels_b)),
         barrier_lackdesiredTraits = ifelse(Q29H %in% rmv, NA, factor(Q29H, levels = levels_b)))

table(df$Q29A)
table(df$barrier_seedTreatlack)
df <- select(df, -c(Q29A:Q29H))


df$growsVeg <- ifelse(is.na(df$VegiAcreage), 0, ifelse(df$VegiAcreage > 0, 1, 0))
growsveg <- df$RespondentID[df$growsVeg == 1]
df$growsField <- ifelse(is.na(df$FieldAcreage), 0, ifelse(df$FieldAcreage > 0, 1, 0))
growsfield <- df$RespondentID[df$growsField == 1]
df$growsForage <- ifelse(is.na(df$ForageAcreage), 0, ifelse(df$ForageAcreage > 0, 1, 0))
growsforage <- df$RespondentID[df$growsForage == 1]


# A second way is to lengthen the data so that each crop gets an observation, making repeats for crop type
df.long <- pivot_longer(df, cols = c(OGVegiSeed, OGFieldSeed, OGForageSeed),
                        names_to = "OGseed_type", values_to = "OGseed_percent")
df.long <- df.long[df.long$RespondentID %in% growsveg & df.long$OGseed_type == "OGVegiSeed" | 
                   df.long$RespondentID %in% growsfield & df.long$OGseed_type == "OGFieldSeed" | 
                   df.long$RespondentID %in% growsforage & df.long$OGseed_type == "OGForageSeed", ]

df$OGVegiSeed <- ifelse(is.na(df$OGVegiSeed), 0 , df$OGVegiSeed)
df$OGFieldSeed <- ifelse(is.na(df$OGFieldSeed), 0 , df$OGFieldSeed)
df$OGForageSeed <- ifelse(is.na(df$OGForageSeed), 0 , df$OGForageSeed)

write.csv(df, "~/Box/organicseed/df.csv", row.names = F)
write.csv(df.long, "~/Box/organicseed/df_long.csv", row.names = F)
