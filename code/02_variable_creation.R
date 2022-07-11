# Variable creation for funding and seed producers
## Start with the merged producer data
library(tidyverse)
df <- read.csv("~/Box/organicseed/merged_df_3panel.csv")

df$VegiAcreage <- ifelse(df$VegiAcreage < 0, NA, df$VegiAcreage)
df$IncreasedOGVegSeedf <- as.numeric(factor(df$IncreasedOGVegSeed, 
                                            levels = c("Decreased the %",
                                                       "About the same %",
                                                       "Increased the %",
                                                       "Already at 100%")))
df$IncreasedOGFieldSeedf <- as.numeric(factor(df$IncreasedOGFieldSeed, 
                                              levels = c("Decreased the %",
                                                         "About the same %",
                                                         "Increased the %",
                                                         "Already at 100%")))
df$IncreasedOGForageSeedf <- as.numeric(factor(df$IncreasedOGForageSeed, 
                                               levels = c("Decreased the %",
                                                          "About the same %",
                                                          "Increased the %",
                                                          "Already at 100%")))
df$IncreasedOGseed_avg <- round(rowMeans(df[, (ncol(df)-2):ncol(df)], na.rm = T), 0)
df$IncreasedOGseed_avg <- ifelse(df$IncreasedOGseed_avg == "NaN", NA, df$IncreasedOGseed_avg)

# One way to think about main crop is to assign each producer to a single crop, or mix
# Last year how many organic acres did you have in annual vegetables. Please fill in number
df$VegiAcreage <- ifelse(is.na(df$VegiAcreage), 0 , df$VegiAcreage)
df$FieldAcreage <- ifelse(is.na(df$FieldAcreage), 0 , df$FieldAcreage)
df$ForageAcreage <- ifelse(is.na(df$ForageAcreage), 0 , df$ForageAcreage)

# Are these crops really unknown?
test <- filter(df, VegiAcreage == 0 & FieldAcreage == 0 & ForageAcreage == 0)
table(test$GrowField)
table(test$GrowForage)
table(test$GrowVegis)
veg <- test[test$GrowVegis == 0 & !is.na(test$GrowVegis) & test$GrowForage == 100,]

df <- df %>% 
  mutate(maincrop = case_when(
    # Making the distinction below because 2011 did not have cover crops
    survey_year %in% c(2016, 2021) & VegiAcreage/TotalAcreage > .26 ~ "Vegetables",
    survey_year %in% c(2016, 2021) & FieldAcreage/TotalAcreage > .26 ~ "Field",
    survey_year %in% c(2016, 2021) & ForageAcreage/TotalAcreage > .26 ~ "Forage",
    survey_year == 2011 & VegiAcreage/TotalAcreage > .34 ~ "Vegetables",
    survey_year == 2011 & FieldAcreage/TotalAcreage > .34 ~ "Field",
    survey_year == 2011 & ForageAcreage/TotalAcreage > .34 ~ "Forage",
    # There are some that have no acreage listed but say they grow this thing, so I am choosing in a random order since I cannot discern main; I think these get dropped anyways though because I cannot get their acreage?
    GrowForage == 100 ~ "Forage",
    GrowField == 100 ~ "Field",
    GrowVegis == 100 ~ "Vegetables", 
    VegiAcreage == 0 & FieldAcreage == 0 & ForageAcreage == 0 |
      GrowVegis == 0 & GrowField == 0 & GrowForage == 0 ~ "Unknown",
    T ~ "Mix"
  )) %>% 
  mutate(OGseed_percent_maincrop = case_when(
    maincrop == "Vegetables" ~ OGVegiSeed,
    maincrop == "Field" ~ OGFieldSeed,
    maincrop == "Forage" ~ OGForageSeed,
    T ~ (OGVegiSeed + OGFieldSeed + OGForageSeed)/3, 
  )) %>% 
  mutate(IncreasedOGSeed = case_when(
    survey_year != 2011 & maincrop == "Vegetables" ~ IncreasedOGVegSeed,
    survey_year != 2011 & maincrop == "Field" ~ IncreasedOGFieldSeed,
    survey_year != 2011 & maincrop == "Forage" ~ IncreasedOGForageSeed,
    survey_year != 2011 & maincrop == "Mix" ~ as.character(IncreasedOGseed_avg),
    survey_year == 2011 ~ IncreasedOGSeed,
    T ~ IncreasedOGSeed
  )) %>% 
  mutate(IncreasedOGSeed = case_when(
    IncreasedOGSeed %in% c("4", "Have been using 100% certified organic seed for at least three years") ~ "Already at 100%",
    IncreasedOGSeed %in% c("3", "Increased the %") ~ "Yes",
    IncreasedOGSeed %in% c("About the same %", "Decreased the %", "1", "2") ~ "No",
    IncreasedOGSeed == "" ~ NA_character_,
    T ~ IncreasedOGSeed
  ))



df$IncreasedOGSeed <- ifelse(df$OGseed_percent_maincrop == 100 & 
                               is.na(df$IncreasedOGSeed), "Already at 100%", 
                             df$IncreasedOGSeed)

table(df$maincrop)
table(df$maincrop, df$survey_year)

filter(df, survey_year == 2021) %>% 
  select(ForageAcreage, FieldAcreage, VegiAcreage, TotalAcreage, maincrop) %>% 
  filter(ForageAcreage != 0)

table(df$IncreasedOGseed)


## Reading in public funding data so that I can merge it with the producer survey
setwd("~/Dropbox/SOSInteractiveSurvey/")
funds <- read.csv("local_data/public-funding-Dec21.csv")
table(funds$proj_crop)
table(funds$location)

funds <- funds %>% 
  mutate(year_range = case_when(
    year %in% 2004:2008 ~ "2004-2008",
    year %in% 2009:2013 ~ "2009-2013",
    year %in% 2014:2018 ~ "2014-2018",
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
  filter(!is.na(year_range))

funds %>% 
  group_by(year_range) %>% 
  summarize(funding_avg_annual_amt = sum(funding_amt)/5)
  
fund_summary <- funds %>% 
  group_by(year_range, Region) %>% 
  summarize(funding_avg_annual_amt = sum(funding_amt)/5) 

matched_funds <- fund_summary %>% 
  filter(Region != "Multi-region") #& maincrop != "Multiple")
# unsure what to do with unmatched funds yet
unmatched_funds <- fund_summary %>% 
  filter(Region == "Multi-region") #| maincrop == "Multiple")

df <- df %>% 
  mutate(year_range = case_when(
    survey_year == 2011 ~ "2004-2008",
    survey_year == 2016 ~ "2009-2013",
    survey_year == 2021 ~ "2014-2018",
    T ~ NA_character_
  ))

df <- left_join(df, matched_funds)

## Reading in seed producer data so that I can merge it with the producer survey
nodes <- fread("~/Box/osa_networks/data_combined_3/net_nodes_withdf.csv") %>% 
  select(ResponseId_dup, variety_n, cropcat_n)
producers <- read.csv("~/Box/osa_networks/data_combined_3/surveys_combined.csv") %>% 
  filter(act_seed_producer == T) %>% left_join(nodes, by = c("ResponseId" = "ResponseId_dup"))
retailers <- read.csv("~/Box/osa_networks/data_combined_3/surveys_combined.csv") %>% 
  filter(act_seed_retail == T)

ppltn.p <- read.csv("~/Box/osa_networks/data_producers/00_populatioN_data/total_populatioN.csv") 

countdf <- ppltn.p %>% 
  mutate(Region = case_when(
    Region == "Canada" ~ "International",
    T ~ Region
  )) %>% 
  filter(Region != "Other") %>% 
  group_by(Region) %>% 
  count()

producers <- producers %>% 
  select(income20, income20_c, contains("pchall_f_"), Region, variety_n, cropcat_n) %>% 
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
  mutate(income20_f = as.numeric(income20_f)) 

producers_summ <- producers %>% 
  group_by(Region) %>% 
  summarize(crop_n = round(mean(cropcat_n, na.rm = T), 2),
            variety_n = round(mean(variety_n, na.rm = T), 2),
            income20_median = median(income20_f, na.rm =T),
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
         chall_sum = sum(c_across(pchall_sum:npchall_sum))/9) %>% 
  left_join(countdf) %>% 
  select(Region, n, income20_median, chall_sum, crop_n, variety_n) %>% 
  rename(producer_n = n,
         median_producer_revenue = income20_median,
         producer_challenge_sum = chall_sum)

producers %>% 
  filter(Region != "International" & Region != "") %>% 
  summarize(mean_income = mean(income20_f, na.rm =T),
            total = nrow(.),
            chall_sum = sum(c_across(pchall_f_agron:npchall_f_market)),
            mean_chall = (chall_sum/9)/nrow(.),
            mean_varieties = mean(variety_n, na.rm = T),
            mean_crops = mean(cropcat_n, na.rm = T))

ppltn.c <- read.csv("~/Box/osa_networks/data_companies/00_populatioN_data/datawrapper_representativeness.csv") %>% 
  group_by(Region) %>% 
  summarize(company_n = sum(n_N)) %>% 
  filter(Region != "other")

retailers <- retailers %>% 
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
  mutate(income20_f = as.numeric(income20_f)) 

retailers_summ <- retailers %>% 
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
         chall_sum = sum(c_across(pchall_sum:npchall_sum))/9) %>% 
  left_join(ppltn.c) %>% 
  select(Region, company_n, income20_median, chall_sum) %>% 
  rename(retailer_n = company_n,
         median_retailer_revenue = income20_median,
         retailer_challenge_sum = chall_sum)

df <- left_join(df, producers_summ) %>% left_join(retailers_summ)
  
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
  mutate(barrier_seedTreatlack = ifelse(NoOGSeedTreat %in% rmv, NA, 
                                        as.numeric(factor(NoOGSeedTreat, levels = levels_b))),
         barrier_buyerReq = ifelse(NoOGSeedProcessor %in% rmv, NA, 
                                   as.numeric(factor(NoOGSeedProcessor, levels = levels_b))),
         barrier_insuffQuantity = ifelse(NoOGSeedQuant %in% rmv, NA, 
                                         as.numeric(factor(NoOGSeedQuant, levels = levels_b))),
         barrier_saveOwn = ifelse(NoOGSeedSave %in% rmv, NA, 
                                  as.numeric(factor(NoOGSeedSave, levels = levels_b))),
         barrier_distrustQuality = ifelse(NoOGSeedQual %in% rmv, NA, 
                                          as.numeric(factor(NoOGSeedQual, levels = levels_b))),
         barrier_price = ifelse(NoOGSeedPrice %in% rmv, NA, 
                                as.numeric(factor(NoOGSeedPrice, levels = levels_b))),
         barrier_availability = ifelse(NoOGSeedVar %in% rmv, NA, 
                                       as.numeric(factor(NoOGSeedVar, levels = levels_b))),
         barrier_lackdesiredTraits = ifelse(NoOGSeedTrait %in% rmv, NA, 
                                            as.numeric(factor(NoOGSeedTrait, levels = levels_b))))
df$barrier_seedTreatlackf = factor(df$barrier_seedTreatlack)
levels(df$barrier_seedTreatlackf) = levels_b
df$barrier_buyerReqf = factor(df$barrier_buyerReq)
levels(df$barrier_buyerReqf) = levels_b
df$barrier_insuffQuantityf = factor(df$barrier_insuffQuantity)
levels(df$barrier_insuffQuantityf) = levels_b
df$barrier_saveOwnf = factor(df$barrier_saveOwn)
levels(df$barrier_saveOwnf) = levels_b
df$barrier_distrustQualityf = factor(df$barrier_distrustQuality)
levels(df$barrier_distrustQualityf) = levels_b
df$barrier_pricef = factor(df$barrier_price)
levels(df$barrier_pricef) = levels_b
df$barrier_availabilityf = factor(df$barrier_availability)
levels(df$barrier_availabilityf) = levels_b
df$barrier_lackdesiredTraitsf = factor(df$barrier_lackdesiredTraits)
levels(df$barrier_lackdesiredTraitsf) = levels_b
  
table(df$NoOGSeedTreat)
table(df$barrier_seedTreatlack)
table(df$barrier_seedTreatlackf)
df <- select(df, -contains("NoOG"))

# Factor analysis of barriers ----
## So far this is not convincing
mat <- df %>% 
  filter(survey_year == 2021) %>% 
  filter(!is.na(RespondentID)) %>% 
  tibble::column_to_rownames("RespondentID") %>% 
  dplyr::select(barrier_seedTreatlack:barrier_lackdesiredTraits) #%>%
#map_dfr(~ ifelse(. == "Not applicable", NA, .)) #%>%  
#filter(. != "Not applicable") #   Getting rid of only all the NAs

ResponseId <- rownames(mat)

#mat <- mat %>% 
#  map_dfr(~ ifelse(is.na(.), "No response", .)) %>% # Then converting
#  map_dfr(~ factor(., levels =
#                     c("No response",
#                       "Not a factor",
#                       "Slight factor",
#                       "Moderate factor",
#                       "Significant factor"))) %>%
#  map_dfr(~ as.numeric(.))

nrow(mat[complete.cases(mat),])
mat.nona <- mat[complete.cases(mat),]

fa = stats::factanal(mat.nona, 
                     factors = 3, 
                     n.obs = nrow(mat.nona), 
                     rotation="varimax")
psy::scree.plot(fa$correlation) # looks like 3 or 4

fa = factanal(mat.nona, 
              factors = 4, 
              n.obs = length(mat.nona), 
              rotation="varimax")
print(fa, digits=2, cutoff=.2, sort=TRUE)

# For now I am getting rid of the 5th factor because it is so sparse
loadings <- as.data.frame(fa$loadings[1:8, 1:4])
loadings$max <- apply(X=loadings, MARGIN=1, FUN=which.max)

#Factor 1
f <- loadings %>% filter(max == 1) %>% 
  row.names() ; f
ltm::cronbach.alpha(dplyr::select(mat, f), na.rm = T) # only 1
f1 <- mat %>% select(f) %>% 
  mutate(barrier_f_traits = rowMeans(., na.rm = T)) %>% 
  select(barrier_f_traits)

#Factor 2
f <- loadings %>% filter(max == 2) %>% 
  row.names() ; f
ltm::cronbach.alpha(dplyr::select(mat, f), na.rm = T) # only 1
f2 <- mat %>% select(f) %>% 
  mutate(barrier_f_treatment = rowMeans(., na.rm = T)) %>% 
  select(barrier_f_treatment)

#Factor 3
f <- loadings %>% filter(max == 3) %>% 
  row.names() ; f
ltm::cronbach.alpha(dplyr::select(mat, f), na.rm = T) # .59
f3 <- mat %>% select(f) %>% 
  mutate(barrier_f_quantity = rowMeans(., na.rm = T)) %>% 
  select(barrier_f_quantity)

#Factor 4
f <- loadings %>% filter(max == 4) %>% 
  row.names() ; f
ltm::cronbach.alpha(dplyr::select(mat, f), na.rm = T) # .38
f4 <- mat %>% select(f) %>% 
  mutate(barrier_f_other = rowMeans(., na.rm = T)) %>% 
  select(barrier_f_other)

# These are no good; not using them

df$growsVeg <- ifelse(is.na(df$VegiAcreage), 0, ifelse(df$VegiAcreage > 0, 1, 0))
growsveg <- df$RespondentID[df$growsVeg == 1]
df$growsField <- ifelse(is.na(df$FieldAcreage), 0, ifelse(df$FieldAcreage > 0, 1, 0))
growsfield <- df$RespondentID[df$growsField == 1]
df$growsForage <- ifelse(is.na(df$ForageAcreage), 0, ifelse(df$ForageAcreage > 0, 1, 0))
growsforage <- df$RespondentID[df$growsForage == 1]


# A second way is to lengthen the data so that each crop gets an observation, making repeats for crop type
df.long <- pivot_longer(df, cols = c(OGVegiSeed, OGFieldSeed, OGForageSeed),
                        names_to = "OGseed_type", values_to = "OGseed_percent")
# This removes the NAs, I think
df.long <- df.long[df.long$RespondentID %in% growsveg & df.long$OGseed_type == "OGVegiSeed" | 
                   df.long$RespondentID %in% growsfield & df.long$OGseed_type == "OGFieldSeed" | 
                   df.long$RespondentID %in% growsforage & df.long$OGseed_type == "OGForageSeed", ]

df$OGVegiSeed <- ifelse(is.na(df$OGVegiSeed), 0 , df$OGVegiSeed)
df$OGFieldSeed <- ifelse(is.na(df$OGFieldSeed), 0 , df$OGFieldSeed)
df$OGForageSeed <- ifelse(is.na(df$OGForageSeed), 0 , df$OGForageSeed)

df.long$OGcrop <- ifelse(stringr::str_detect(df.long$OGseed_type, "Vegi"), "Vegetable",
                  ifelse(stringr::str_detect(df.long$OGseed_type, "Forage"), "Forage",
                  ifelse(stringr::str_detect(df.long$OGseed_type, "Field"), "Field", NA)))

df.long$TotalCropAcreage <- ifelse(df.long$OGcrop == "Vegetable", df.long$VegiAcreage,
                            ifelse(df.long$OGcrop == "Forage", df.long$ForageAcreage,
                            ifelse(df.long$OGcrop == "Field", df.long$FieldAcreage, "missed")))

df.long <- df.long %>% 
  mutate(IncreasedOGSeed = case_when(
  OGcrop == "Vegetable" ~ IncreasedOGVegSeed,
  OGcrop == "Field" ~ IncreasedOGFieldSeed,
  OGcrop == "Forage" ~ IncreasedOGForageSeed,
  T ~ "Error"
)) %>% 
  mutate(IncreasedOGSeed = case_when(
    IncreasedOGSeed %in% c("4", "Have been using 100% certified organic seed for at least three years") ~ "Already at 100%",
    IncreasedOGSeed %in% c("3", "Increased the %") ~ "Yes",
    IncreasedOGSeed %in% c("About the same %", "Decreased the %", "1", "2") ~ "No",
    IncreasedOGSeed == "" ~ NA_character_,
    T ~ IncreasedOGSeed
  ))

df.long$IncreasedOGSeed <- ifelse(df.long$OGseed_percent == 100 & 
                               is.na(df.long$IncreasedOGSeed), "Already at 100%", 
                             df.long$IncreasedOGSeed)

saveRDS(df, "~/Box/organicseed/df.rds")
saveRDS(df.long, "~/Box/organicseed/df_long.rds")
