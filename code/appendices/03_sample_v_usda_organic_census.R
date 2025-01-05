library(dplyr)
library(stringr)
library(tidyUSDA)
source('code/keys.R')

# 0. Read in data just like for data summary ----
df.full <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled.rds") %>% 
  select(OGseed_percent_maincrop_prop,
         OG_value_s,
         barrier_all_rev_s,
         CertifierRequestn,
         noncommercial_seedsource_s,
         acreage_log_s,
         cropcat_n_s,
         years_certified_s,
         maincrop,
         Region,
         survey_year) %>% 
  na.omit() %>% 
  mutate(survey_year = as.numeric(as.character(survey_year))) %>% 
  mutate(survey_year = case_when(
    survey_year == 2011 ~ 2010,
    survey_year == 2016 ~ 2015,
    survey_year == 2021 ~ 2020,
    T ~ survey_year
  ))

western <- c("CA", "OR", "WA", "ID", "NV", "AZ", "UT", "NM", "CO", "WY", "MT", 
             "AK", "HI")
northcentral <- c("ND", "SD", "KS", "NE", "MN", "IA", "MO", "WI", "IL", "MI", 
                  "IN", "OH")
southern <- c("TX", "OK", "AR", "LA", "KY", "TN", "MS", "AL", "FL", "GA", "SC",
              "NC", "VA")
northeast <- c("WV", "MD", "DE", "NJ", "PA", "NY", "CT", "RI", "MA", "VT", 
               "NH", "ME")

# 1. Compare whole sample to USDA NASS organic survey census ----
## Understand tidyUSDA 
## Here are the parameters: https://quickstats.nass.usda.gov/
# These seem to be base: Program > Sector > Group > Commodity > Category > Data Iterm
## allProgram
## allSector
## allGroup
## allCommodity
# Then these shift...
## allCategory # comes after you choose a commodity, or?
## allDomain
## domains <- data.frame(allDomain)
dataitems <- data.frame("dataItems" = tidyUSDA::allDataItem)

## Aggregate data on acreage and operations ----
organic_acres <- getQuickstat(
  key = usda_quickstats_key,
  data_item = 'AG LAND, CROPLAND, ORGANIC - ACRES',
  geographic_level = 'STATE',
  year = c('2011', '2016', '2021'),
  geometry = F,
  lower48 = F) %>% 
  select(state_name, state_alpha, year, Value) %>% 
  rename(area_acres = Value)

organic_n_operations <- getQuickstat(
  key = usda_quickstats_key,
  data_item = 'AG LAND, CROPLAND, ORGANIC - NUMBER OF OPERATIONS',
  geographic_level = 'STATE',
  year = c('2011', '2016', '2021'),
  geometry = F,
  lower48 = F) %>% 
  select(state_name, state_alpha, year, Value) %>% 
  rename(operations_count = Value)

# Based on these outputs, it looks like:
# source: census; sector: economics; group: farm & land assets; commodity: ag land; category: area OR operations OR net income; domain: organic status

## Text: aggregate representativeness ----
totals <- organic_n_operations %>% 
  group_by(year) %>% 
  summarize(total = sum(operations_count))
totals
# These numbers are different than their reports, but this is focused on crops
# and so it is destined to be smaller. Unclear whether exempt farms (<$5,000/year)
# are included in here
# https://www.nass.usda.gov/Publications/Highlights/2015/2014_Organic_Survey_Highlights.pdf
# https://www.nass.usda.gov/Publications/Highlights/2022/2022_Organic_Highlights.pdf

## Summarize by state, region
organic_summary_by_state <- left_join(organic_acres, organic_n_operations) 
organic_summary_by_region <- organic_summary_by_state %>% 
  mutate(region = case_when(
    state_alpha %in% western ~ "West",
    state_alpha %in% northcentral ~ "North Central",
    state_alpha %in% southern ~ "South",
    state_alpha %in% northeast ~ "Northeast",
    T ~ NA
  )) %>% 
  group_by(year, region) %>% 
  summarize(total_acres = sum(area_acres, na.rm = T),
            avg_acres = round(mean(area_acres, na.rm = T)),
            total_operations_count = sum(operations_count, na.rm = T),
            avg_operations_count = round(mean(operations_count, na.rm = T)),
            avg_farm_size = round(total_acres/total_operations_count)
            # ^^ This is so crude and doesn't capture the distribution 
            ## But I can't seem to do anything else...
            ) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(national_operations = sum(total_operations_count),
         national_acres = sum(total_acres),
         prop_operations = round(total_operations_count/national_operations, 2),
         prop_acres = round(total_acres/national_acres, 2)) %>% 
  ungroup()


### Table A3a ----
## Year-region operations count
usda_ops <- select(organic_summary_by_region,
       year, region, prop_operations, total_operations_count) %>% 
  rename(usda_census_prop_operations = prop_operations,
         usda_census_n_operations = total_operations_count)
usda_ops

table_a3a <- df.full %>% 
  group_by(survey_year, Region) %>% 
  count() %>% 
  rename(total_operations_count = n) %>% 
  ungroup() %>% 
  group_by(survey_year) %>% 
  mutate(national_operations = sum(total_operations_count),
         prop_operations = round(total_operations_count/national_operations, 2)) %>% 
  select(survey_year, Region, prop_operations, total_operations_count) %>% 
  rename(sample_prop_operations = prop_operations,
         sample_n_operations = total_operations_count) %>% 
  cbind(usda_ops[,3:4]) %>% 
  mutate(representation_n_ops = sample_prop_operations-usda_census_prop_operations)
table_a3a

### Table A3b ----
## Year-region average acreage
## Can't get distributions of size with the USDA data
usda_acre <- select(organic_summary_by_region,
       year, region, avg_farm_size) %>% # Did use avg_farm_size
  rename(usda_census_avg_farm_size = avg_farm_size)
usda_acre

table_a3b <- df.full %>% 
  group_by(survey_year, Region) %>% 
  summarize(avg_farm_size = round(mean(TotalAcreage)),
            median_farm_size = round(median(TotalAcreage))) %>% 
  rename(sample_avg_farm_size = avg_farm_size) %>% 
  cbind(usda_acre[,3])  %>% 
  mutate(representation_acres = sample_avg_farm_size-usda_census_avg_farm_size)
table_a3b

### Table 3 ----
table_a3 <- left_join(table_a3a, table_a3b)

table_a3 <- table_a3 %>% 
  select(survey_year, Region, sample_n_operations,
         sample_prop_operations, usda_census_n_operations,
         usda_census_prop_operations,  representation_n_ops, 
         sample_avg_farm_size, usda_census_avg_farm_size, representation_acres)

## Right now it is overwhelming to compare proportions of each response
## I think it makes sense to make this less elaborate

table_a3 <- table_a3 %>% 
  mutate(representativeness_ops = 
           round(100*(sample_n_operations/usda_census_n_operations),1)) %>% 
  select(survey_year, Region, sample_n_operations,
         usda_census_n_operations, representativeness_ops,
         sample_avg_farm_size, usda_census_avg_farm_size, representation_acres) 

write.csv(table_a3, "~/Documents/Davis/R-Projects/organicseed_adoption/tables/append_rept1.csv", row.names = F)

# 2. Crop-specific data - NOT INCLUDING ----
# Then there seem to be crop-specific approaches 
## But I cannot get the data in aggregate, so I will group together

og_crops_acres <- data.frame('data' = unlist(str_extract_all(dataitems$dataItems, 
                                                     '.*\\, ORGANIC \\- ACRES HARVESTED')))
og_crops_acres <- og_crops_acres[!str_detect(og_crops_acres$data, 'TOTALS'),]
organic_crop_acres <- data.frame()
error <- c(4,6,14)  
for(i in 1:length(og_crops_acres)){
  if(i %in% error) {next}
  crop_df <- getQuickstat(
    key = usda_quickstats_key,
    data_item = og_crops_acres[i],
    geographic_level = 'STATE',
    year = c('2011', '2016', '2021'),
    geometry = F,
    lower48 = F)
  organic_crop_acres <- rbind(organic_crop_acres, crop_df)
  Sys.sleep(2)
}
og_crops_acres[error]

og_crops_ops <- data.frame('data' = unlist(str_extract_all(dataitems$dataItems, 
                                                             '.*\\, ORGANIC \\- OPERATIONS WITH AREA HARVESTED')))
og_crops_ops <- og_crops_ops[!str_detect(og_crops_ops$data, 'TOTALS'),]
organic_crop_ops <- data.frame()
error <- c(4,6,14)  
for(i in 1:length(og_crops_ops)){
  if(i %in% error) {next}
  crop_df <- getQuickstat(
    key = usda_quickstats_key,
    data_item = og_crops_ops[i],
    geographic_level = 'STATE',
    year = c('2011', '2016', '2021'),
    geometry = F,
    lower48 = F)
  organic_crop_ops <- rbind(organic_crop_ops, crop_df)
  Sys.sleep(2)
}
og_crops_ops[error]

## Join them together
nrow(organic_crop_acres) < nrow(organic_crop_ops)
organic_crop_acres_agg <- organic_crop_acres %>% 
  rename(area_acres = Value) %>% 
  # This is overcoming the distinction between some apples, beans, etc
  group_by(state_name, state_alpha, year, group_desc, commodity_desc) %>% 
  summarize(area_acres = sum(area_acres, na.rm = T)) %>% ungroup()
organic_crop_ops_agg <- organic_crop_ops %>% 
  rename(operations_count = Value) %>% 
  # This is overcoming the distinction between some apples, beans, etc
  group_by(state_name, state_alpha, year, group_desc, commodity_desc) %>%
  summarize(operations_count = sum(operations_count, na.rm = T)) %>% ungroup()
crop_summary_by_state <- left_join(organic_crop_ops_agg, organic_crop_acres_agg)
table(crop_summary_by_state$group_desc)

## Group by crop type and assign region -- without fruit or horticulture
crop_summary_by_region <- crop_summary_by_state %>% 
  mutate(region = case_when(
     state_alpha %in% western ~ "West",
     state_alpha %in% northcentral ~ "North Central",
     state_alpha %in% southern ~ "South",
     state_alpha %in% northeast ~ "Northeast",
     T ~ NA)) %>% 
  # have to get rid of these or else proportions are off
  filter(!(group_desc %in% c("HORTICULTURE", "FRUIT & TREE NUTS")))  %>% 
  group_by(year, region, group_desc) %>% 
  summarize(total_acres = sum(area_acres, na.rm = T),
            med_acres = round(median(area_acres, na.rm = T)),
            total_operations_count = sum(operations_count, na.rm = T),
            avg_operations_count = round(mean(operations_count, na.rm = T)),
            avg_farm_size = round(total_acres/total_operations_count)) %>% 
  ungroup() %>% 
  ## This is where the prop is coming from: year x crop
  group_by(year, group_desc) %>% 
  mutate(national_operations = sum(total_operations_count),
         national_acres = sum(total_acres),
         prop_operations = round(total_operations_count/national_operations, 2),
         prop_acres = round(total_acres/national_acres, 2)) %>% 
  rename(maincrop = group_desc, survey_year = year, Region = region) %>% 
  mutate(maincrop = case_when(
    maincrop == "FIELD CROPS" ~ "Field",
    maincrop == "VEGETABLES" ~ "Vegetables",
    T ~ maincrop
  )) 

### Table AXa ----
#### Operations count: by year, region, crop type
usda_crop_ops <- select(crop_summary_by_region,
                   survey_year, Region, maincrop, prop_operations, total_operations_count) %>% 
  rename(usda_census_prop_operations = prop_operations,
         usda_census_n_operations = total_operations_count) %>% 
  mutate(survey_year = case_when(
    survey_year == 2011 ~ 2010,
    survey_year == 2016 ~ 2015,
    survey_year == 2021 ~ 2020,
    T ~ survey_year
  ))
usda_crop_ops

table_aXa <- df.full %>% 
  group_by(survey_year, maincrop, Region) %>% 
  count() %>% 
  rename(total_operations_count = n) %>% 
  ungroup() %>% 
  group_by(survey_year, maincrop) %>% 
  mutate(national_operations = sum(total_operations_count),
         prop_operations = round(total_operations_count/national_operations, 2)) %>% 
  select(survey_year, Region, maincrop, prop_operations, total_operations_count) %>% 
  rename(sample_prop_operations = prop_operations,
         sample_n_operations = total_operations_count) %>% 
  full_join(usda_crop_ops) %>% 
  filter(maincrop != "Forage") %>% 
  mutate(representation_n_ops = sample_prop_operations-usda_census_prop_operations)
table_aXa

### Table AXb ----
usda_crop_acre <- select(crop_summary_by_region,
                         survey_year, Region, maincrop, avg_farm_size) %>% 
  rename(usda_census_avg_farm_size = avg_farm_size) %>% 
  mutate(survey_year = case_when(
    survey_year == 2011 ~ 2010,
    survey_year == 2016 ~ 2015,
    survey_year == 2021 ~ 2020,
    T ~ survey_year
  ))
usda_crop_acre

table_aXb <- df.full %>% 
  group_by(survey_year, Region, maincrop) %>% 
  summarize(avg_farm_size = round(mean(maincrop_acrage)),
            median_farm_size = round(median(maincrop_acrage))
            ) %>% 
  rename(sample_avg_farm_size = avg_farm_size) %>% 
  full_join(usda_crop_acre)  %>% 
  filter(maincrop != "Forage") %>% 
  mutate(representation_acres = sample_avg_farm_size-usda_census_avg_farm_size)
table_aXb

mean(df.full$TotalAcreage)
sd(df.full$TotalAcreage)
## Test -- these are not exactly the same as Table 2 in 03_data_summary because those account for crop specific acreage on a farm even if it is not your main crop, which is the most accurate, but this is for the acreage of a specific crop only if it is your main crop
df.full %>% 
  group_by(survey_year, maincrop) %>% 
  summarize(avg_farm_size_main = round(mean(maincrop_acrage)))

### Table X ----
table_aX <- left_join(table_aXa, table_aXb)
colnames(table_aX)
table_aX <- table_aX %>% 
  select(survey_year, maincrop, Region, sample_n_operations,
         sample_prop_operations, usda_census_n_operations,
         usda_census_prop_operations,  representation_n_ops, 
         sample_avg_farm_size, usda_census_avg_farm_size, representation_acres)

## Right now it is overwhelming to compare proportions of each response
## I think it makes sense to make this less elaborate

table_aX <- table_aX %>% 
  mutate(representativeness_ops = 
           round(100*(sample_n_operations/usda_census_n_operations),1)) %>% 
  select(survey_year, maincrop, Region, sample_n_operations,
         usda_census_n_operations, representativeness_ops,
         sample_avg_farm_size, usda_census_avg_farm_size, representation_acres) 
table_aX 

write.csv(table_aX, "~/Documents/Davis/R-Projects/organicseed_adoption/tables/append_rept2.csv", row.names = F)

