# 0. Read in data just like for data summary ----
library(dplyr)
# This is the dataframe where I have identified main crops of growers, so each observation is only one grower
df <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds") %>% 
  filter(Region != "International", maincrop != "Unknown") %>% 
  select(maincrop, OGseed_percent_maincrop, IncreasedOGSeed,
         VegiAcreage, FieldAcreage, ForageAcreage, # Total acres for each crop
         OGVegiSeed, OGFieldSeed, OGForageSeed, # % organic seed used
         TotalAcreage, 
         OGSeedOGIntegrity, 
         OGSeedOGIntegrityf, 
         OGSeedOGIntegrityn, # Belief that organic seed is important
         CertifierRequest,
         CertifierRequestn, # Certifier has requested
         survey_year, # year variables (funding)
         cropcat_n, 
         Region, # regional variables (region and seed production)
         contains("barrier"), # barriers
         noncommercial_seedsource,
         contains("AddSteps"),
         YearsCertified, data_coll_method) 

df.full <- select(df, OGseed_percent_maincrop,
                  OGseed_percent_maincrop, 
                  OGSeedOGIntegrityn,
                  CertifierRequestn,
                  maincrop, survey_year, Region, 
                  TotalAcreage, cropcat_n, 
                  VegiAcreage, FieldAcreage, ForageAcreage,
                  noncommercial_seedsource,
                  #YearsCertified
                  barrier_buyerReq,
                  barrier_insuffQuantity,
                  barrier_lackdesiredTraits,
                  barrier_availability, data_coll_method) %>% 
  na.omit() %>% 
  mutate(OGseed_percent_maincrop_prop = OGseed_percent_maincrop/100,
         CVseed_percent_maincrop_prop = 1-OGseed_percent_maincrop_prop)

# 2. Expand on convenience versus random ----
table(df.full$data_coll_method, df.full$survey_year)

# USDA census?
library(tidyUSDA)
## Here are the parameters: https://quickstats.nass.usda.gov/
source('code/keys.R')
# These seem to be base: Program > Sector > Group > Commodity > Category > Data Iterm
allProgram
allSector
allGroup
allCommodity
# Then these shift...
allCategory # comes after you choose a commodity, or?
allDomain
domains <- data.frame(allDomain)
dataitems <- data.frame(tidyUSDA::allDataItem)


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

western <- c("CA", "OR", "WA", "ID", "NV", "AZ", "UT", "NM", "CO", "WY", "MT", 
                     "AK", "HI")
northcentral <- c("ND", "SD", "KS", "NE", "MN", "IA", "MO", "WI", "IL", "MI", 
                          "IN", "OH")
southern <- c("TX", "OK", "AR", "LA", "KY", "TN", "MS", "AL", "FL", "GA", "SC",
                      "NC", "VA")
northeast <- c("WV", "MD", "DE", "NJ", "PA", "NY", "CT", "RI", "MA", "VT", 
                   "NH", "ME")

organic_summary_by_state <- left_join(organic_acres, organic_n_operations) 
organic_summary_by_region <- organic_summary_by_state %>% 
  mutate(region = case_when(
    state_alpha %in% western ~ "West",
    state_alpha %in% northcentral ~ "North Central",
    state_alpha %in% southern ~ "South",
    state_alpha %in% northeast ~ "Northeast",
    T ~ NA
  )) %>% 
  group_by(region, year) %>% 
  summarize(total_acres = sum(area_acres, na.rm = T),
            avg_acres = mean(area_acres, na.rm = T),
            total_operations_count = sum(operations_count, na.rm = T),
            avg_operations_count = mean(operations_count, na.rm = T),
            avg_farm_size = round(total_acres/total_operations_count))


# Based on these outputs, it looks like:
# source: census; sector: economics; group: farm & land assets; commodity: ag land; category: area OR operations OR net income; domain: organic status

# Then there seem to be crop-specific approaches

allProgram
## Are they representative by geography? ----
stats
## Are they representative by crop? ----

## Are they representative by farm size? ----


# Here I will download organic farm census data to compare to samples