library(tidyverse)
library(lubridate)
setwd("~/Dropbox/SOSInteractiveSurvey/")

assign_yr_range <- function(year_var){
  x <- ifelse(year_var >= 2002 & year_var <= 2008, "2002-2008",
              ifelse(year_var >= 2009 & year_var <= 2013, "2009-2013",
                     ifelse(year_var >= 2014 & year_var <= 2018, "2014-2018", NA_character_)))
  return(x)
}

ids <- 1:(3*4*3)
year_range <- c("2002-2008","2009-2013", "2014-2018")
regions <- c("North Central", "Northeast", "South", "West")
maincrop <- c("Vegetables", "Forage", "Field") 
shape_crops <- data.frame("year_range" = rep(year_range, each = 4*3),
                          "Region" = c(rep(regions, length.out = 3*4*3)),
                          "maincrop" = c(rep(maincrop, each = 4, 
                                             length.out = 3*4*3)),
                          "id" = ids)
ids <- 1:(3*4)
shape_nocrops <- data.frame("year_range" = rep(year_range, each = 4),
                            "Region" = c(rep(regions, length.out = 3*4)),
                            "id" = ids)

year_range2 <- c("pre-2002", "2002-2008","2009-2013", "2014-2018")
ids <- 1:(4*4)
shape_nocrops_pre2002 <- data.frame("year_range" = rep(year_range2, each = 4),
                                    "Region" = c(rep(regions, length.out = 4*4)),
                                    "id" = ids)
# ---- FUNDING ----
## Setting up funds at year range, region, and crop ----
funds <- read.csv('~/Box/quantifying_seed_innovation/organicseed_funding.csv') %>% 
  rename("funding_amt" = award_dollars) %>% 
  filter()
funds <- funds %>% 
  mutate(year_range = case_when(
    year %in% 2002:2008 ~ "2002-2008",
    year %in% 2009:2013 ~ "2009-2013",
    year %in% 2014:2018 ~ "2014-2018",
    T ~ NA_character_
  )) %>% 
  filter(!is.na(year_range)) %>% 
  select(year, year_range, Region, maincrop, funding_amt)

## Table 1: Funding only by years ----
funds_yearonly <- funds %>% 
  group_by(year_range) %>% 
  summarize(funding_amt = sum(funding_amt, na.rm = T))

funds_yearonly$average[1] <- funds_yearonly$funding_amt[1]/7
funds_yearonly$average[2] <- funds_yearonly$funding_amt[2]/5
funds_yearonly$average[3] <- funds_yearonly$funding_amt[3]/5

## Funding only by region  ----
funds_regiononly <- funds %>% 
  group_by(Region) %>% 
  summarize(funding_amt = sum(funding_amt))

funds_regiononly$average[1] <- funds_regiononly$funding_amt[1]/7
funds_regiononly$average[2] <- funds_regiononly$funding_amt[2]/5
funds_regiononly$average[3] <- funds_regiononly$funding_amt[3]/5

## Funding by year and region----
funds_year_region <- funds %>% 
  group_by(year_range, Region) %>% 
  summarize(funding_amt = sum(funding_amt))

funds_year_region$average[1] <- funds_year_region$funding_amt[1]/7
funds_year_region$average[2] <- funds_year_region$funding_amt[2]/5
funds_year_region$average[3] <- funds_year_region$funding_amt[3]/5

## Funding by year range, region, and crop ----

funds_year_region_crop <- funds %>% 
  group_by(year_range, Region, maincrop) %>% 
  summarize(funding_amt = sum(funding_amt, na.rm = T)) 

funds_year_region_crop$average[1] <- funds_year_region_crop$funding_amt[1]/7
funds_year_region_crop$average[2] <- funds_year_region_crop$funding_amt[2]/5
funds_year_region_crop$average[3] <- funds_year_region_crop$funding_amt[3]/5

# Remove multiple regions and crops ----
funds_nomulti <- read.csv("~/Box/quantifying_seed_innovation/organicseed_funding_year_region_crop.csv") %>% 
  rename("funding_amt" = award_dollars) %>% 
  mutate(year_range = case_when(
    year %in% 2002:2008 ~ "2002-2008",
    year %in% 2009:2013 ~ "2009-2013",
    year %in% 2014:2018 ~ "2014-2018",
    T ~ NA_character_
  )) %>% 
  filter(!is.na(year_range))

funds_year_region_crop <- funds_nomulti %>% 
  group_by(year_range, Region, maincrop) %>% 
  summarize(funding_amt = sum(funding_amt, na.rm = T)) 

funds_year_region_crop$average_annual_funding[1] <- funds_year_region_crop$funding_amt[1]/7
funds_year_region_crop$average_annual_funding[2] <- funds_year_region_crop$funding_amt[2]/5
funds_year_region_crop$average_annual_funding[3] <- funds_year_region_crop$funding_amt[3]/5

# ---- SUPPLY CHAIN ----
## Patterns for matching later ----
western <- c("California", "Oregon", "Washington", "Idaho", "Nevada", "Arizona", "Utah", "New Mexico", "Colorado", "Wyoming", "Montana", "Alaska", "Hawaii")
northcentral <- c("North Dakota", "South Dakota", "Kansas", "Nebraska", "Minnesota", "Iowa", "Missouri", "Wisconsin", "Illinois", "Michigan", "Indiana", "Ohio")
southern <- c("Texas", "Oklahoma", "Arkansas", "Louisiana", "Kentucky", "Tennessee", "Mississippi", "Alabama", "Florida", "Georgia", "South Carolina", "North Carolina", "Virginia")
northeast <- c("West Virginia", "Maryland", "Delaware", "New Jersey", "Pennsylvania", "New York", "Connecticut", "Rhode Island", "Massachusetts", "Vermont", "New Hampshire", "Maine")
field_pattern <- "[Cc]orn|[Ss]oy|[Cc]otton|[Ww]heat|[Aa]maranth|[Bb]arley|[Rr]ye|[Ss]pelt|[Bb]uckwheat|[Oo]at|[Mm]illet|[Qq]uinoa|[Tt]riticale|[Rr]ice|[Ss]orghum"
forage_pattern <- "[Hh]ay|[Pp]asture|[Aa]lfalfa|[Cc]lover|[Gg]rass|[Ff]ieldpea|[Vv]etch|[Ss]ilage"
veg_pattern <- read.csv("~/Box/osa_networks/data_combined/crop_family_manual.csv") %>%
  filter(cat == "Vegetables") %>% 
  mutate(variety = textstem::lemmatize_words(variety))
veg_pattern <- paste(veg_pattern$variety, collapse = "|") %>% 
  str_replace_all("wsquash|ssquash", "squash")

## Reading in datasets ----
allgrowers <- read.csv('~/Box/quantifying_seed_innovation/nop_allgrowers.csv') %>% 
  mutate(year_range = assign_yr_range(cert_year),
         year_range = ifelse(cert_year <= 2001, "pre-2002", year_range)) %>% 
  filter(!is.na(year_range))
seedproducers <- read.csv('~/Box/quantifying_seed_innovation/nop_seedsupplychain.csv') %>% 
  mutate(year_range = assign_yr_range(cert_year),
         year_range = ifelse(cert_year <= 2001, "pre-2002", year_range)) %>% 
  filter(!is.na(year_range))
dnb <- read.csv('~/Box/quantifying_seed_innovation/dnb.csv') %>% 
  filter(Region != "Outside US" & !is.na(Region)) %>% 
  mutate(year_range = assign_yr_range(year_started),
         year_range = ifelse(year_started <= 2001, "pre-2002", year_range)) %>% 
  filter(!is.na(year_range))

years <- data.frame(year = min(dnb$year_started):2018)
dnb_table <- dnb %>% 
  #filter(year_started >= 2002 & year_started <= 2018) %>% 
  group_by(year_started) %>% 
  count()  %>% 
  rename(company_n = n) %>% 
  rename(year = year_started) %>% 
  right_join(years) %>% 
  mutate(company_n = ifelse(is.na(company_n), 0, company_n)) %>% 
  arrange(year) 

dnb_table$company_n_building <- NA
for(i in 1:nrow(dnb_table)){
  if(i == 1){
    dnb_table$company_n_building[i] <- dnb_table$company_n[i]
  }
  else {
    dnb_table$company_n_building[i] <- dnb_table$company_n[i] +
      dnb_table$company_n_building[i-1]
  }  
}

years <- data.frame(year = min(allgrowers$cert_year):2018)
allgrowers_table <- allgrowers %>% 
  #filter(cert_year >= 2000 & cert_year <= 2019) %>% 
  group_by(cert_year) %>% 
  count() %>% 
  rename(grower_n = n) %>% 
  rename(year = cert_year) %>% 
  right_join(years) %>% 
  mutate(grower_n = ifelse(is.na(grower_n), 0, grower_n)) 

allgrowers_table$grower_n_building <- NA
for(i in 1:nrow(allgrowers_table)){
  if(i == 1){
    allgrowers_table$grower_n_building[i] <- allgrowers_table$grower_n[i]
  }
  else {
    allgrowers_table$grower_n_building[i] <- allgrowers_table$grower_n[i] +
      allgrowers_table$grower_n_building[i-1]
  }  
}
years <- data.frame(year = min(seedproducers$cert_year):2018)
seedproducers_table <- seedproducers %>% 
  #filter(cert_year >= 2002 & cert_year <= 2018) %>% 
  group_by(cert_year) %>% 
  count() %>% 
  rename(seedproducers_n = n) %>% 
  rename(year = cert_year) %>% 
  right_join(years) %>% 
  mutate(seedproducers_n = ifelse(is.na(seedproducers_n), 0, seedproducers_n)) %>% 
  arrange(year)

seedproducers_table$seedproducers_n_building <- NA
for(i in 1:nrow(seedproducers_table)){
  if(i == 1){
    seedproducers_table$seedproducers_n_building[i] <- seedproducers_table$seedproducers_n[i]
  }
  else {
    seedproducers_table$seedproducers_n_building[i] <- seedproducers_table$seedproducers_n[i] +
      seedproducers_table$seedproducers_n_building[i-1]
  }  
}

market_table <- full_join(dnb_table, seedproducers_table) 

valuechain_table <- full_join(market_table, allgrowers_table) %>% 
  mutate(supplychain_n = company_n_building + seedproducers_n_building,
         ratio = round(grower_n_building/supplychain_n)) %>% 
  mutate(year_range = case_when(
    year %in% 2002:2008 ~ "2002-2008",
    year %in% 2009:2013 ~ "2009-2013",
    year %in% 2014:2018 ~ "2014-2018",
    T ~ NA_character_
  )) 

## Supply chain by year  ----
supplychain_yearonly <- valuechain_table %>% 
  group_by(year_range) %>% 
  summarize(supplychain_n_avg = mean(seedproducers_n + company_n),
            grower_n_avg = mean(grower_n),
            ratio_avg = mean(ratio)) 

## Supply chain by year and region ----

dnb_year_reg_pre02 <- dnb %>% 
  group_by(year_range, Region) %>% 
  count() %>% 
  rename(company_n = n) %>% 
  right_join(shape_nocrops_pre2002) %>% 
  mutate(company_n = ifelse(is.na(company_n), 0, company_n)) %>% 
  mutate(year_range = factor(year_range, c("pre-2002", "2002-2008", "2009-2013", "2014-2018"))) %>% 
  arrange(Region, year_range)

dnb_year_reg_pre02$company_n_building <- NA
for(i in 1:nrow(dnb_year_reg_pre02)){
  if(i %in% c(1,5,9,13)){
    dnb_year_reg_pre02$company_n_building[i] <- dnb_year_reg_pre02$company_n[i]
  }
  else {
    dnb_year_reg_pre02$company_n_building[i] <- dnb_year_reg_pre02$company_n[i] +
      dnb_year_reg_pre02$company_n_building[i-1]
  }  
}

allgrowers_year_reg_pre02 <- allgrowers %>% 
  group_by(year_range, Region) %>% 
  count() %>% 
  rename(grower_n = n) %>% 
  right_join(shape_nocrops_pre2002) %>% 
  mutate(grower_n = ifelse(is.na(grower_n), 0, grower_n)) %>% 
  select(-id) %>% 
  mutate(year_range = factor(year_range, c("pre-2002", "2002-2008", "2009-2013", "2014-2018"))) %>% 
  arrange(Region, year_range)

allgrowers_year_reg_pre02$grower_n_building <- NA
for(i in 1:nrow(allgrowers_year_reg_pre02)){
  if(i %in% c(1,5,9,13)){
    allgrowers_year_reg_pre02$grower_n_building[i] <- allgrowers_year_reg_pre02$grower_n[i]
  }
  else {
    allgrowers_year_reg_pre02$grower_n_building[i] <- allgrowers_year_reg_pre02$grower_n[i] +
      allgrowers_year_reg_pre02$grower_n_building[i-1]
  }  
}

seedproducers_year_reg_pre02 <- seedproducers %>% 
  group_by(year_range, Region) %>% 
  count() %>% 
  rename(seedproducers_n = n) %>% 
  right_join(shape_nocrops_pre2002) %>% 
  mutate(seedproducers_n = ifelse(is.na(seedproducers_n), 0, seedproducers_n)) %>% 
  select(-id) %>% 
  mutate(year_range = factor(year_range, c("pre-2002", "2002-2008", "2009-2013", "2014-2018"))) %>% 
  arrange(Region, year_range)

seedproducers_year_reg_pre02$seedproducers_n_building <- NA
for(i in 1:nrow(seedproducers_year_reg_pre02)){
  if(i %in% c(1,5,9,13)){
    seedproducers_year_reg_pre02$seedproducers_n_building[i] <- seedproducers_year_reg_pre02$seedproducers_n[i]
  }
  else {
    seedproducers_year_reg_pre02$seedproducers_n_building[i] <- seedproducers_year_reg_pre02$seedproducers_n[i] +
      seedproducers_year_reg_pre02$seedproducers_n_building[i-1]
  }  
}

market_reg_pre02 <- full_join(dnb_year_reg_pre02, seedproducers_year_reg_pre02) 
valuechain_reg_pre02 <- full_join(market_reg_pre02, allgrowers_year_reg_pre02) %>% 
  mutate(supplychain_n = company_n_building + seedproducers_n_building,
         ratio_building = round(grower_n_building/supplychain_n)) %>% 
  select(-id)

supplychain_year_region <- valuechain_reg_pre02 %>% 
  filter(year_range != "pre-2002" & !is.na(year_range)) %>% 
  group_by(year_range, Region) 

# Sales ----
dnb_sales_reg <- dnb %>% 
  group_by(Region) %>% 
  summarize(avg_annual_sales = round(mean(sales, na.rm = T)/1000000, 2))

# Intellectual property ----
pp <- read.csv('~/Box/quantifying_seed_innovation/uspto_plantpatents.csv') %>% 
  filter(Region != "International" & !is.na(Region)) %>% 
  filter(ornamental == F & organic == T) %>% 
  select(WKU, year_issued, cropcat_veg, cropcat_field, cropcat_forage, Region) %>% 
  rename("id" = WKU) %>% 
  mutate(year_range = assign_yr_range(year_issued)) %>% 
  select(-year_issued) %>% 
  filter(!is.na(year_range)) %>% 
  select(-id)
pvp <- read.csv('~/Box/quantifying_seed_innovation/pvp.csv') %>% 
  filter(Region != "International" & !is.na(Region)) %>% 
  filter(ornamental == F & organic == T) %>% 
  select(`Application..`, year_issued, cropcat_veg, cropcat_field, cropcat_forage, Region) %>% 
  rename("id" = `Application..`) %>% 
  mutate(year_range = assign_yr_range(year_issued)) %>% 
  select(-year_issued) %>% 
  filter(!is.na(year_range)) %>% 
  select(-id)
ossi <- read.csv('~/Box/quantifying_seed_innovation/ossi.csv') %>% 
  filter(Region != "International" & !is.na(Region)) %>% 
  filter(organic == T) %>% 
  select(variety_name, year_pledge, cropcat_veg, cropcat_field, cropcat_forage, Region) %>% 
  rename("id" = variety_name) %>% 
  mutate(year_range = assign_yr_range(year_pledge)) %>% 
  select(-year_pledge) %>% 
  filter(!is.na(year_range)) %>% 
  select(-id)
ip_reg <- rbind(pp, pvp, ossi) %>% 
  mutate(maincrop = case_when(
    cropcat_veg == T & cropcat_field == F & cropcat_forage == F ~ "Vegetables",
    cropcat_veg == F & cropcat_field == T & cropcat_forage == F ~ "Field",
    cropcat_veg == F & cropcat_field == F & cropcat_forage == T ~ "Forage",
    T ~ NA_character_
  )) %>% 
  # there are some crops that are flowers
  filter(!is.na(maincrop))

ip_yearonly <- ip_reg %>% 
  group_by(year_range) %>% 
  count()

# re-running ossi because there are some that are marked both field and veg and then they end up with no crop and get removed.
ip_year_region_region <- ip_reg %>% 
  group_by(year_range, Region, maincrop) %>% 
  count()

# Combining them ----
regional_summary <- left_join(funds_year_region_crop, supplychain_year_region) %>% 
  left_join(ip_year_region_region) %>% left_join(dnb_sales_reg)

# I am not saving this, but it should match exactly what came out of the sumary from quant, which was saved in the organicseed adoption Box

## Tables 1b and 1c ----
write.csv(funds_yearonly, "~/Documents/Davis/R-Projects/organicseed/tables/table1b.csv", row.names = F)
write.csv(supplychain_yearonly, "~/Documents/Davis/R-Projects/organicseed/tables/table1c.csv", row.names = F)



