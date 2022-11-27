library(tidyverse)
library(lubridate)
setwd("~/Dropbox/SOSInteractiveSurvey/")

# ---- FUNDING ----
## Setting up funds at year range, region, and crop ----
funds <- read.csv("local_data/public-funding-Dec21.csv")

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
  mutate(maincrop = case_when(
    proj_crop == "Field Crops" ~ "Field",
    proj_crop == "Forage and / or Cover Crops" ~ "Forage",
    proj_crop == "Vegetables" ~ "Vegetables",
    T ~ "Multiple"
  )) %>% 
  filter(!is.na(year_range)) %>% 
  select(year, year_range, Region, maincrop, funding_amt)

## Table 1: Funding only by years ----
funds_yearonly <- funds %>% 
  group_by(year_range) %>% 
  summarize(avg = sum(funding_amt, na.rm = T)/5)

## Funding only by region  ----
funds_regiononly <- funds %>% 
  group_by(Region) %>% 
  summarize(funding_amt = sum(funding_amt))

## Funding by year and region----
funds_year_region <- funds %>% 
  group_by(year_range, Region) %>% 
  summarize(funding_amt = sum(funding_amt))

## Funding by year range, region, and crop ----

funds_year_region_crop <- funds %>% 
  group_by(year_range, Region, maincrop) %>% 
  summarize(total_funds = sum(funding_amt, na.rm = T)) 

# What's missing? Make it zero
missing <- data.frame(year_range = 
                        c(rep("2004-2008", 6), rep("2009-2013", 1), 
                          rep("2014-2018", 4)),
           Region = c("North Central", "Northeast", rep("South", 3), 
                      "West", "Northeast", "North Central", "Northeast", 
                      "South", "West"),
           maincrop = c("Vegetables", "Forage", "Field", "Forage", 
                        "Vegetables", "Forage", "Forage",
                        "Forage", "Forage", "Forage", "Forage"),
           total_funds = c(rep(0, (6+1+4))))

funds_year_region_crop <- rbind(funds_year_region_crop, missing)
unmatched_funds <- funds_year_region_crop %>% 
  filter(Region == "Multi-region" | maincrop == "Multiple")

# Needing to add the unmatched values generally to region, crop, and year
funds_year_region_crop <- funds_year_region_crop %>% 
  mutate(amt_multir_multic = case_when(
    year_range == "2004-2008"  ~ filter(unmatched_funds, 
                                        year_range == "2004-2008" &
                                          Region == "Multi-region" &
                                          maincrop == "Multiple")[[4]], 
    year_range == "2009-2013" ~ filter(unmatched_funds, 
                                       year_range == "2009-2013" &
                                         Region == "Multi-region" &
                                         maincrop == "Multiple")[[4]], 
    year_range == "2014-2018" ~ filter(unmatched_funds, 
                                       year_range == "2014-2018" &
                                         Region == "Multi-region" &
                                         maincrop == "Multiple")[[4]], 
    T ~ 0)) %>% 
  mutate(amt_oner_multic = case_when(
    year_range == "2004-2008" &
      Region == "North Central"  ~ filter(unmatched_funds, 
                                          year_range == "2004-2008" &
                                            Region == "North Central" &
                                            maincrop == "Multiple")[[4]],
    year_range == "2004-2008" &
      Region == "Northeast" ~ filter(unmatched_funds, 
                                     year_range == "2004-2008" &
                                       Region == "Northeast" &
                                       maincrop == "Multiple")[[4]],
    year_range == "2004-2008" &
      Region == "South" ~ filter(unmatched_funds, 
                                 year_range == "2004-2008" &
                                   Region == "South" &
                                   maincrop == "Multiple")[[4]],
    year_range == "2004-2008" &
      Region == "West" ~ filter(unmatched_funds, 
                                year_range == "2004-2008" &
                                  Region == "West" &
                                  maincrop == "Multiple")[[4]],
    year_range == "2009-2013" &
      Region == "North Central" ~ filter(unmatched_funds, 
                                         year_range == "2009-2013" &
                                           Region == "North Central" &
                                           maincrop == "Multiple")[[4]], 
    year_range == "2009-2013" &
      Region == "Northeast"  ~ filter(unmatched_funds, 
                                      year_range == "2009-2013" &
                                        Region == "Northeast" &
                                        maincrop == "Multiple")[[4]], 
    year_range == "2009-2013" &
      Region == "South" ~ filter(unmatched_funds, 
                                 year_range == "2009-2013" &
                                   Region == "South" &
                                   maincrop == "Multiple")[[4]], 
    year_range == "2009-2013" &
      Region == "West" ~ filter(unmatched_funds, 
                                year_range == "2009-2013" &
                                  Region == "West" &
                                  maincrop == "Multiple")[[4]], 
    year_range == "2014-2018" &
      Region == "North Central" ~ filter(unmatched_funds, 
                                         year_range == "2014-2018" &
                                           Region == "North Central" &
                                           maincrop == "Multiple")[[4]], 
    year_range == "2014-2018" &
      Region == "South" ~ filter(unmatched_funds, 
                                 year_range == "2014-2018" &
                                   Region == "South" &
                                   maincrop == "Multiple")[[4]], 
    year_range == "2014-2018" &
      Region == "West" ~ filter(unmatched_funds, 
                                year_range == "2014-2018" &
                                  Region == "West" &
                                  maincrop == "Multiple")[[4]],
    T ~ 0)) %>% 
  mutate(amt_multir_onec = case_when(
    year_range == "2009-2013" &
      maincrop == "Field" ~ filter(unmatched_funds, 
                                   year_range == "2009-2013" &
                                     Region == "Multi-region" &
                                     maincrop == "Field")[[4]], 
    year_range == "2009-2013" &
      maincrop == "Vegetables" ~ filter(unmatched_funds, 
                                        year_range == "2009-2013" &
                                          Region == "Multi-region" &
                                          maincrop == "Vegetables")[[4]],
    year_range == "2014-2018" &
      maincrop == "Field" ~ filter(unmatched_funds, 
                                   year_range == "2014-2018" &
                                     Region == "Multi-region" &
                                     maincrop == "Field")[[4]], 
    year_range == "2014-2018" &
      maincrop == "Forage" ~ filter(unmatched_funds, 
                                    year_range == "2014-2018" &
                                      Region == "Multi-region" &
                                      maincrop == "Forage")[[4]], 
    year_range == "2014-2018" &
      maincrop == "Vegetables" ~ filter(unmatched_funds, 
                                        year_range == "2014-2018" &
                                          Region == "Multi-region" &
                                          maincrop == "Vegetables")[[4]], 
    T ~ 0))

# Then get rid of the multiple and add them in elsewhere
funds_year_region_crop <- funds_year_region_crop %>% 
  ungroup() %>% 
  filter(Region != "Multi-region", maincrop != "Multiple") %>% 
  mutate(total_funds = rowSums(.[4:7])) %>% 
  select(year_range, Region, maincrop, total_funds) %>% 
  mutate(funding_avg_annual_amt = total_funds/5)


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
# These are the survey data only
nodes <- read.csv("~/Box/osa_networks/data_combined/nodes_plus_surveydata_attributes.csv") %>% 
  select(Operation, variety_n, cropcat_n)
master <- read.csv("~/Box/osa_networks/data_combined/master_list_manual.csv") %>% 
  select(Operation, State)
producers <- readRDS("~/Box/osa_networks/data_combined/surveys.rds") %>% 
  filter(act_seed_producer == T | act_seed_breeder == T |
           act_seed_handling == T | act_seed_retail == T) %>% 
  left_join(nodes) %>% 
  mutate(cropcat_field = case_when(cropcat_grain == T ~ T, T ~ cropcat_field)) %>% 
  mutate(cropcat_veg = case_when(cropcat_propogules == T ~ T, T ~ cropcat_veg)) %>% 
  mutate(cropcat_other = case_when(cropcat_orn == T |
                                     cropcat_herbs == T ~ T,
                                   T ~ F
  )) %>% 
  rename(physical_street = Street.Address)

## Beyond the survey: NOP ----
# Now for beyond the survey. The NOP list here has all organic growers (crops and livestock), but filtered to certified before 2020 so that it is appropriate for my groupings
nop <- read.csv("~/Box/organicseed/NOP_USorganicCrops_6.28.22.csv") %>% 
  slice(-(1:2)) %>% 
  filter(Physical.Address..Country == "United States of America (the)") %>% 
  mutate(cert_year = year(mdy(.$Effective.Date.of.Operation.Status))) %>% 
  filter(cert_year <= 2020) %>% 
  mutate(Region = case_when(
    Physical.Address..State.Province %in% western ~ "West",
    Physical.Address..State.Province %in% northcentral ~ "North Central",
    Physical.Address..State.Province %in% southern ~ "South",
    Physical.Address..State.Province %in% northeast ~ "Northeast",
    T ~ NA_character_)) %>% 
  mutate(yrs_ag = 2021-cert_year) %>% 
  mutate(cropcat_veg = str_detect(tolower(Certified.Products.Under.CROPS.Scope), 
                                  paste("[Vv]egetables", veg_pattern,
                                        "[Gg]arlic", sep = "|")),
         cropcat_field = str_detect(tolower(Certified.Products.Under.CROPS.Scope), 
                                    field_pattern),
         cropcat_forage = str_detect(Certified.Products.Under.CROPS.Scope, 
                                     forage_pattern))
# Then this is the NOP list that I used to consider the population of seed producers, to match up with the populatioN data and current nop database
nop.old <- read.csv("~/Box/osa_networks/data_contacts/01_raw_inputs/NOP.database.products.csv") %>% 
  mutate(cert_year = year(mdy(.$Effective.Date.of.Operation.Status)))
ppltn.p <- read.csv("~/Box/osa_networks/data_producers/00_populatioN_data/total_populatioN.csv")

## Beyond the survey: D&B ----
dnb <- read.csv("~/Box/organicseed/dnb_organic_seed_companies.csv") %>% 
  full_join(read.csv("~/Box/organicseed/dnb_manual_pull.csv"), 
            by = c("Company.Name" = "dnb_company_name")) %>% 
  filter(Company.Name != "") %>% 
  select(Operation, Company.Name, City, State.Or.Province, 
         Postal.Code, Country.Region, URL,
         Sales..USD., Pre.Tax.Profit..USD., Assets..USD., Employees..Single.Site.,
         Employees..Total., Parent.Company, Parent.Country.Region, 
         Address.Line.1, Key.ID,
         year_started) %>% 
  rename(Company_dnb = Company.Name,
         sales = Sales..USD., assets = Assets..USD., 
         n_employees = Employees..Total.,
         physical_street = Address.Line.1) %>% 
  mutate(State = trimws(State.Or.Province)) %>% 
  mutate(Region = case_when(
    State %in% western ~ "West",
    State %in% northcentral ~ "North Central",
    State %in% southern ~ "South",
    State %in% northeast ~ "Northeast",
    Country.Region != "United States" ~ "Outside US",
    T ~ NA_character_)) %>% 
  mutate(dnblisting = T)


## Combine D&B and NOP ----
### First bind in D&B
p_co <- full_join(producers, dnb, by = c("Operation", 
                                             "Region")) %>% 
  mutate(physical_street = ifelse(is.na(.$physical_street.x) & !is.na(.$physical_street.y),
                         .$physical_street.y, .$physical_street.x)) %>% 
  select(Operation, RecipientEmail, ResponseId, income20, income20_c, 
         contains("pchall"), Region, cropcat_field, cropcat_forage, 
         cropcat_grain, cropcat_veg, cropcat_orn, cropcat_propogules, 
         cropcat_herbs, physical_street,
         variety_n, cropcat_n, yrs_ag, yrs_orgseed, 
         sales, n_employees, year_started, dnblisting) %>% 
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
  rename(Email = RecipientEmail)  %>% 
  mutate(yrs_ag = 2022-year_started)

### Then clean up old NOP to get the full NOP data
ppltn_p_nop <- nop.old %>% 
  filter(nop.old$Email %in% 
           ppltn.p$Email[!is.na(ppltn.p$email)] |
           nop.old$Operation.Name %in% 
           ppltn.p$operation[!is.na(ppltn.p$operation)] |
           nop.old$Physical.Address..Street.1 %in% 
           ppltn.p$physical_street[!is.na(ppltn.p$physical_street)]) %>% 
  rename(Operation = `Operation.Name`, 
         physical_street = `Physical.Address..Street.1`) %>% 
  mutate(cropcat_veg = str_detect(tolower(Certified.Products.Under.CROPS.Scope), 
                                          paste("[Vv]egetables", veg_pattern,
                                                "[Gg]arlic", sep = "|")),
         cropcat_field = str_detect(tolower(Certified.Products.Under.CROPS.Scope), 
                                     field_pattern),
         cropcat_forage = str_detect(Certified.Products.Under.CROPS.Scope, 
                                      forage_pattern)) %>% 
  mutate(Region = case_when(
    Physical.Address..State.Province %in% western ~ "West",
    Physical.Address..State.Province %in% northcentral ~ "North Central",
    Physical.Address..State.Province %in% southern ~ "South",
    Physical.Address..State.Province %in% northeast ~ "Northeast",
    T ~ NA_character_)) %>% 
  mutate(yrs_ag = 2021-cert_year) %>% 
  select(Operation, physical_street, Email,
         cert_year, Region,  
         yrs_ag, cropcat_veg, cropcat_field, cropcat_forage) 

### Then merge the old NOP list with the survey and D&B.
# Warning, this merge gets weird. So first we take those from the database that have the same operation name as those surveyed. Left to p_co keeps survey data by default over database data -- no matches
p_co_left <- left_join(p_co, select(ppltn_p_nop, Operation, cert_year, yrs_ag))  %>% unique()
# Then we remove Operation
# Then we merge on email, so we stack on those that have the same email, but we don't merge by some data because we want the manually specified values to be from survey
p_co_email <- full_join(filter(select(ppltn_p_nop, Email, yrs_ag, cert_year, Region, 
                                      cropcat_veg, cropcat_field, cropcat_forage), Email != ""), 
                        p_co_left, by = c("Email", "Region")) %>% 
  mutate(yrs_ag = ifelse(is.na(.$yrs_ag.x) & !is.na(.$yrs_ag.y),
                         .$yrs_ag.y, .$yrs_ag.x),
         cert_year = ifelse(is.na(.$cert_year.x) & !is.na(.$cert_year.y),
                         .$cert_year.y, .$cert_year.x),
         #physical_street = ifelse(.$physical_street.x != "" & 
         #                           !is.na(.$physical_street.y),
         #                   .$physical_street.y, .$physical_street.x),
         cropcat_veg = ifelse(is.na(.$cropcat_veg.y),
                         .$cropcat_veg.x, .$cropcat_veg.y),
         cropcat_field = ifelse(is.na(.$cropcat_field.y),
                         .$cropcat_field.x, .$cropcat_field.y),
         cropcat_forage = ifelse(is.na(.$cropcat_forage.y),
                         .$cropcat_forage.x, .$cropcat_forage.y)) %>% 
  select(-c(yrs_ag.x, yrs_ag.y, cert_year.x, cert_year.y,
            cropcat_veg.x, cropcat_veg.y,
            cropcat_field.x, cropcat_field.y, cropcat_forage.x, cropcat_forage.y)) %>% 
  unique()
# Then we remove Email
ppltn_p_nop_emails <- filter(ppltn_p_nop, Email == "")

# Then we merge on email, so we stack on those that have the same address
p_co_street <- full_join(select(filter(ppltn_p_nop, Email == "" & physical_street != ""), 
                                physical_street, yrs_ag, cert_year, Region, 
                                       cropcat_veg, cropcat_field, cropcat_forage), 
                        p_co_left, by = c("physical_street", "Region")) %>% 
  mutate(yrs_ag = ifelse(is.na(.$yrs_ag.x) & !is.na(.$yrs_ag.y),
                         .$yrs_ag.y, .$yrs_ag.x),
         cert_year = ifelse(is.na(.$cert_year.x) & !is.na(.$cert_year.y),
                            .$cert_year.y, .$cert_year.x),
         cropcat_veg = ifelse(is.na(.$cropcat_veg.y),
                              .$cropcat_veg.x, .$cropcat_veg.y),
         cropcat_field = ifelse(is.na(.$cropcat_field.y),
                                .$cropcat_field.x, .$cropcat_field.y),
         cropcat_forage = ifelse(is.na(.$cropcat_forage.y),
                                 .$cropcat_forage.x, .$cropcat_forage.y)) %>% 
  select(-c(yrs_ag.x, yrs_ag.y, cert_year.x, cert_year.y,
            cropcat_veg.x, cropcat_veg.y, cropcat_field.x, 
            cropcat_field.y, cropcat_forage.x, cropcat_forage.y)) %>%
  unique()
p_co <- rbind(p_co_left, p_co_email, p_co_street) %>%  unique()


## Supply chain only by region----
supplychain_regiononly <- p_co %>% 
  group_by(Region) %>% 
  summarize(sum_rev_dnb = sum(sales, na.rm =T),
            ndnbcompanies = sum(!is.na(dnblisting)),
            median_rev_survey = median(income20_f, na.rm = T),
            mean_rev_survey = mean(income20_f, na.rm = T)) %>% 
  filter(Region != "International") 

supplychain_regiononly <- p_co %>% 
  group_by(Region) %>% 
  count() %>% 
  rename(nsupplychainactors = n) %>% 
  right_join(supplychain_regiononly)

supplychain_regiononly <- nop %>% 
  group_by(Region) %>% 
  count() %>% 
  rename(ngrowers = n) %>% 
  right_join(supplychain_regiononly) %>% 
  mutate(ratio = ngrowers/nsupplychainactors)

# Lengthening data by year ---- 
## Do this before crop because we want to still keep NOP and dnb's data on companies that we don't have crop data for
p_co_year <- p_co %>% 
  select(ResponseId, Operation, income20_c, Region, variety_n:income20_f) %>% 
  mutate(years0408 = case_when(
    is.na(dnblisting) & yrs_ag >= 12 ~ "2004-2008",
    dnblisting == T & yrs_ag >= 14 ~ "2004-2008",
    T ~ NA_character_ )) %>% 
  mutate(years0913 = case_when(
    is.na(dnblisting) & yrs_ag >= 7 ~ "2009-2013",
    dnblisting == T & yrs_ag >= 9 ~ "2009-2013",
    T ~ NA_character_ )) %>% 
  mutate(years14on = case_when(
    is.na(dnblisting) & yrs_ag >= 0 ~ "2014-2018",
    dnblisting == T & yrs_ag >= 0 ~ "2014-2018",
    T ~ NA_character_
  )) %>% 
  pivot_longer(cols = years0408:years14on, 
               names_to = "title", values_to = "year_range") %>% 
  select(-title) %>% 
  filter(!is.na(year_range)) # removing these because there are NAs for ones that are only in one section

nop_year <- nop %>% 
  mutate(years0408 = case_when(
    yrs_ag >= 12 ~ "2004-2008",
    T ~ NA_character_ )) %>% 
  mutate(years0913 = case_when(
    yrs_ag >= 7 ~ "2009-2013",
    T ~ NA_character_ )) %>% 
  mutate(years14on = case_when(
    yrs_ag >= 0 ~ "2014-2018",
    T ~ NA_character_
  )) %>% 
  pivot_longer(cols = years0408:years14on, 
               names_to = "title", values_to = "year_range") %>% 
  select(-title) %>% 
  filter(!is.na(year_range))

## Supply chain only by year ----
supplychain_yearonly <- p_co_year %>% 
  group_by(year_range) %>% 
  summarize(
    sum_rev_dnb = sum(sales, na.rm =T),
    ndnbcompanies = sum(!is.na(dnblisting)),
    median_rev_survey = median(income20_f, na.rm = T),
    mean_rev_survey = mean(income20_f, na.rm = T)) %>% 
  mutate(scaled_sum_rev_dnb = sum_rev_dnb-.$sum_rev_dnb[1])
 
supplychain_yearonly <- p_co_year %>% 
  group_by(year_range) %>% 
  count() %>% 
  rename(nsupplychainactors = n) %>% 
  right_join(supplychain_yearonly)

supplychain_yearonly <- nop_year %>% 
  group_by(year_range) %>% 
  count() %>% 
  rename(ngrowers = n) %>% 
  right_join(supplychain_yearonly) %>% 
  mutate(ratio = ngrowers/nsupplychainactors)

## Supply chain by year and region ----
supplychain_year_region <- p_co_year %>% 
  filter(Region != "International") %>% 
  group_by(year_range, Region) %>% 
  summarize(
    sum_rev_dnb = sum(sales, na.rm =T),
    ndnbcompanies = sum(!is.na(dnblisting)),
    median_rev_survey = median(income20_f, na.rm = T),
    mean_rev_survey = mean(income20_f, na.rm = T)) 

supplychain_year_region <- p_co_year %>% 
  group_by(year_range, Region) %>% 
  count() %>% 
  rename(nsupplychainactors = n) %>% 
  right_join(supplychain_year_region)

supplychain_year_region <- nop_year %>% 
  group_by(year_range, Region) %>% 
  count() %>% 
  rename(ngrowers = n) %>% 
  right_join(supplychain_year_region) %>% 
  mutate(ratio = ngrowers/nsupplychainactors)

# Lengthening data by crop ---- 
## Once we do this we lose dnb
p_co_crop <- p_co %>% 
  pivot_longer(cols = c(cropcat_field, cropcat_forage, cropcat_veg), #cropcat_other
               names_to = "maincrop", values_to = "logical") %>% 
  filter(logical == T) %>% 
  mutate(maincrop = case_when(
    maincrop == "cropcat_field" ~ "Field",
    maincrop == "cropcat_forage" ~ "Forage",
    maincrop == "cropcat_veg" ~ "Vegetables",
    #maincrop == "cropcat_other" ~ "Other"
  )) %>% 
  select(-cropcat_orn, -cropcat_herbs, -cropcat_propogules, 
         -cropcat_grain, -logical) %>% 
  unique()

nop_crop <- nop %>% 
  pivot_longer(cols = c(cropcat_field, cropcat_forage, cropcat_veg),
               names_to = "maincrop", values_to = "logical") %>% 
  filter(logical == T) %>% 
  mutate(maincrop = case_when(
    maincrop == "cropcat_field" ~ "Field",
    maincrop == "cropcat_forage" ~ "Forage",
    maincrop == "cropcat_veg" ~ "Vegetables",
  )) %>% 
  unique()

## Supply chain by crop ----

supplychain_croponly <- p_co_crop %>% 
  filter(Region != "International") %>% 
  group_by(maincrop) %>% 
  summarize(
    #sum_rev_dnb = sum(sales, na.rm =T),
    median_rev_survey = median(income20_f, na.rm = T),
    mean_rev_survey = mean(income20_f, na.rm = T)) 

supplychain_croponly <- p_co_crop %>% 
  filter(Region != "International") %>% 
  group_by(maincrop) %>% 
  count() %>% 
  rename(nsupplychainactors = n) %>% 
  left_join(supplychain_croponly)

supplychain_croponly <- nop_crop %>% 
  group_by(Region, maincrop) %>% 
  count() %>% 
  rename(ngrowers = n) %>% 
  right_join(supplychain_croponly) %>% 
  mutate(ratio = ngrowers/nsupplychainactors)

## Supply chain by region and crop ----
supplychain_crop_region <- p_co_crop %>% 
  filter(Region != "International") %>% 
  group_by(Region, maincrop) %>% 
  summarize(
    #sum_rev_dnb = sum(sales, na.rm =T),
    median_rev_survey = median(income20_f, na.rm = T),
    mean_rev_survey = mean(income20_f, na.rm = T)) 

supplychain_crop_region <- p_co_crop %>% 
  group_by(Region, maincrop) %>% 
  count() %>% 
  rename(nsupplychainactors = n) %>% 
  right_join(supplychain_crop_region)

supplychain_crop_region <- nop_crop %>% 
  group_by(Region, maincrop) %>% 
  count() %>% 
  rename(ngrowers = n) %>% 
  right_join(supplychain_crop_region) %>% 
  mutate(ratio = ngrowers/nsupplychainactors)

# Lengthen by crop and year ----
 p_co_crop_year <- p_co_crop %>% 
  select(ResponseId, Operation, income20_c, 
         Region, variety_n:maincrop) %>% 
  mutate(years0408 = case_when(
    yrs_ag >= 12 ~ "2004-2008",
    T ~ NA_character_ )) %>% 
  mutate(years0913 = case_when(
    yrs_ag >= 7 ~ "2009-2013",
    T ~ NA_character_ )) %>% 
  mutate(years14on = case_when(
    yrs_ag >= 0 ~ "2014-2018",
    T ~ NA_character_
  )) %>% 
  pivot_longer(cols = years0408:years14on, 
               names_to = "title", values_to = "year_range") %>% 
  select(-title) %>% 
  filter(!is.na(year_range))

nop_crop_year <- nop_crop %>% 
  mutate(years0408 = case_when(
    yrs_ag >= 12 ~ "2004-2008",
    T ~ NA_character_ )) %>% 
  mutate(years0913 = case_when(
    yrs_ag >= 7 ~ "2009-2013",
    T ~ NA_character_ )) %>% 
  mutate(years14on = case_when(
    yrs_ag >= 0 ~ "2014-2018",
    T ~ NA_character_
  )) %>% 
  pivot_longer(cols = years0408:years14on, 
               names_to = "title", values_to = "year_range") %>% 
  select(-title) %>% 
  filter(!is.na(year_range))

## Supply chain by year and crop ----
supplychain_year_crop <- p_co_crop_year %>% 
  filter(Region != "International") %>% 
  group_by(year_range, maincrop) %>% 
  summarize(#sum_rev_dnb = sum(sales, na.rm = T),
    median_rev_survey = median(income20_f, na.rm = T),
    mean_rev_survey = mean(income20_f, na.rm = T)) 

supplychain_year_crop <- p_co_crop_year %>% 
  ungroup() %>% 
  filter(Region != "International") %>% 
  filter(!is.na(year_range)) %>% 
  group_by(year_range, maincrop) %>% 
  count() %>% 
  rename(nsupplychainactors = n) %>% 
  left_join(supplychain_year_crop)

supplychain_year_crop <- nop_crop_year %>% 
  filter(Region != "International") %>% 
  filter(!is.na(year_range)) %>% 
  group_by(year_range, maincrop) %>% 
  count() %>% 
  rename(ngrowers = n) %>% 
  right_join(supplychain_year_crop) %>% 
  mutate(ratio = ngrowers/nsupplychainactors) %>% 
  filter(maincrop != "Other") 

## Supply chain by year, region, and crop ----
supplychain_year_region_crop <- p_co_crop_year %>% 
  filter(Region != "International") %>% 
  group_by(year_range, Region, maincrop) %>% 
  summarize(#sum_rev_dnb = sum(sales, na.rm = T),
            median_rev_survey = median(income20_f, na.rm = T),
            mean_rev_survey = mean(income20_f, na.rm = T)) 

supplychain_year_region_crop <- p_co_crop_year %>% 
  ungroup() %>% 
  filter(Region != "International") %>% 
  filter(!is.na(year_range)) %>% 
  group_by(year_range, Region, maincrop) %>% 
  count() %>% 
  rename(nsupplychainactors = n) %>% 
  left_join(supplychain_year_region_crop)

supplychain_year_region_crop <- nop_crop_year %>% 
  filter(Region != "International") %>% 
  filter(!is.na(year_range)) %>% 
  group_by(year_range, Region, maincrop) %>% 
  count() %>% 
  rename(ngrowers = n) %>% 
  right_join(supplychain_year_region_crop) %>% 
  mutate(ratio = ngrowers/nsupplychainactors) %>% 
  filter(maincrop != "Other") 


# ---- COMPLETE REGIONAL SUMMARY ----
## Create and save regional summary ----
#ratios <- select(supplychain_year_region_crop, year_range:nsupplychainactors)
ratios <- select(supplychain_year_region_crop, year_range:nsupplychainactors) # Number of actors by year and crop and region
sales <- select(supplychain_yearonly, year_range, sum_rev_dnb) # Sales by years
regional_summary <- full_join(ratios, sales) %>% full_join(funds_year_region_crop) %>% 
  filter(Region != "Multi-region" & Region != "Outside US") 
# Not using crop level because I want to use the dnb revenue data and it isn't available for
survey_respondents <- readRDS("~/Box/organicseed/df.rds") %>% 
  filter(Region != "International", !is.na(Region), maincrop != "Unknown") %>% 
  group_by(Region, year_range) %>% 
  count() %>% 
  rename(nrespondents = n)

## What about the producer data we have ----
regional_summary <- left_join(regional_summary, survey_respondents)

## Saving data ----
write.csv(regional_summary, "~/Box/organicseed/regional_data.csv", row.names = F)

## Tables 1b and 1c ----
write.csv(funds_yearonly, "~/Documents/Davis/R-Projects/organicseed/tables/table1b.csv", row.names = F)
write.csv(supplychain_yearonly, "~/Documents/Davis/R-Projects/organicseed/tables/table1c.csv", row.names = F)


# ---- COMBINE SURVEY WITH REGIONAL SUMMARIES ----
## join df and regional summary ----
df <- readRDS("~/Box/organicseed/df.rds")
df.long <- readRDS("~/Box/organicseed/df_long.rds")
## This what was I originially used, but regional summary now has crop in it and nsupplychain is now across years. 
df_simple <- left_join(df, regional_summary)
# Need to make these the same so that I match on the OG crop and not the main
df.long$maincrop <- df.long$OGcrop
table(df.long$maincrop)
df.long_simple <- left_join(df.long, regional_summary) 

saveRDS(df_simple, "~/Box/organicseed/df_onelevel.rds")
saveRDS(df.long_simple, "~/Box/organicseed/df_long_onelevel.rds")

