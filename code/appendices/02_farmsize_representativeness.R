## OG Farm size distribution
## Have we seen the bifurcation of conventional and organic?

og <- read.csv("~/Documents/Davis/R-Projects/big-organic-bifurcation/data/df.csv")
og$cert_year <- lubridate::year(lubridate::mdy(og$Effective.Date.of.CROPS.Status))
og <- dplyr::select(og, cert_year, acres, Mailing.Address..State.Province)
colnames(og)[3] <- "state"

farm_size_cat_l <- c("1-9", "10-49", "50-69", "70-99", "100-139", "140-179",
                   "180-219", "220-259", "260-499", "500-999", "1000-1999", "2000+")

western <- c("California", "Oregon", "Washington", "Idaho", "Nevada",
             "Arizona", "Utah", "New Mexico", "Colorado", "Wyoming", "Montana",
             "Alaska", "Hawaii")
northcentral <- c("North Dakota", "South Dakota", "Kansas",
                  "Nebraska", "Minnesota", "Iowa", "Missouri", "Wisconsin", 
                  "Illinois", "Michigan", "Indiana", "Ohio")
southern <- c("Texas", "Oklahoma", "Arkansas", "Louisiana", 
              "Kentucky", "Tennessee", "Mississippi", "Alabama", "Florida", 
              "Georgia", "South Carolina", "North Carolina", "Virginia")
northeast <- c("West Virginia", "Maryland", "Delaware", 
               "New Jersey", "Pennsylvania", "New York", "Connecticut", 
               "Rhode Island", "Massachusetts", "Vermont", "New Hampshire", 
               "Maine", "District of Columbia")

og <- og %>% 
  filter(!is.na(acres)) %>% 
  mutate(farm_size_cat = case_when(
    acres <=9 ~ farm_size_cat_l[1],
    acres > 9 & acres <= 49 ~ farm_size_cat_l[2],
    acres > 49 & acres <= 179 ~ farm_size_cat_l[3],
    acres > 179 & acres <= 499 ~ farm_size_cat_l[4],
    acres > 499 & acres <= 999 ~ farm_size_cat_l[5],
    acres > 999 ~ farm_size_cat_l[6],
    T ~ NA
  )) %>% 
  mutate(farm_size_cat_broad = case_when(
    farm_size_cat %in% farm_size_cat_l[5:6] ~ ">499",
    farm_size_cat %in% farm_size_cat_l[3:4] ~ "45-499",
    farm_size_cat %in% farm_size_cat_l[1:2] ~ "1-49",
    T ~ NA
  )) %>% 
  mutate(region = case_when(
    state %in% western ~ "West",
    state %in% northcentral ~ "North Central",
    state %in% southern ~ "South",
    state %in% northeast ~ "Northeast",
    T ~ NA
  ))

## Regional farm size dist
size_summ <- og %>% 
  filter(cert_year <= 2017) %>% 
  group_by(region, farm_size_cat) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(region) %>% 
  mutate(prop= round(100*(n/sum(n)), 2))


size_lvl <- c("1-9", "10-49", "50-179", "180-499","500-999", "1000+")
size_summ %>% 
  ggplot(aes(x = factor(farm_size_cat, size_lvl), 
             y = prop, fill = region)) +
  geom_col(position = "dodge")


## Bifurcation point
og_summ <- og %>% 
  group_by(cert_year, farm_size_cat_broad) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(cert_year) %>% 
  mutate(prop = n/sum(n))

og_summ %>% 
  filter(cert_year > 2000) %>% 
  ggplot(aes(x = cert_year, y = prop, fill = factor(farm_size_cat_broad,
                                                    rev(c("1-49", "45-499", ">499"))))) +
  geom_area(alpha=0.6 , size=.1, colour="black") +
  labs(fill = "")

og %>% 
  filter(cert_year > 2000) %>% 
  group_by(cert_year) %>% 
  mutate(total_acres = sum(acres)) %>% 
  ungroup() %>% 
  group_by(cert_year, farm_size_cat_broad) %>% 
  mutate(total_acres_percat = sum(acres),
         prop = total_acres_percat/total_acres) %>% 
  ggplot(aes(x = cert_year, y = prop, fill = factor(farm_size_cat_broad,
                                                    rev(c("1-49", "45-499", ">499"))))) +
  geom_area(alpha=0.6 , size=.1, colour="black") +
  labs(fill = "")

library(tidyUSDA)
source('code/keys.R')
# All cropland (conventional) ----
dataitems <- data.frame("dataItems" = tidyUSDA::allDataItem)

all_agland_n_operations_cat <- getQuickstat(
  key = usda_quickstats_key,
  data_item = 'AG LAND, CROPLAND - NUMBER OF OPERATIONS',
  geographic_level = 'STATE',
  sector = "ECONOMICS",
  domain = "AREA OPERATED",
  year = c('2012', '2017', '2022'),
  geometry = F,
  lower48 = F) %>% 
  select(state_name, state_alpha, year, domaincat_desc, Value) %>% 
  rename(operations_count = Value)
table(all_agland_n_operations_cat$year)


size_summ <- all_agland_n_operations_cat %>% 
  filter(year == 2017) %>% 
  mutate(region = case_when(
    state_name %in% toupper(western) ~ "West",
    state_name %in% toupper(northcentral) ~ "North Central",
    state_name %in% toupper(southern) ~ "South",
    state_name %in% toupper(northeast) ~ "Northeast",
    T ~ NA
  )) %>% 
  mutate(domaincat_desc = stringr::str_remove_all(domaincat_desc, "AREA OPERATED\\: \\(|\\)|,"),
         domaincat_desc = stringr::str_replace_all(domaincat_desc, " TO ", "-")) %>% 
  rename(farm_size_cat = domaincat_desc) %>% 
  group_by(region, farm_size_cat) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(region) %>% 
  mutate(prop= round(100*(n/sum(n)), 2))


size_lvl <- c("1-9", "10-49", "50-179", "180-499","500-999", "1000+")
size_summ %>% 
  ggplot(aes(x = factor(farm_size_cat, size_lvl), 
             y = prop, fill = region)) +
  geom_col(position = "dodge")