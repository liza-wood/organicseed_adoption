## OG Farm size distribution
## Have we seen the bifurcation of conventional and organic?
library(stringr)
og <- read.csv("~/Documents/Davis/R-Projects/big-organic-bifurcation/data/df.csv")
og$cert_year <- lubridate::year(lubridate::mdy(og$Effective.Date.of.CROPS.Status))

#types <- unique(unlist(stringr::str_extract_all(og$Certified.Products.Under.CROPS.Scope, "[A-Za-z]+\\:")))
#types <- c("nuts?", "trees?", "vegetables?", "fruits?", "herbs?", "spices?",
#           "flower", "pasture", "forage", "fungi")
#none <- which(!(str_detect(tolower(og$Certified.Products.Under.CROPS.Scope), paste(types, collapse = "|"))))
#og_none <- og[none, 'Certified.Products.Under.CROPS.Scope']
#og_none <- og_none[og_none != ""]
#unique(sort(og_none))
## other is the weird group. Perhaps I want other, unless mushroom
#
#nofruit <- c("nuts?", "trees?", "vegetables?", "herbs?", "spices?",
#           "flower", "pasture", "forage")
#no_fruit <- which(!(str_detect(og$Certified.Products.Under.CROPS.Scope, paste(nofruit, collapse = "|"))))
#og_nofruit <- og$Certified.Products.Under.CROPS.Scope[no_fruit]
#og_nofruit <- og_nofruit[og_nofruit != ""]

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

farm_size_cat_l <- c("1-9", "10-49", "50-69", "70-99", "100-139", "140-179",
                     "180-219", "220-259", "260-499", "500-999", "1000-1999", "2000+")
og <- og %>% 
  filter(!is.na(acres)) %>% 
  mutate(farm_size_cat = case_when(
    acres <=9 ~ farm_size_cat_l[1],
    acres > 9 & acres <= 49 ~ farm_size_cat_l[2],
    acres > 49 & acres <= 69 ~ farm_size_cat_l[3],
    acres > 69 & acres <= 99 ~ farm_size_cat_l[4],
    acres > 99 & acres <= 139 ~ farm_size_cat_l[5],
    acres > 139 & acres <= 179 ~ farm_size_cat_l[6],
    acres > 179 & acres <= 219 ~ farm_size_cat_l[7],
    acres > 219 & acres <= 259 ~ farm_size_cat_l[8],
    acres > 259 & acres <= 499 ~ farm_size_cat_l[9],
    acres > 499 & acres <= 999 ~ farm_size_cat_l[10],
    acres > 999 & acres <= 1999 ~ farm_size_cat_l[11],
    acres > 1999 ~ farm_size_cat_l[12],
    T ~ NA
  )) %>% 
  mutate(farm_size_cat_broad = case_when(
    farm_size_cat %in% farm_size_cat_l[10:12] ~ ">499",
    farm_size_cat %in% farm_size_cat_l[3:9] ~ "45-499",
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
table(og$farm_size_cat_broad)

size_lvl <- rev(c("1-49", "45-499", ">499"))
og %>% 
  group_by(region, farm_size_cat) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(region) %>% 
  mutate(prop= round(100*(n/sum(n)), 2)) %>% 
  ggplot(aes(x = factor(farm_size_cat, farm_size_cat_l), 
             y = prop, fill = region)) +
  geom_col(position = "dodge")

og %>% 
  group_by(region, farm_size_cat_broad) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(region) %>% 
  mutate(prop= round(100*(n/sum(n)), 2)) %>% 
  ggplot(aes(x = factor(farm_size_cat, farm_size_cat_l), 
             y = prop, fill = region)) +
  geom_col(position = "dodge")

## By year ----
## farm size dist
size_summ_22 <- og %>% 
  filter(cert_year < 2022) %>% 
  group_by(region, farm_size_cat_broad) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(region) %>% 
  mutate(prop= round(100*(n/sum(n)), 2)) %>% 
  mutate(year_cut = 2022)

size_summ_17 <- og %>% 
  filter(cert_year < 2017) %>% 
  group_by(region, farm_size_cat_broad) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(region) %>% 
  mutate(prop= round(100*(n/sum(n)), 2))%>% 
  mutate(year_cut = 2017)

size_summ_12 <- og %>% 
  filter(cert_year < 2012) %>% 
  group_by(region, farm_size_cat_broad) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(region) %>% 
  mutate(prop= round(100*(n/sum(n)), 2)) %>% 
  mutate(year_cut = 2012)

size_summ_07 <- og %>% 
  filter(cert_year < 2007) %>% 
  group_by(region, farm_size_cat_broad) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(region) %>% 
  mutate(prop= round(100*(n/sum(n)), 2)) %>% 
  mutate(year_cut = 2007)

size_summ_03 <- og %>% 
  filter(cert_year < 2003) %>% 
  group_by(region, farm_size_cat_broad) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(region) %>% 
  mutate(prop= round(100*(n/sum(n)), 2)) %>% 
  mutate(year_cut = 2003)

size_summ <- rbind(size_summ_03, size_summ_07, size_summ_12, size_summ_17, size_summ_22)


size_lvl <- rev(c("1-49", "45-499", ">499"))
size_summ %>% 
  ggplot(aes(x = factor(farm_size_cat_broad, size_lvl), 
             y = prop, fill = region)) +
  geom_col(position = "dodge") +
  facet_grid(~year_cut)


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


# Survey data ----
df <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds") %>% 
  filter(Region != "International", maincrop != "Unknown") %>% 
  mutate(survey_year = case_when(
    survey_year == 2011 ~ 2010,
    survey_year == 2016 ~ 2015,
    survey_year == 2021 ~ 2020,
    T ~ survey_year
  ))

df.full <- select(df, OGseed_percent_maincrop,
                  OGseed_percent_maincrop, 
                  OGSeedOGIntegrityn,
                  CertifierRequestn,
                  maincrop, survey_year, Region, 
                  TotalAcreage, cropcat_n, 
                  noncommercial_seedsource,
                  #YearsCertified
                  barrier_buyerReq,
                  barrier_insuffQuantity,
                  barrier_lackdesiredTraits,
                  barrier_availability, data_coll_method) %>% 
  na.omit() 

df.full <- df.full %>% 
  mutate(farm_size_cat = case_when(
    TotalAcreage <=9 ~ farm_size_cat_l[1],
    TotalAcreage > 9 & TotalAcreage <= 49 ~ farm_size_cat_l[2],
    TotalAcreage > 49 & TotalAcreage <= 69 ~ farm_size_cat_l[3],
    TotalAcreage > 69 & TotalAcreage <= 99 ~ farm_size_cat_l[4],
    TotalAcreage > 99 & TotalAcreage <= 139 ~ farm_size_cat_l[5],
    TotalAcreage > 139 & TotalAcreage <= 179 ~ farm_size_cat_l[6],
    TotalAcreage > 179 & TotalAcreage <= 219 ~ farm_size_cat_l[7],
    TotalAcreage > 219 & TotalAcreage <= 259 ~ farm_size_cat_l[8],
    TotalAcreage > 259 & TotalAcreage <= 499 ~ farm_size_cat_l[9],
    TotalAcreage > 499 & TotalAcreage <= 999 ~ farm_size_cat_l[10],
    TotalAcreage > 999 & TotalAcreage <= 1999 ~ farm_size_cat_l[11],
    TotalAcreage > 1999 ~ farm_size_cat_l[12],
    T ~ NA
  )) %>% 
  mutate(farm_size_cat_broad = case_when(
    farm_size_cat %in% farm_size_cat_l[10:12] ~ ">499",
    farm_size_cat %in% farm_size_cat_l[3:9] ~ "45-499",
    farm_size_cat %in% farm_size_cat_l[1:2] ~ "1-49",
    T ~ NA
  )) 


df_acreage_table <- df.full %>% 
  filter(survey_year == 2020) %>% 
  group_by(Region, farm_size_cat) %>% 
  count() %>% 
  rename(regional_cat_total = n) %>% 
  ungroup() %>% 
  group_by(Region) %>% 
  mutate(regional_total = sum(regional_cat_total),
         prop = regional_cat_total/regional_total)

df_acreage_table %>% 
  ggplot(aes(x = factor(farm_size_cat, farm_size_cat_l), 
             y = prop, fill = Region)) +
  geom_col(position = "dodge")
