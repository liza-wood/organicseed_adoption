## OG Farm size distribution
## Have we seen the bifurcation of conventional and organic?
library(stringr)

# I DON'T USE THIS!!

## NOP data ----
nop <- read.csv("~/Documents/Davis/R-Projects/big-organic-bifurcation/data/df.csv")
nop$cert_year <- lubridate::year(lubridate::mdy(nop$Effective.Date.of.CROPS.Status))

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

nop <- dplyr::select(nop, cert_year, acres, Mailing.Address..State.Province)
colnames(nop)[3] <- "state"

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
nop <- nop %>% 
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
  mutate(farm_size_cat_med = case_when(
    farm_size_cat %in% farm_size_cat_l[10:12] ~ ">499",
    farm_size_cat %in% farm_size_cat_l[7:9] ~ "180-499",
    farm_size_cat %in% farm_size_cat_l[3:6] ~ "50-179",
    farm_size_cat %in% farm_size_cat_l[1:2] ~ "1-49",
    T ~ NA
  )) %>% 
  mutate(farm_size_cat_broad = case_when(
    farm_size_cat %in% farm_size_cat_l[10:12] ~ ">499",
    farm_size_cat %in% farm_size_cat_l[3:9] ~ "50-499",
    farm_size_cat %in% farm_size_cat_l[1:2] ~ "1-49",
    T ~ NA
  )) %>% 
  mutate(Region = case_when(
    state %in% western ~ "West",
    state %in% northcentral ~ "North Central",
    state %in% southern ~ "South",
    state %in% northeast ~ "Northeast",
    T ~ NA
  ))

med_lvl <- c("1-49","50-179","180-499",">499")
brd_lvl <- c("1-49","50-499",">499")

# Distribution
nop_summ1 <- nop %>% 
  filter(cert_year > 2002 & cert_year <= 2022) %>% 
  group_by(farm_size_cat_med) %>%  # region
  count() %>% 
  ungroup() %>% 
  #group_by(region) %>% 
  mutate(prop= round((n/sum(n)), 2))

  nop_summ1 %>% 
  ggplot(aes(x = factor(farm_size_cat_med, med_lvl), 
             y = prop)) +
  geom_col() +
  labs(fill = "", x = "Farm size", y = "Proportion of organic operations") +
  theme_minimal() 

# Distribution by certification year 
nop_summ2 <- nop %>% 
  filter(cert_year > 2002 & cert_year < 2020) %>% 
  group_by(cert_year, farm_size_cat_med) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(cert_year) %>% 
  mutate(prop = n/sum(n)) 

nop_summ2 %>% 
  ggplot(aes(x = cert_year, y = prop, fill = factor(farm_size_cat_med,
                                                    med_lvl))) +
  geom_area(alpha=0.8 , size=.5, colour="black") +
  labs(fill = "", x = "", y = "Proportion of organic operations") +
  theme_minimal() +
  scale_fill_viridis_d() 

# Distributions of acreage
nop_summ3 <- nop %>% 
  filter(cert_year > 2002 & cert_year < 2020) %>% 
  group_by(cert_year) %>% 
  mutate(total_acres = sum(acres)) %>% 
  ungroup() %>% 
  group_by(cert_year, farm_size_cat_med) %>% 
  mutate(total_acres_percat = sum(acres),
         prop = total_acres_percat/total_acres)

nop_summ3 %>% 
  ggplot(aes(x = cert_year, y = prop, fill = factor(farm_size_cat_med, 
                                                    med_lvl))) +
  geom_area(alpha=0.8 , size=.5, colour="black") +
  labs(fill = "", x = "", y = "Proportion of organic acreage") +
  theme_minimal() +
  scale_fill_viridis_d() 

# Survey data ----
df <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds") %>% 
  filter(Region != "International", maincrop != "Unknown") %>% 
  mutate(cert_year = case_when(
    survey_year == 2011 ~ 2011 - YearsCertified,
    survey_year == 2016 ~ 2016 - YearsCertified,
    survey_year == 2021 ~ 2021 - YearsCertified,
    T ~ NA
  ),
  cert_year = round(cert_year)+1) %>% 
  mutate(survey_year = case_when(
    survey_year == 2011 ~ 2010,
    survey_year == 2016 ~ 2015,
    survey_year == 2021 ~ 2020,
    T ~ survey_year
  )) 
table(df$cert_year)

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
                  barrier_availability, cert_year) %>% 
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
  mutate(farm_size_cat_med = case_when(
    farm_size_cat %in% farm_size_cat_l[10:12] ~ ">499",
    farm_size_cat %in% farm_size_cat_l[7:9] ~ "180-499",
    farm_size_cat %in% farm_size_cat_l[3:6] ~ "50-179",
    farm_size_cat %in% farm_size_cat_l[1:2] ~ "1-49",
    T ~ NA
  )) %>% 
  mutate(farm_size_cat_broad = case_when(
    farm_size_cat %in% farm_size_cat_l[10:12] ~ ">499",
    farm_size_cat %in% farm_size_cat_l[3:9] ~ "45-499",
    farm_size_cat %in% farm_size_cat_l[1:2] ~ "1-49",
    T ~ NA
  )) 

# Distribution
df_summ1 <- df.full %>% 
  filter(cert_year > 2002 & cert_year <= 2022) %>% 
  group_by(farm_size_cat_med) %>%  # region
  count() %>% 
  ungroup() %>% 
  #group_by(region) %>% 
  mutate(prop= round((n/sum(n)), 2)) 
  
df_summ1 %>% 
  ggplot(aes(x = factor(farm_size_cat_med, med_lvl), 
             y = prop)) +
  geom_col() +
  labs(fill = "", x = "Farm size", y = "Proportion of organic operations") +
  theme_minimal() 

# Distribution by certification year 
df_summ2 <- df.full %>% 
  filter(cert_year > 2002 & cert_year < 2020) %>% 
  group_by(cert_year, farm_size_cat_med) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(cert_year) %>% 
  mutate(prop = n/sum(n)) 

df_summ2 %>% 
  ggplot(aes(x = cert_year, y = prop, fill = factor(farm_size_cat_med,
                                                    med_lvl))) +
  geom_area(alpha=0.8 , size=.5, colour="black") +
  labs(fill = "", x = "", y = "Proportion of organic operations") +
  theme_minimal() +
  scale_fill_viridis_d() 

# Distributions of acreage
df_summ3 <- df.full %>% 
  filter(cert_year > 2002 & cert_year < 2020) %>% 
  group_by(cert_year) %>% 
  mutate(total_acres = sum(TotalAcreage)) %>% 
  ungroup() %>% 
  group_by(cert_year, farm_size_cat_med) %>% 
  mutate(total_acres_percat = sum(TotalAcreage),
         prop = total_acres_percat/total_acres)
df_summ3 %>% 
  ggplot(aes(x = cert_year, y = prop, fill = factor(farm_size_cat_med, 
                                                    med_lvl))) +
  geom_area(alpha=0.8 , size=.5, colour="black") +
  labs(fill = "", x = "", y = "Proportion of organic acreage") +
  theme_minimal() +
  scale_fill_viridis_d() 

# FIGURE 1. Put them together ----
## Year only
df_summ4 <- df.full %>% 
  group_by(survey_year, farm_size_cat_med) %>%  # region
  count() %>% 
  ungroup() %>% 
  group_by(survey_year) %>% 
  mutate(prop= round((n/sum(n)), 2)) %>% 
  mutate(source = "Survey sample")

nop_summ4 <- nop %>% 
  mutate(survey_year = case_when(
    cert_year <= 2011 ~ 2010,
    cert_year <= 2016 ~ 2015,
    cert_year <= 2021 ~ 2020,
    T ~ NA
  )) %>% 
  filter(!is.na(survey_year)) %>% 
  group_by(survey_year, farm_size_cat_med) %>%  # region
  count() %>% 
  ungroup() %>% 
  group_by(survey_year) %>% 
  mutate(prop= round((n/sum(n)), 2)) %>% 
  mutate(source = "NOP INTEGRITY")

summ4 <- rbind(df_summ4, nop_summ4)

summ4 %>% 
  ggplot(aes(x = factor(farm_size_cat_med, med_lvl), 
             y = prop, fill = source)) +
  geom_col(position = "dodge") +
  labs(fill = "", x = "Farm size", y = "Proportion of organic operations") +
  theme_minimal() +
  facet_grid(~survey_year) +
  scale_fill_viridis_d(begin = 0.2, end = 0.8) +
  theme(legend.position = "bottom")

ggsave('figures/append_repf1_nop.png', width = 7, height = 4)

## Year and region 
df_summ5 <- df.full %>% 
  group_by(survey_year, Region, farm_size_cat_med) %>%  # region
  count() %>% 
  ungroup() %>% 
  group_by(survey_year, Region) %>% 
  mutate(prop= round((n/sum(n)), 2)) %>% 
  mutate(source = "Survey sample")

nop_summ5 <- nop %>% 
  mutate(survey_year = case_when(
    cert_year <= 2011 ~ 2010,
    cert_year <= 2016 ~ 2015,
    cert_year <= 2021 ~ 2020,
    T ~ NA
  )) %>% 
  filter(!is.na(survey_year)) %>% 
  group_by(survey_year, Region, farm_size_cat_med) %>%  
  count() %>% 
  ungroup() %>% 
  group_by(survey_year, Region) %>% 
  mutate(prop= round((n/sum(n)), 2)) %>% 
  mutate(source = "NOP INTEGRITY")

summ5 <- rbind(df_summ5, nop_summ5)

summ5 %>% 
  filter(!is.na(Region)) %>% 
  ggplot(aes(x = factor(farm_size_cat_med, med_lvl), 
             y = prop, fill = source)) +
  geom_col(position = "dodge") +
  labs(fill = "", x = "Farm size (acres)", y = "Proportion of organic operations") +
  geom_text(aes(label = prop), position = position_dodge(width = .9),
            vjust = -.2, size = 2) +
  theme_minimal() +
  facet_grid(Region~survey_year) +
  scale_fill_viridis_d(begin = 0.2, end = 0.8) +
  theme(legend.position = "bottom")

table(df.full$Region, df.full$survey_year)


