# Scrape farm size based on state profile
library(tesseract)
library(rvest)
library(xml2)
# https://www.nass.usda.gov/Publications/AgCensus/2017/Online_Resources/County_Profiles/

# 1. Scrape state profile PDFs ----
url <- "https://www.nass.usda.gov/Publications/AgCensus/2017/Online_Resources/County_Profiles/"
page <- read_html(url)
states <- xml_find_all(page, '//*[(@id = "field_body")]//*[contains(concat( " ", @class, " " ), concat( " ", "listStyleNone", " " ))]//*[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]') %>% 
  html_text() %>% 
  unique()

states <- states[states != "Puerto Rico"]

dir.create('ocr_pdfs')
setwd('ocr_pdfs')

farm_size_cat <- c("1-9", "10-49", "50-179",
                   "180-499", "500-999", "1000+")

acreage_df <- data.frame()
for(i in 1:length(states)){
  state <- str_replace_all(states[i], ' ', '_')
  state_url <- paste0("https://www.nass.usda.gov/Publications/AgCensus/2017/Online_Resources/County_Profiles/", state, "/index.php")
  if(state == "South_Carolina"){
    state_page <- "cp99045.pdf"
  } else if(state == "West_Virginia") {
    state_page <- "cp99054.pdf"
    } else {
    state_page <- read_html(state_url) %>% 
      xml_find_all('//*[(@id = "field_body")]//p//a') %>% 
      html_attr("href")
  }
  
  report_url <- paste0("https://www.nass.usda.gov/Publications/AgCensus/2017/Online_Resources/County_Profiles/", state, "/", state_page)
  
  pdf <- tryCatch(ocr(report_url),
                  error = function(e) NULL)
  og <- str_locate(pdf, "organically")[2,2]
  og_area <- str_sub(pdf, og-75, og+75)[2]
  perc_farm_og <- ifelse(str_detect(og_area, "organically \\(Z\\)"), 0, 
         as.numeric(str_extract(og_area, "(?<=Farm )[0-9]+")))
  
  
  end <- str_locate(pdf, "Farms by Size")[1,2]
  general_table_area <- str_sub(pdf, end, end + 400)[1]
  
  n1 <- as.numeric(str_remove(str_extract(general_table_area, 
                                   "(?<=1 to 9 acres )[0-9,]+"), ","))
  n2 <- as.numeric(str_remove(str_extract(general_table_area, 
                                     "(?<=10 to 49 acres )[0-9,]+"), ","))
  n3 <- as.numeric(str_remove(str_extract(general_table_area, 
                                      "(?<=50 to 179 acres )[0-9,]+"), ","))
  n4 <- as.numeric(str_remove(str_extract(general_table_area, 
                                      "(?<=180 to 499 acres )[0-9,]+"), ","))
  n5 <- as.numeric(str_remove(str_extract(general_table_area, 
                                       "(?<=500 to 999 acres )[0-9,]+"), ","))
  n6 <- as.numeric(str_remove(str_extract(general_table_area, 
                                    "(?<=1,000 \\+ acres )[0-9,]+"), ","))
  
  n <- c(n1, n2, n3, n4, n5, n6)
  df <- data.frame("state" = states[i], 
                   farm_size_cat, n, perc_farm_og)
  acreage_df <- rbind(acreage_df, df)
  # Get rid of these when it is over
  file.remove(list.files('../ocr_pdfs/'), full.names = T)
}

# 2. Summarize USDA farm size distribution by region ----
unique(acreage_df$state)

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

acreage_df <- acreage_df %>% 
  mutate(region = case_when(
  state %in% western ~ "West",
  state %in% northcentral ~ "North Central",
  state %in% southern ~ "South",
  state %in% northeast ~ "Northeast",
  T ~ NA
)) %>% 
  group_by(state) %>% 
  mutate(state_total = sum(n),
         n_og_farms = round((perc_farm_og/100)*state_total)) %>% 
  ungroup()

## Percentage of farms that are organic in each region
acreage_df %>% 
  select(region, state_total, n_og_farms) %>% 
  unique() %>% 
  group_by(region) %>% 
  summarize(region_total = sum(state_total),
         region_og_total = sum(n_og_farms),
         prop_og_reg = round(100*(region_og_total/region_total), 2))
  
acreage_table <- acreage_df %>% 
  group_by(region, farm_size_cat) %>% 
  summarize(regional_cat_total = sum(n)) %>% 
  ungroup() %>% 
  group_by(region) %>% 
  mutate(regional_total = sum(regional_cat_total),
         prop = regional_cat_total/regional_total) 


size_lvl <- c("1-9", "10-49", "50-179", "180-499","500-999", "1000+")
acreage_table %>% 
  ggplot(aes(x = factor(farm_size_cat, size_lvl), 
             y = prop, fill = region)) +
  geom_col(position = "dodge")


# 3. Read in data just like for data summary ----
library(dplyr)
library(stringr)
# This is the dataframe where I have identified main crops of growers, so each observation is only one grower
df <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds") %>% 
  mutate(maincrop_acrage = case_when(
    maincrop == "Field" ~ FieldAcreage,
    maincrop == "Forage" ~ ForageAcreage,
    maincrop == "Vegetables" ~ VegiAcreage,
    T ~ NA
  )) %>% 
  filter(Region != "International", maincrop != "Unknown") %>% 
  select(maincrop, OGseed_percent_maincrop, IncreasedOGSeed,
         #VegiAcreage, FieldAcreage, ForageAcreage, # Total acres for each crop
         #OGVegiSeed, OGFieldSeed, OGForageSeed, # % organic seed used
         TotalAcreage, maincrop_acrage,
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
                  maincrop_acrage,
                  noncommercial_seedsource,
                  #YearsCertified
                  barrier_buyerReq,
                  barrier_insuffQuantity,
                  barrier_lackdesiredTraits,
                  barrier_availability, data_coll_method) %>% 
  na.omit() %>% 
  mutate(survey_year = case_when(
    survey_year == 2011 ~ 2010,
    survey_year == 2016 ~ 2015,
    survey_year == 2021 ~ 2020,
    T ~ survey_year
  ))

df.full <- df.full %>% 
  mutate(farm_size_cat = case_when(
    TotalAcreage <= 9 ~ "1-9",
    TotalAcreage > 9 & TotalAcreage <= 49 ~ "10-49",
    TotalAcreage > 49 & TotalAcreage <= 179~ "50-179",
    TotalAcreage > 179 & TotalAcreage <= 499~ "180-499",
    TotalAcreage > 499 & TotalAcreage <= 999~ "500-999",
    TotalAcreage > 999~ "1000+",
    T ~ NA
  ))
table(is.na(df.full$farm_size_cat))
size_lvl <- c("1-9", "10-49", "50-179", "180-499","500-999", "1000+")

df_acreage_table <- df.full %>% 
  filter(survey_year != 2010) %>% 
  group_by(Region, farm_size_cat) %>% 
  count() %>% 
  rename(regional_cat_total = n) %>% 
  ungroup() %>% 
  group_by(Region) %>% 
  mutate(regional_total = sum(regional_cat_total),
         prop = regional_cat_total/regional_total)

df_acreage_table %>% 
  ggplot(aes(x = factor(farm_size_cat, size_lvl), 
             y = prop, fill = Region)) +
  geom_col(position = "dodge")

# 4. Combine USDA + survey data ----
colnames(df_acreage_table) <- tolower(colnames(df_acreage_table))

acreage_table$source = "USDA 2017 Census State Profiles (Conventional & Organic)"
df_acreage_table$source <- "Organic survey sample 2015 & 2020"

acreage_table_comb <- rbind(acreage_table, df_acreage_table)

acreage_table_comb %>% 
  ggplot(aes(x = factor(farm_size_cat, size_lvl), 
             y = prop, fill = source)) +
  geom_col(position = "dodge") +
  facet_grid(~region) +
  theme_minimal() +
  scale_fill_viridis_d(begin = 0.2, end = 0.6) +
  labs(y = "Proportion of operations", x = "Acreage category",
       fill = "") +
  theme(legend.position = "bottom",
        axis.text.x=element_text(angle=45, hjust=1))

ggsave('~/Documents/Davis/R-Projects/organicseed_adoption/figures/figurea3.png',
       width = 8, height = 4)
