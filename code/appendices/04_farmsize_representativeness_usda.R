library(dplyr)
library(ggplot2)
# OG Farm size distribution
# Data from the USDA census tabulations, sent from Mallory NASS
#devtools::install_github("liza-wood/usdaoRganic")
library(usdaoRganic)
data(organic_census)
usda <- organic_census
usda$year[usda$year == 2012] <- 2010
usda$year[usda$year == 2017] <- 2015
usda$year[usda$year == 2022] <- 2020

df.full <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled.rds") %>% 
  select(OGseed_percent_maincrop_prop,
         OG_value_s,
         barrier_all_rev_s,
         CertifierRequestn,
         noncommercial_seedsource_s,
         TotalAcreage,
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


acres_cat7 <- c("1 to 9", "10 to 49", "50 to 179", "180 to 499",
                "500 to 999", "1,000 to 1,999", "2,000 +")

df.full <- df.full %>% 
  mutate(acreage_range = case_when(
    TotalAcreage <10 ~ acres_cat7[1],
    TotalAcreage >= 10 & TotalAcreage < 50 ~ acres_cat7[2],
    TotalAcreage >= 50 & TotalAcreage < 180 ~ acres_cat7[3],
    TotalAcreage >= 180 & TotalAcreage < 500~ acres_cat7[4],
    TotalAcreage >= 500 & TotalAcreage < 1000 ~ acres_cat7[5],
    TotalAcreage >= 1000 & TotalAcreage < 2000 ~ acres_cat7[6],
    TotalAcreage >= 2000 ~ acres_cat7[7],
    T ~ NA
  ))

# FIGURE 1. Put them together ----
## Year only
df_summ4 <- df.full %>% 
  group_by(survey_year, acreage_range) %>%  # region
  count() %>% 
  ungroup() %>% 
  group_by(survey_year) %>% 
  mutate(prop= round((n/sum(n)), 2)) %>% 
  mutate(source = "Survey sample")

usda_summ4 <- usda %>% 
  rename(survey_year = year) %>% 
  filter(!is.na(survey_year)) %>% 
  group_by(survey_year, acreage_range) %>%  # region
  summarize(n = sum(organic_ops_count)) %>% 
  ungroup() %>% 
  group_by(survey_year) %>% 
  mutate(prop= round((n/sum(n)), 2)) %>% 
  mutate(source = "USDA Census")

summ4 <- rbind(df_summ4, usda_summ4)

summ4 %>% 
  mutate(year = case_when(
    survey_year == 2010 ~ "Survey: 2010 | Census: 2012",
    survey_year == 2015 ~ "Survey: 2015 | Census: 2017",
    survey_year == 2020 ~ "Survey: 2020 | Census: 2022"
  )) %>% 
  ggplot(aes(x = factor(acreage_range, acres_cat7), 
             y = prop, fill = source)) +
  geom_col(position = position_dodge(preserve = "single")) +
  labs(fill = "", x = "Farm size (acres)", y = "Proportion of operations") +
  facet_grid(~year) +
  scale_fill_grey(start = 0.4, end = 0.8) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45,vjust = 1, hjust=1))

ggsave('figures/append_repf1_usda.png', width = 7, height = 4)

# Region too -----

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

usda <- usda %>% 
  mutate(Region = case_when(
    state %in% western ~ "West",
    state %in% northcentral ~ 'North Central',
    state %in% southern ~ "South",
    state %in% northeast ~ "Northeast",
    T ~ NA
  ))


# FIGURE 1. Put them together ----
## Year only
df_summ5 <- df.full %>% 
  group_by(survey_year, Region, acreage_range) %>%  # region
  count() %>% 
  ungroup() %>% 
  group_by(survey_year, Region) %>% 
  mutate(prop= round((n/sum(n)), 2)) %>% 
  mutate(source = "Survey sample")

usda_summ5 <- usda %>% 
  mutate(survey_year = year) %>% 
  filter(!is.na(survey_year)) %>% 
  group_by(survey_year, Region, acreage_range) %>%  # region
  summarize(n = sum(organic_ops_count)) %>% 
  ungroup() %>% 
  group_by(survey_year, Region) %>% 
  mutate(prop= round((n/sum(n)), 2)) %>% 
  mutate(source = "USDA Census")

summ5 <- rbind(df_summ5, usda_summ5)

summ5 %>% 
  mutate(year = case_when(
    survey_year == 2010 ~ "Survey: 2010 | Census: 2012",
    survey_year == 2015 ~ "Survey: 2015 | Census: 2017",
    survey_year == 2020 ~ "Survey: 2020 | Census: 2022"
  )) %>% 
  ggplot(aes(x = factor(acreage_range, acres_cat7), 
             y = prop, fill = source)) +
  geom_col(position = position_dodge(preserve = "single")) +
  labs(fill = "", x = "Farm size (acres)", y = "Proportion of operations") +
  facet_grid(Region~year) +
  scale_fill_grey(start = 0.4, end = 0.8) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Times", size = 14),
        axis.text.x = element_text(angle = 45,vjust = 1, hjust=1))

ggsave('figures/append_repf1_usda.png', width = 8, height = 10)

## Stopped ----

summ5$prop[summ5$survey_year == 2010 & summ5$Region == "North Central" &
             summ5$size == "1 to 9"]
summ5$prop[summ5$survey_year == 2010 & summ5$Region == "Northeast" &
             summ5$size == "1 to 9"]
summ5$prop[summ5$survey_year == 2010 & summ5$Region == "South" &
             summ5$size == "1 to 9"]
summ5$prop[summ5$survey_year == 2010 & summ5$Region == "West" &
             summ5$size == "1 to 9"]

# Collapse smallest farms ----
acres_cat8 <- c("1 to 49", "50 to 179", "180 to 499",
                "500 to 999", "1,000 to 1,999", "2,000 +")
df_summ6 <- df.full %>% 
  mutate(size = case_when(
    size %in% c('1 to 9', '10 to 49') ~ '1 to 49',
    T ~ size
  )) %>% 
  group_by(survey_year, Region, size) %>%  # region
  count() %>% 
  ungroup() %>% 
  group_by(survey_year, Region) %>% 
  mutate(prop= round((n/sum(n)), 2)) %>% 
  mutate(source = "Survey sample")

usda_summ6 <- usda %>% 
  mutate(size = case_when(
    size %in% c('1 to 9', '10 to 49') ~ '1 to 49',
    T ~ size
  )) %>% 
  mutate(survey_year = case_when(
    year == 2012 ~ 2010,
    year == 2017 ~ 2015,
    T ~ NA
  )) %>% 
  filter(!is.na(survey_year)) %>% 
  group_by(survey_year, Region, size) %>%  # region
  summarize(n = sum(organic_ops)) %>% 
  ungroup() %>% 
  group_by(survey_year, Region) %>% 
  mutate(prop= round((n/sum(n)), 2)) %>% 
  mutate(source = "USDA Census")

summ6 <- rbind(df_summ6, usda_summ6)

summ6 %>% 
  mutate(year = case_when(
    survey_year == 2010 ~ "Survey: 2010 | Census: 2012",
    survey_year == 2015 ~ "Survey: 2015 | Census: 2017",
    survey_year == 2020 ~ "Survey: 2020 | Census: forthcoming"
  )) %>% 
  ggplot(aes(x = factor(size, acres_cat8), 
             y = prop, fill = source)) +
  geom_col(position = position_dodge(preserve = "single")) +
  labs(fill = "", x = "Farm size", y = "Proportion of operations") +
  facet_grid(Region~year) +
  scale_fill_viridis_d(begin = 0.2, end = 0.8) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45,vjust = 1, hjust=1))
