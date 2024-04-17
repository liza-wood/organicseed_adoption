# OG Farm size distribution
# Data from the USDA census tabulations, sent from Mallory NASS
usda <- read.csv('~/Documents/Davis/R-Projects/big-organic-bifurcation/data/usda_og_tabs.csv')

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
                  #barrier_buyerReq,
                  barrier_insuffQuantity,
                  barrier_lackdesiredTraits,
                  barrier_availability, barrier_seedTreatlack) %>% 
  na.omit() 

acres_cat7 <- c("1 to 9", "10 to 49", "50 to 179", "180 to 499",
                "500 to 999", "1,000 to 1,999", "2,000 +")

df.full <- df.full %>% 
  mutate(size = case_when(
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
  group_by(survey_year, size) %>%  # region
  count() %>% 
  ungroup() %>% 
  group_by(survey_year) %>% 
  mutate(prop= round((n/sum(n)), 2)) %>% 
  mutate(source = "Survey sample")

usda_summ4 <- usda %>% 
  mutate(survey_year = case_when(
    year == 2012 ~ 2010,
    year == 2017 ~ 2015,
    T ~ NA
  )) %>% 
  filter(!is.na(survey_year)) %>% 
  group_by(survey_year, size) %>%  # region
  summarize(n = sum(organic_ops)) %>% 
  ungroup() %>% 
  group_by(survey_year) %>% 
  mutate(prop= round((n/sum(n)), 2)) %>% 
  mutate(source = "USDA Census")

summ4 <- rbind(df_summ4, usda_summ4)

summ4 %>% 
  mutate(year = case_when(
    survey_year == 2010 ~ "Survey: 2010 | Census: 2012",
    survey_year == 2015 ~ "Survey: 2015 | Census: 2017",
    survey_year == 2020 ~ "Survey: 2020 | Census: forthcoming"
  )) %>% 
  ggplot(aes(x = factor(size, acres_cat7), 
             y = prop, fill = source)) +
  geom_col(position = position_dodge(preserve = "single")) +
  labs(fill = "", x = "Farm size", y = "Proportion of operations") +
  facet_grid(~year) +
  scale_fill_viridis_d(begin = 0.2, end = 0.8) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45,vjust = 1, hjust=1))

ggsave('figures/append_repf1_usda.png', width = 7, height = 4)
