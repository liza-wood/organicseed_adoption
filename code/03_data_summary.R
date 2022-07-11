library(VIM)
library(tidyverse)
library(lubridate)

# This is the dataframe where I have identified main crops of producers, so each observation is only one producer
  
df <- readRDS("~/Box/organicseed/df.rds")
# This is the dataframe where I have lengthened out each crop for the grower, so any grower may have up to 3 crops
df.long <- readRDS("~/Box/organicseed/df_long.rds")

palette <- RColorBrewer::brewer.pal(8, "YlGnBu")
# How many crops are each grower growing?
df.long %>% 
  group_by(RespondentID) %>% 
  count() %>% 
  group_by(n) %>% 
  count()

# Let's narrow this down to the variables of interest
df <- df %>% 
  filter(Region != "International", maincrop != "Unknown") %>% 
  select(maincrop, OGseed_percent_maincrop, IncreasedOGSeed,
         VegiAcreage, FieldAcreage, ForageAcreage, # Total acres for each crop
         OGVegiSeed, OGFieldSeed, OGForageSeed, # % organic seed used
         TotalAcreage, 
         OGSeedOGIntegrityf, OGSeedOGIntegrityn, # Belief that organic seed is important
         CertifierRequest,
         CertifierRequestn, # Certifier has requested
         funding_avg_annual_amt, survey_year, year_range, # year variables (funding)
         producer_n, median_producer_revenue, producer_challenge_sum, crop_n, variety_n,
         retailer_n, median_retailer_revenue, retailer_challenge_sum, 
         Region, # regional variables (region and seed production)
         contains("barrier"), # barriers
         contains("AddSteps")) 

# Let's narrow this down to the variables of interest
df.long <- df.long %>% 
  filter(Region != "International") %>% 
  select(OGcrop, OGseed_percent, IncreasedOGSeed,
         VegiAcreage, FieldAcreage, ForageAcreage, # Total acres for each crop
         TotalAcreage, 
         OGSeedOGIntegrityf, OGSeedOGIntegrityn, # Belief that organic seed is important
         CertifierRequest,
         CertifierRequestn, # Certifier has requested
         funding_avg_annual_amt, survey_year, year_range, # year variables (funding)
         producer_n, Region, # regional variables (region and seed production)
         retailer_n, median_retailer_revenue, 
         contains("barrier"), # barriers
         contains("AddSteps")) 

# Where are the NAs in the data?
aggr(df, col=c('navyblue','red'), 
     numbers=TRUE, sortVars=TRUE, 
     labels=names(df), cex.axis=.7, gap=3, 
     ylab=c("Histogram of missing data","Pattern"))


# TABLE1: Make a table of basic distributions of each variable ----
df11 <- filter(df, survey_year == 2011)
df16 <- filter(df, survey_year == 2016)
df21 <- filter(df, survey_year == 2021)
df.long11 <- filter(df.long, survey_year == 2011)
df.long16 <- filter(df.long, survey_year == 2016)
df.long21 <- filter(df.long, survey_year == 2021)

median(df11$OGseed_percent, na.rm = T)
median(df16$OGseed_percent, na.rm = T)
median(df21$OGseed_percent, na.rm = T)

df$barrier_availability
variable_df <- data.frame(
  Questions = c("Number of respondents",
                "Last year, what approximate percentage of total acreage of [crops] was planted with certified organic seed?",
                "Over the last 3 years have you increased or decreased the percentage of organic seed that you use in for your [crops]",
                "Total Acreage",
                "Vegetable Crop Acreage",
                "Field Crop Acreage",
                "Forage Crop Acreage",
                "Organic seed is important to the integrity of organic food production",
                "A factor in your decision NOT to purchase organic seed (1-4):",
                "      Specific variety not available as organic seed",
                "      Processor (buyer) requires or supplies varieties that are not available organically",
                "      Distrust of organic seed quality",
                "      Insufficient quantity of seed",
                "      Price",
                "Has your certifier requested that you take greater steps to source organic seed?"
                ),
                
  mean2011 = c(nrow(df11),
           round(mean(df.long11$OGseed_percent,na.rm = T), 2),
           round(table(df.long11$IncreasedOGSeed)[3]/sum(table(df.long11$IncreasedOGSeed)[1:3]), 2),
           round(mean(df.long11$TotalAcreage[df.long11$TotalAcreage != 0], na.rm = T), 2),
           round(mean(df.long11$VegiAcreage[df.long11$VegiAcreage != 0], na.rm = T), 2),
           round(mean(df.long11$FieldAcreage[df.long11$FieldAcreage != 0], na.rm = T), 2),
           round(mean(df.long11$ForageAcreage[df.long11$ForageAcreage != 0], na.rm = T), 2),
           round(mean(df11$OGSeedOGIntegrityn, na.rm = T), 2),
           "",
           NA,
           NA,
           NA,
           NA,
           NA,
           round(table(df11$CertifierRequest)[2]/sum(table(df11$CertifierRequest)[1:2]), 2)),
           
  mean2016 = c(nrow(df16),
             round(mean(df.long16$OGseed_percent,na.rm = T), 2),
             round(table(df.long16$IncreasedOGSeed)[3]/sum(table(df.long16$IncreasedOGSeed)[1:3]), 2),
             round(mean(df.long16$TotalAcreage[df.long16$TotalAcreage != 0], na.rm = T), 2),
             round(mean(df.long16$VegiAcreage[df.long16$VegiAcreage != 0], na.rm = T), 2),
             round(mean(df.long16$FieldAcreage[df.long16$FieldAcreage != 0], na.rm = T), 2),
             round(mean(df.long16$ForageAcreage[df.long16$ForageAcreage != 0], na.rm = T), 2),
             round(mean(df16$OGSeedOGIntegrityn, na.rm = T), 2),
             "",
             round(mean(df16$barrier_availability, na.rm = T), 2),
             round(mean(df16$barrier_buyerReq, na.rm = T), 2),
             round(mean(df16$barrier_distrustQuality, na.rm = T), 2),
             round(mean(df16$barrier_insuffQuantity, na.rm = T), 2),
             round(mean(df16$barrier_price, na.rm = T), 2),
             round(table(df16$CertifierRequest)[2]/sum(table(df16$CertifierRequest)[1:2]), 2)),
  
  mean2021 = c(nrow(df21),
             round(mean(df.long21$OGseed_percent,na.rm = T), 2),
             round(table(df.long21$IncreasedOGSeed)[3]/sum(table(df.long21$IncreasedOGSeed)[1:3]), 2),
             round(mean(df.long21$TotalAcreage[df.long21$TotalAcreage != 0], na.rm = T), 2),
             round(mean(df.long21$VegiAcreage[df.long21$VegiAcreage != 0], na.rm = T), 2),
             round(mean(df.long21$FieldAcreage[df.long21$FieldAcreage != 0], na.rm = T), 2),
             round(mean(df.long21$ForageAcreage[df.long21$ForageAcreage != 0], na.rm = T), 2),
             round(mean(df21$OGSeedOGIntegrityn, na.rm = T), 2),
             "",
             round(mean(df21$barrier_availability, na.rm = T), 2),
             round(mean(df21$barrier_buyerReq, na.rm = T), 2),
             round(mean(df21$barrier_insuffQuantity, na.rm = T), 2),
             round(mean(df21$barrier_seedTreatlack, na.rm = T), 2),
             round(mean(df21$barrier_price, na.rm = T), 2),
             round(table(df21$CertifierRequest)[2]/sum(table(df21$CertifierRequest)[1:2]), 2)
             )
)

funding_df <- df %>% 
  group_by(Region, survey_year) %>% 
  select(funding_avg_annual_amt) %>% 
  unique() %>% 
  pivot_wider(names_from = survey_year, values_from = funding_avg_annual_amt) %>% 
  ungroup() %>% 
  select(Region, `2011`, `2016`, `2021`) %>% 
  rbind(c("Total", 2530119, 4360990, 6109936))
# Total is from line 105 in variable creation

regional_df <- df %>% 
  filter(Region != "International" & !is.na(Region)) %>% 
  group_by(Region) %>% 
  select(producer_n, median_producer_revenue, crop_n, variety_n,
         retailer_n, median_retailer_revenue) %>% 
  unique() %>% 
  ungroup() %>% 
  rbind(c("Total", 
          sum(unique(.$producer_n), na.rm = T), 
          mean(unique(.$median_producer_revenue), na.rm = T),
          mean(unique(.$crop_n), na.rm = T),
          mean(unique(.$variety_n), na.rm = T),
          sum(unique(.$retailer_n), na.rm = T),
          mean(unique(.$median_retailer_revenue), na.rm = T)))

# But what about as a ratio? Need to read in all organic growers?
growers <- read.csv("~/Box/organicseed/NOP_USorganicCrops_6.28.22.csv") %>% slice(-c(1:2))

western <- c("CA", "OR", "WA", "ID", "NV", "AZ", "UT", "NM", "CO", "WY", "MT", 
             "AK", "HI","California", "Oregon", "Washington", "Idaho", "Nevada",
             "Arizona", "Utah", "New Mexico", "Colorado", "Wyoming", "Montana",
             "Alaska", "Hawaii")
northcentral <- c("ND", "SD", "KS", "NE", "MN", "IA", "MO", "WI", "IL", "MI", 
                  "IN", "OH", "North Dakota", "South Dakota", "Kansas",
                  "Nebraska", "Minnesota", "Iowa", "Missouri", "Wisconsin", 
                  "Illinois", "Michigan", "Indiana", "Ohio")
southern <- c("TX", "OK", "AR", "LA", "KY", "TN", "MS", "AL", "FL", "GA", "SC",
              "NC", "VA", "Texas", "Oklahoma", "Arkansas", "Louisiana", 
              "Kentucky", "Tennessee", "Mississippi", "Alabama", "Florida", 
              "Georgia", "South Carolina", "North Carolina", "Virginia")
northeast <- c("WV", "MD", "DE", "NJ", "PA", "NY", "CT", "RI", "MA", "VT", 
               "NH", "ME", "West Virginia", "Maryland", "Delaware", 
               "New Jersey", "Pennsylvania", "New York", "Connecticut", 
               "Rhode Island", "Massachusetts", "Vermont", "New Hampshire", "Maine")

growers <- growers %>% 
  mutate(Region = case_when(
    Physical.Address..State.Province %in% western  ~ "West",
    Physical.Address..State.Province %in% northcentral ~ "North Central",
    Physical.Address..State.Province %in% southern ~ "South",
    Physical.Address..State.Province %in% northeast ~ "Northeast",
    T ~ NA_character_)) %>% 
  group_by(Region) %>% 
  count() %>% 
  filter(!is.na(Region))

regional_df <- left_join(regional_df, growers)
regional_df$n[regional_df$Region == "Total"] <- sum(regional_df$n, na.rm = T)
regional_df$producer_ratio <- round(as.numeric(regional_df$producer_n)/regional_df$n, 2)
regional_df$retailer_ratio <- round(as.numeric(regional_df$retailer_n)/regional_df$n, 2)
           
write.csv(variable_df, "~/Documents/Davis/R-Projects/organicseed/tables/table1a.csv", row.names = F)
write.csv(funding_df, "~/Documents/Davis/R-Projects/organicseed/tables/table1b.csv", row.names = F)
write.csv(regional_df, "~/Documents/Davis/R-Projects/organicseed/tables/table1c.csv", row.names = F)

# FIGURES FOR PAPER -----
blues <- RColorBrewer::brewer.pal(8, "PuBu")
blues <- colorRampPalette(c("lightgrey", "#1A476f"))(8)
greens <- RColorBrewer::brewer.pal(8, "Greens")
greens <- colorRampPalette(c("lightgrey", "#55752f"))(8)
reds <- RColorBrewer::brewer.pal(8, "Oranges")
reds <- colorRampPalette(c("lightgrey", "#90353b"))(8)

# Figure 1a and summaries for funding ----
funds_organic <- read.csv("~/Dropbox/SOSInteractiveSurvey/local_data/public-funding-Dec21.csv")
fed_breed <- read.csv("~/Box/lgu/breeding_awards_combined.csv")
fed_all <- read.csv("~/Box/lgu/data_raw/federal_funding/allawards.csv") %>% 
  filter(Award.Dollars > 0) %>% 
  mutate(breeding = case_when(
    Proposal.Number %in% fed_breed$Proposal.Number ~ T,
    T ~ F
  )) %>% 
  mutate(organic = case_when(
    str_detect(Program.Name, "[Oo]rganic") ~ T,
    T ~ F
  )) %>% 
  mutate(year = year(Award.Date)) 
fed_organicbreed <- filter(fed_all, breeding == T & organic == T)
fed_all <- unique(fed_all)
fed_all <- filter(fed_all, year > 2003 & year < 2019)

# Compare my measure for organic breeding funds from federal (using regex) versus what we used for SOS report:
sos_funds <- funds_organic %>% 
  filter(year > 2001) %>% 
  group_by(year) %>% 
  summarize(annual_total = sum(funding_amt)) 
sos_funds %>% 
  ggplot(aes(x = year, y = annual_total)) +
  geom_col() +
  geom_smooth(method = "lm")
lm(scale(annual_total) ~ year, data = sos_funds)$coefficients[2]

fed_funds <- fed_organicbreed %>% 
  group_by(year) %>% 
  summarize(annual_total = sum(Award.Dollars))
fed_funds %>% 
  ggplot(aes(x = year, y = annual_total)) +
  geom_col() +
  geom_smooth(method = "lm")
lm(scale(annual_total) ~ year, data = sos_funds)$coefficients[2]

year_color <- data.frame(year = 2004:2018,
                         year_color = c(rep(greens[8], 5),
                                        rep(reds[8], 5),
                                        rep(blues[8], 5)))

# This is looking at organic breeding as portion of all federal funding, according to the USDA database only and my regex, not the SOS generated list.
fed_summary <- fed_all %>% 
  group_by(year, breeding, organic) %>% 
  summarize(breeding_sum = sum(Award.Dollars, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(annual_sum = sum(breeding_sum, na.rm = T)) %>% 
  filter(breeding == T & organic == T) %>% 
  unique() %>% 
  mutate(Percent = round(100*(breeding_sum/annual_sum), 1)) %>% 
  left_join(year_color)

pfunding <- ggplot(fed_summary, aes(x = year, y = breeding_sum/100000, 
                                    label = paste0(Percent, "%"))) +
  geom_col(fill = fed_percent$year_color) +
  geom_text(size = 3, vjust = 2) +
  theme_linedraw() +
  theme(legend.position = "none") +
  labs(x = "", y = "Federal funding by $100 thousand\n(% of total funding)"); pfunding

# This is not satisfying, so let's identify the federal fundings from the SOS list and look at this amount as a portion of tital federal funds 

funds_organic <- funds_organic %>% 
  filter(year > 2003 & year < 2019) %>% 
  mutate(year_range = case_when(
    year %in% 2004:2008 ~ "2004-2008",
    year %in% 2009:2013 ~ "2009-2013",
    year %in% 2014:2018 ~ "2014-2018",
    T ~ NA_character_
  )) %>% 
  mutate(source_aggregate = case_when(
    funding_source %in% c("FAFO", "OFRF", "Other Non-Federal Funds", "Clif Bar Family Foundation") ~ "non-public",
    T ~ "public"
  )) %>% 
  mutate(federal = case_when(
    funding_source %in% c("FAFO", "OFRF", "Other Non-Federal Funds", "Clif Bar Family Foundation", "SARE") ~ F,
    T ~ T
  )) %>% 
  group_by(year_range) %>% 
  mutate(average_by_project = sum(funding_amt)/n()) %>% 
  mutate(annual_average = sum(funding_amt)/5)

fed_summary_elab <- fed_all %>% 
  group_by(year) %>% 
  summarize(annual_sum = sum(Award.Dollars, na.rm = T)) %>% 
  left_join(year_color)

fed_summary <- funds_organic %>% 
  filter(!is.na(year_range), federal == T) %>% 
  group_by(year_range, year) %>% 
  summarize(organic_total = sum(funding_amt)) %>% 
  left_join(fed_summary_elab) %>% 
  mutate(Percent = round(100*(organic_total/annual_sum), 1))

pfunding <- sos_funds %>% 
  filter(year > 2003 & year < 2019) %>% 
  ggplot(aes(x = year, y = annual_total/1000000, 
             label = paste0(fed_summary$Percent, "%"))) +
  geom_col(fill = fed_summary$year_color) +
  geom_text(size = 3, vjust = -0.5) +
  theme_linedraw() +
  theme(legend.position = "none") +
  labs(x = "", y = "Funding in millions\n(% of total federal funding)"); pfunding

# Average percentages over the years
100*(sum(fed_summary$organic_total[fed_summary$year_range == "2004-2008"])/sum(fed_summary$annual_sum[fed_summary$year_range == "2004-2008"]))
100*(sum(fed_summary$organic_total[fed_summary$year_range == "2009-2013"])/sum(fed_summary$annual_sum[fed_summary$year_range == "2009-2013"]))
100*(sum(fed_summary$organic_total[fed_summary$year_range == "2014-2018"])/sum(fed_summary$annual_sum[fed_summary$year_range == "2014-2018"]))

# Some details from different sources
funding_sources_amt <- funds_organic %>% 
  group_by(year_range) %>% 
  mutate(yr_range_total = sum(funding_amt, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year_range, source_aggregate) %>% 
  summarize(yr_range_total = yr_range_total, 
            source_sum = sum(funding_amt),
            Percent = round(100*(source_sum/yr_range_total), 1)) %>% 
  unique()

funding_sources_number <- funds_organic %>% 
  group_by(year_range) %>% 
  mutate(n_projects = n()) %>% 
  ungroup() %>% 
  group_by(year_range, source_aggregate) %>% 
  summarize(n_projects = n_projects, 
            source_n = n(),
            Percent = round(100*(source_n/n_projects), 1)) %>% 
  unique()

# Summaries about market proxies ----
## 1. Certifier perceptions is just straight from the survey Kiki sent
## 2. Seed producers and companies -- still could be improved (e.g. database for seed companies?)
regional_df

# 7 = "$300,000-$1 million"
# 6 = "$75,000-$99,999",
# 5 = "$100,000-$149,999",
# 4 = "$150,000-$299,999",

# Figure 1b and summaries for barriers ----
barrier_eachyr <- barriers %>% 
  group_by(survey_year, barrier, score) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(survey_year, barrier) %>%
  mutate(total = sum(n)) %>% 
  filter(score == "Significant factor" | score == "Moderate factor") %>% 
  mutate(n_both = sum(n)) %>% 
  mutate(Percent = round(100*(n_both/total), 2)) %>% 
  ungroup() %>% 
  select(-score, -n) %>% 
  unique()

order <- barrier_eachyr %>% 
  filter(survey_year == 2016) %>% 
  arrange(-Percent) 

pbarrier <- barrier_eachyr %>% 
  filter(!(barrier %in% c("Lacks seed treatment",
                          "Save own seed",
                          "Distrust quality"))) %>% 
  ggplot(aes(y = Percent, fill = factor(survey_year),
             x = forcats::fct_rev(factor(barrier, levels = order$barrier)),
             label = paste0("n = ", n_both))) +
  geom_col(position = "dodge") +
  geom_text(position=position_dodge(width=0.9), size = 3, hjust = 1.2, vjust = .5) +
  coord_flip() +
  scale_fill_manual(values = c(reds[8], blues[8])) + 
  labs(x = "", fill = "", y = "Percent of respondents") +
  theme_linedraw() +
  theme(legend.position = "none"); pbarrier

# Certifier behavior ----

table(df$CertifierRequestn, df$survey_year)
round(100*(prop.table(table(df$CertifierRequestn, df$survey_year), margin = 2)), 1)

# Figured 1c percent acreage of crops ----
pacreage <- df %>% 
  ggplot(aes(x = factor(survey_year), y = OGseed_percent_maincrop,
             color = factor(survey_year))) +
  geom_boxplot() +
  labs(x = "", y = "Percent organic seed used", color = "") +
  scale_color_manual(values = c(greens[8],reds[8], blues[8])) +
  theme_linedraw() +
  theme(legend.position = "none"); pacreage

# Figure 1d self-reported increase ----
increase <- df %>% 
  filter(!is.na(IncreasedOGSeed)) %>% 
  group_by(survey_year, IncreasedOGSeed) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(survey_year) %>% 
  mutate(Percent = 100*(n/sum(n))) %>% 
  mutate(IncreasedOGseed = case_when(is.na(IncreasedOGSeed) ~ "No response", 
                                     T ~ IncreasedOGSeed)) 
pincrease <- increase %>% 
  ggplot(aes(fill = factor(survey_year), 
             y = Percent,
             x = factor(IncreasedOGSeed, levels = c("No response", "No", 
                                                    "Yes", "Already at 100%")),
             label = paste0("n = ", n))) +
  geom_col(position = "dodge") +
  geom_text(position=position_dodge(width=0.9), size = 3, hjust = 1.2, vjust = .5) +
  coord_flip() +
  labs(x = "", y = "Percent of respondents", 
       fill = "") +
  scale_fill_manual(values = c(greens[8],reds[8], blues[8])) +
  theme_linedraw() +
  theme(legend.position = "none") ; pincrease

# Combining plots for Figure 1 ----
library(cowplot)
legend <- get_legend(
  ggplot(increase, aes(x = IncreasedOGSeed, y = Percent, fill = factor(survey_year))) +
    geom_col() +
    scale_fill_manual(values = c(greens[8], reds[8], blues[8])) +
    labs(fill = "")); legend

ggdraw() + 
  draw_plot(pfunding, x = 0, y = .5, width = .49, height = .5) +
  draw_label("A", x = .05, y = .98) +
  draw_plot(pbarrier, x = .5, y = .5, width = 0.49, height = .5) +
  draw_label("B", x = .5, y = .98) +
  draw_plot(legend, x = 0, y = 0, width = 0.14, height = .5 ) +
  draw_plot(pacreage, x = .15, y = 0, width = 0.34, height = .5) +
  draw_label("C", x = .15, y = .48) +
  draw_plot(pincrease, x= .5, y = 0, width = 0.49, height = .5) +
  draw_label("D", x = .55, y = .48)
ggsave("tables/figure1.png", height = 6, width = 10, dpi = 800)

### Older versions of exploration ----
           
# Summary of dependent variable ----
summary(df$OGseed_percent_maincrop)

df %>% 
  ggplot(aes(x = factor(survey_year), y = OGseed_percent_maincrop)) +
  geom_boxplot(aes(color = maincrop),
               position =  position_dodge2(preserve = "single")) +
  labs(x = "", y = "Percent organic seed used", 
       color = "Main crop") +
  scale_color_manual(values = palette[c(2,4,6,8)]) +
  theme_linedraw()

summary(df.long$OGseed_percent)

df.long %>% 
  ggplot(aes(x = factor(survey_year), y = OGseed_percent)) +
  geom_boxplot(aes(color = OGcrop)) +
  labs(x = "", y = "Percent organic seed used", 
       color = "",
       title = "Last year, what approximate percentage of total acreage\nof [crops] was planted with certified organic seed?") +
  scale_color_manual(values = palette[c(3,6,8)]) +
  theme_linedraw()


df %>% 
  filter(!is.na(IncreasedOGSeed)) %>% 
  group_by(survey_year, maincrop, IncreasedOGSeed) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(survey_year, maincrop) %>% 
  mutate(Percent = 100*(n/sum(n))) %>% 
  mutate(IncreasedOGseed = case_when(is.na(IncreasedOGSeed) ~ "No response", 
                                     T ~ IncreasedOGSeed)) %>% 
  ggplot(aes(x = factor(survey_year), 
             y = Percent,
             fill = factor(IncreasedOGSeed, levels = c("No response", "No", 
                                                       "Yes", "Already at 100%")))) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Percent respondents", 
       fill = "Increased organic seed use") +
  scale_fill_manual(values = palette[c(2,4,6,8)]) +
  theme_linedraw() +
  facet_wrap(~maincrop)

table(df.long$IncreasedOGSeed, df.long$survey_year)
table(df.long$IncreasedOGSeed, df.long$OGcrop)
table(df.long$OGcrop, df.long$survey_year)

df.long %>% 
  filter(!is.na(IncreasedOGSeed)) %>% 
  group_by(survey_year, OGcrop, IncreasedOGSeed) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(survey_year, OGcrop) %>% 
  mutate(Percent = 100*(n/sum(n))) %>% 
  mutate(IncreasedOGseed = case_when(is.na(IncreasedOGSeed) ~ "No response", 
                                     T ~ IncreasedOGSeed)) %>% 
  ggplot(aes(x = factor(survey_year), 
             y = Percent,
             fill = factor(IncreasedOGSeed, levels = c("No response", "No", 
                                                       "Yes", "Already at 100%")))) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Percent respondents", 
       fill = "",
       title = "Over the last 3 years have you increased or decreased\nthe percentage of organic seed that you use in for your [crops] ") +
  scale_fill_manual(values = palette[c(2,4,6,8)]) +
  theme_linedraw() +
  facet_wrap(~OGcrop) +
  theme(strip.background =element_rect(fill="white"),
        strip.text = element_text(colour = 'black'))


# Summary of "demand" variables ----
## Demand of the grower, based on percieved importance, and demand of the regulator
table(df$OGSeedOGIntegrityf)
table(is.na(df$OGSeedOGIntegrityf))
summary(df$OGSeedOGIntegrityn)

table(df$CertifierRequest)
summary(df$CertifierRequestn)

# For additional steps, it doesn't look that different over time so not inclduing the year
## Because of this I can include timely and contract, which are only for 2016 and 2021
df %>% 
  pivot_longer(cols = c(AddStepsTrial, AddStepsCat, AddStepsSeedFinder, AddStepsSeedFinder,
                        AddStepsTimely, AddStepsContract), 
               names_to = "step_kind", values_to = "logical") %>% 
  select(step_kind, logical) %>%  # survey_year, 
  filter(!is.na(logical)) %>% 
  group_by(step_kind, logical) %>% # survey_year, 
  count() %>% 
  ungroup() %>% 
  group_by(step_kind) %>% # survey_year, 
  mutate(sum = sum(n)) %>% 
  ungroup() %>% 
  mutate(Percent = round(100*(n/sum))) %>% 
  mutate(step_kind = case_when(
    step_kind == "AddStepsCat" ~ "Research more than three seed catalogues",
    step_kind == "AddStepsSeedFinder" ~ "Search the Organic Seed Finder website",
    step_kind == "AddStepsTimely" ~ "Request seed in a timely manner",
    step_kind == "AddStepsContract" ~ "Contract organic seed production",
    T ~ "Conduct trials of available organic varieties"
  )) %>% 
  filter(logical == "Yes") %>% 
  ggplot(aes(x = step_kind, y = Percent)) +
  geom_col() +
  coord_flip()
  
  

# Summary of "supply" variables ----
## Funding, at the time-panel level
df %>% 
  select(year_range, survey_year, Region, funding_avg_annual_amt) %>% 
  unique()




## Barriers to sourcing organic seed
## This is only available for 2021 survey so maybe run a separate model for this
barriers <- df %>% 
  filter(survey_year %in% c(2016,2021)) %>% 
  rename(
    "Lacks seed treatment" = barrier_seedTreatlackf,
    "Save own seed" = barrier_saveOwnf,
    "Price" = barrier_pricef,
    "Lacks desired traits" = barrier_lackdesiredTraitsf,
    "Insufficient quantity" = barrier_insuffQuantityf,
    "Distrust quality" = barrier_distrustQualityf,
    "Buyer requires certain variety" = barrier_buyerReqf,
    "Availability of certain varieties" = barrier_availabilityf
  ) %>% 
  pivot_longer(`Lacks seed treatment`:`Availability of certain varieties`, 
               names_to = "barrier", values_to = "score") %>% 
  filter(!is.na(score))
  
barrier_allyr <- barriers %>% 
  group_by(survey_year, barrier, score) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(barrier) %>% # could add survey_year here if I wanted to facet
  mutate(Percent = round(100*(n/sum(n)), 2)) %>% 
  ungroup()

order <- barrier_allyr %>% 
  filter(score == "Significant factor" | score == "Moderate factor" | score == "Slight factor") %>% 
  group_by(barrier) %>% 
  summarize(n = sum(n)) %>% 
  arrange(n) 

barrier_allyr %>% 
  ggplot(aes(y = Percent, x = factor(barrier, levels = order$barrier), 
             fill = score)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(x = "", fill = "",
       title = "To what extent were the following reasons factor in your\ndecision NOT to purchase organic seed:") +
  theme_linedraw() 


table(df$barrier_seedTreatlack)
table(df$barrier_buyerReq)
table(df$barrier_insuffQuantity)
table(df$barrier_saveOwn)
table(df$barrier_distrustQuality)
table(df$barrier_price)
table(df$barrier_availability)
table(df$barrier_lackdesiredTraits)

# Summary of demographic variables:
table(df$maincrop)
table(df$maincrop, df$survey_year)
table(df.long$OGcrop)
table(df.long$OGcrop, df.long$survey_year)
summary(df$TotalAcreage)
summary(df.long$TotalCropAcreage)

# Last year how many organic acres did you have in annual vegetables. Please fill in number
summary(df$VegiAcreage)
summary(df$FieldAcreage)
summary(df$ForageAcreage)
df$VegiAcreage <- ifelse(df$VegiAcreage < 0, NA, df$VegiAcreage)
df$VegiAcreage <- ifelse(is.na(df$VegiAcreage), 0 , df$VegiAcreage)
df$FieldAcreage <- ifelse(is.na(df$FieldAcreage), 0 , df$FieldAcreage)
df$ForageAcreage <- ifelse(is.na(df$ForageAcreage), 0 , df$ForageAcreage)

# Last year what approximate percentage of total acreage of annual vegetables was planted with certified organic seed. Please fill in number
summary(df$OGVegiSeed)
summary(df$OGFieldSeed)
summary(df$OGForageSeed)

summary(df$VegiAcreage/df$TotalAcreage)
summary(df$FieldAcreage/df$TotalAcreage)
summary(df$ForageAcreage/df$TotalAcreage)

# Certifiers across the US ----
nop <- read.csv("~/Box/organicseed/NOP_USorganicCrops_6.28.22.csv") %>% 
  slice(-(1:2)) %>% 
  filter(Physical.Address..Country == "United States of America (the)")

certifiers <- nop %>% 
  group_by(Certifier.Name) %>% 
  count() %>% 
  arrange(-n) %>% 
  ungroup() %>% 
  mutate(sum = sum(n),
         Percent = 100*(n/sum)) 

certifiers %>% 
  filter(Percent > 2) %>% 
  summarize(sum(Percent))


