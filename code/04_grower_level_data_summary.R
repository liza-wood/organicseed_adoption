library(VIM)
library(tidyverse)
library(lubridate)

# This is the dataframe where I have identified main crops of growers, so each observation is only one grower
df <- readRDS("~/Box/organicseed/df_onelevel.rds") 
# This is the dataframe where I have lengthened out each crop for the grower, so any grower may have up to 3 crops
df.long <- readRDS("~/Box/organicseed/df_long_onelevel.rds")

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
         # I seemed to have ditched this stuff
         #producer_n, median_producer_revenue, producer_challenge_sum, 
         #retailer_n, median_retailer_revenue, retailer_challenge_sum, 
         #variety_n, 
         cropcat_n, median_rev_survey, 
         ngrowers, nsupplychainactors,
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
         # I seemed to have ditched this stuff
         #producer_n, median_producer_revenue, producer_challenge_sum, 
         #retailer_n, median_retailer_revenue, retailer_challenge_sum, 
         #variety_n, 
         cropcat_n, median_rev_survey, 
         ngrowers, nsupplychainactors,
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


write.csv(variable_df, "~/Documents/Davis/R-Projects/organicseed/tables/table1a.csv", row.names = F)
# Other parts of table 1 are in 03_regional_data: line 33 for funding and supplychain_yearonly for supply chain

# FIGURES FOR PAPER -----
# 2004-2008
greens <- RColorBrewer::brewer.pal(8, "Greens")
greens <- colorRampPalette(c("lightgrey", "#55752f"))(8)
# 2009-2013
reds <- RColorBrewer::brewer.pal(8, "Oranges")
reds <- colorRampPalette(c("lightgrey", "#90353b"))(8)
# 2014-2018
blues <- RColorBrewer::brewer.pal(8, "PuBu")
blues <- colorRampPalette(c("lightgrey", "#1A476f"))(8)

# Figure 1b and summaries for barriers ----
# COME BACK TO THIS
barrier_eachyr <- df %>% 
  pivot_longer(cols = c(barrier_seedTreatlack, barrier_buyerReq,
                        barrier_insuffQuantity,barrier_distrustQuality), 
               names_to = "barrier_type", values_to = "barrier_strength")
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

# Sentiment ----
df %>% 
  filter(!is.na(OGSeedOGIntegrityf)) %>% 
  ggplot(aes(fill = factor(survey_year),
             x = OGSeedOGIntegrityf)) +
  geom_bar(position = "dodge") +
  #geom_text(position=position_dodge(width=0.9), size = 3, hjust = 1.2, vjust = .5) +
  coord_flip() +
  scale_fill_manual(values = c(greens[8], reds[8], blues[8])) + 
  labs(x = "", fill = "", y = "Percent of respondents") +
  theme_linedraw() +
  theme(legend.position = "none")

# Sentiment ----
df %>% 
  filter(!is.na(TotalAcreage)) %>% 
  ggplot(aes(x = factor(survey_year),
             y = TotalAcreage)) +
  geom_boxplot() +
  #geom_text(position=position_dodge(width=0.9), size = 3, hjust = 1.2, vjust = .5) +
  coord_flip() +
  scale_fill_manual(values = c(greens[8], reds[8], blues[8])) + 
  labs(x = "", fill = "", y = "Percent of respondents") +
  theme_linedraw() +
  theme(legend.position = "none")

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

# What about attitudes
df %>% 
  ggplot(aes(x = factor(survey_year), y = OGSeedOGIntegrityn,
             color = factor(survey_year))) +
  geom_boxplot() +
  labs(x = "", y = "Values organic seed", color = "") +
  scale_color_manual(values = c(greens[8],reds[8], blues[8])) +
  theme_linedraw() +
  theme(legend.position = "none")

# Farm size ----
df %>% 
  ggplot(aes(x = factor(survey_year), y = TotalAcreage,
             color = factor(survey_year))) +
  geom_boxplot() +
  labs(x = "", y = "Percent organic seed used", color = "") +
  scale_color_manual(values = c(greens[8],reds[8], blues[8])) +
  theme_linedraw() +
  theme(legend.position = "none") +
  ylim(c(0,1000))
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

## See github here for older explorations of data