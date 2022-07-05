library(VIM)
library(tidyverse)

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
         producer_n, median_producer_revenue, producer_challenge_sum, 
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
         contains("barrier"), # barriers
         contains("AddSteps")) 

# Where are the NAs in the data?
aggr(df, col=c('navyblue','red'), 
     numbers=TRUE, sortVars=TRUE, 
     labels=names(df), cex.axis=.7, gap=3, 
     ylab=c("Histogram of missing data","Pattern"))

# Make a table of basic distributions of each variable ----
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
  group_by(Region) %>% 
  select(producer_n, median_producer_revenue, producer_challenge_sum) %>% 
  unique() %>% 
  ungroup() %>% 
  rbind(c("Total", 882, 4.7, 2.3))
# Total is from line 190 in variable creation
           
write.csv(variable_df, "~/Documents/Davis/R-Projects/organicseed/tables/table1a.csv", row.names = F)
write.csv(funding_df, "~/Documents/Davis/R-Projects/organicseed/tables/table1b.csv", row.names = F)
write.csv(regional_df, "~/Documents/Davis/R-Projects/organicseed/tables/table1c.csv", row.names = F)
           
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

table(df$IncreasedOGSeed)
table(is.na(df$IncreasedOGSeed), df$survey_year)
df$IncreasedOGSeed <- ifelse(df$OGseed_percent_maincrop == 100 & 
                               is.na(df$IncreasedOGSeed), "Already at 100%", 
                             df$IncreasedOGSeed)
table(is.na(df$IncreasedOGSeed), df$survey_year)

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

## Strength of seed producing regions, at the regional level for one time period
df %>% 
  select(Region, producer_n) %>% 
  unique()

## Strength of seed producing regions, at the regional level for one time period
df %>% 
  select(Region, median_producer_revenue) %>% 
  unique()
# 7 = "$300,000-$1 million"
# 6 = "$75,000-$99,999",
# 5 = "$100,000-$149,999",
# 4 = "$150,000-$299,999",

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


# FIGURES FOR PAPER -----
blues <- RColorBrewer::brewer.pal(8, "PuBu")
blues <- colorRampPalette(c("lightgrey", "#1A476f"))(8)
greens <- RColorBrewer::brewer.pal(8, "Greens")
greens <- colorRampPalette(c("lightgrey", "#55752f"))(8)
reds <- RColorBrewer::brewer.pal(8, "Oranges")
reds <- colorRampPalette(c("lightgrey", "#90353b"))(8)

funds_organic <- read.csv("~/Dropbox/SOSInteractiveSurvey/local_data/public-funding-Dec21.csv")
fed_breed <- read.csv("~/Box/lgu/breeding_awards_combined.csv")
fed_all <- read.csv("~/Box/lgu/data_raw/federal_funding/allawards.csv") %>% 
  filter(Award.Dollars > 0) %>% 
  mutate(breeding = case_when(
    Proposal.Number %in% fed_breed$Proposal.Number ~ T,
    T ~ F
  )) %>% 
  mutate(year = year(Award.Date)) 
year_color <- data.frame(year = 2006:2020,
                         year_color = c(rep(greens[8], 5),
                                        rep(reds[8], 5),
                                        rep(blues[8], 5)))
                                  

fed_all <- unique(fed_all)
fed_all <- filter(fed_all, year > 2005 & year < 2021)

fed_summary <- fed_all %>% 
  group_by(year, breeding) %>% 
  summarize(breeding_sum = sum(Award.Dollars, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(annual_sum = sum(breeding_sum, na.rm = T)) %>% 
  filter(breeding == T) %>% 
  unique() %>% 
  mutate(Percent = round(100*(breeding_sum/annual_sum), 1)) %>% 
  left_join(year_color)

pfunding <- ggplot(fed_summary, aes(x = year, y = breeding_sum/100000, 
                        label = paste0(Percent, "%"))) +
  geom_col(fill = fed_summary$year_color) +
  geom_text(size = 3, vjust = 2) +
  theme_linedraw() +
  theme(legend.position = "none") +
  labs(x = "", y = "Federal funding by $100 thousand\n(% of total funding)"); pfunding


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

pacreage <- df %>% 
  ggplot(aes(x = factor(survey_year), y = OGseed_percent_maincrop,
         color = factor(survey_year))) +
  geom_boxplot() +
  labs(x = "", y = "Percent organic seed used", color = "") +
  scale_color_manual(values = c(greens[8],reds[8], blues[8])) +
  theme_linedraw() +
  theme(legend.position = "none"); pacreage

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

legend <- get_legend(
  ggplot(increase, aes(x = IncreasedOGSeed, y = Percent, fill = factor(survey_year))) +
           geom_col() +
           scale_fill_manual(values = c(greens[8], reds[8], blues[8])) +
           labs(fill = "")); legend

library(cowplot)
?ggdraw
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
