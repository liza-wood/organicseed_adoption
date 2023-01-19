library(VIM)
library(tidyverse)
library(lubridate)

# This is the dataframe where I have identified main crops of growers, so each observation is only one grower
df <- readRDS("~/Box/organicseed_adoption/df.rds") 
# Let's narrow this down to the variables of interest
df <- df %>% 
  filter(Region != "International", maincrop != "Unknown") %>% 
  select(maincrop, OGseed_percent_maincrop, IncreasedOGSeed,
         VegiAcreage, FieldAcreage, ForageAcreage, # Total acres for each crop
         OGVegiSeed, OGFieldSeed, OGForageSeed, # % organic seed used
         TotalAcreage, 
         OGVarietiesSuccessn, 
         OGSeedOGIntegrityn, # Belief that organic seed is important
         CertifierRequest,
         CertifierRequestn, # Certifier has requested
         survey_year, # year variables (funding)
         cropcat_n, 
         Region, # regional variables (region and seed production)
         contains("barrier"), # barriers
         SeedSourceOwn,
         contains("AddSteps")) 

summary(df$OGseed_percent_maincrop)
table(df$CertifierRequest[df$IncreasedOGSeed != "Already at 100%"])
summary(df$CertifierRequestn[df$IncreasedOGSeed != "Already at 100%"])
summary(df$barrier_availability)
summary(df$barrier_lackdesiredTraits)
summary(df$barrier_saveOwn)
summary(df$SeedSourceOwn)
summary(df$barrier_buyerReq)
summary(df$barrier_distrustQuality)
summary(df$TotalAcreage)
summary(df$cropcat_n)

table(is.na(df$barrier_availability), df$survey_year)
table(is.na(df$TotalAcreage), df$survey_year)


# Where are the NAs in the data?
aggr(df, col=c('navyblue','red'), 
     numbers=TRUE, sortVars=TRUE, 
     labels=names(df), cex.axis=.7, gap=1, 
     ylab=c("Histogram of missing data","Pattern"))


# TABLE2: Make a table of basic distributions of each variable ----
df11 <- filter(df, survey_year == 2011)
df16 <- filter(df, survey_year == 2016)
df21 <- filter(df, survey_year == 2021)

variable_df <- data.frame(
  Questions = c("Last year, what approximate percentage of total acreage of [crops] was planted with certified organic seed?",
                #"Over the last 3 years have you increased or decreased the percentage of organic seed that you use in for your [crops]",
                "Organic seed is important to the success of organic food production",
                "Has your certifier requested that you take greater steps to source organic seed? (% 'Yes' for those who are not already compliant)",
                "Please indicate the degree to which the following was a factor to NOT use organic seed:",
                "      Specific variety not available as organic seed",
                "      Lack of desirable genetic traits",
                "      Processor (buyer) requires or supplies varieties that are not available organically",
                "      Distrust of organic seed quality",
                "      Saves own seed",
                "Total Acreage",
                "      Vegetable Crop Acreage",
                "      Field Crop Acreage",
                "      Forage Crop Acreage",
                "Number of crop categories",
                "Number of respondents"
                ),
                
  mean2011 = c(
           round(mean(df11$OGseed_percent,na.rm = T), 2),
           #round(table(df11$IncreasedOGSeed)[3]/sum(table(df11$IncreasedOGSeed)[c(2,3)]), 2),
           round(mean(df11$OGVarietiesSuccessn, na.rm = T), 2),
           round(table(df11$CertifierRequest)[2]/sum(table(df11$IncreasedOGSeed)[c(2,3)]), 2),
           "",
           round(mean(df11$barrier_availability, na.rm = T), 2),
           round(mean(df11$barrier_lackdesiredTraits, na.rm = T), 2),
           round(mean(df11$barrier_buyerReq, na.rm = T), 2),
           round(mean(df11$barrier_distrustQuality, na.rm = T), 2),
           round(mean(df11$barrier_saveOwn, na.rm = T), 2),
           round(mean(df11$TotalAcreage[df11$TotalAcreage != 0], na.rm = T), 2),
           round(mean(df11$VegiAcreage[df11$VegiAcreage != 0], na.rm = T), 2),
           round(mean(df11$FieldAcreage[df11$FieldAcreage != 0], na.rm = T), 2),
           round(mean(df11$ForageAcreage[df11$ForageAcreage != 0], na.rm = T), 2),
           round(mean(df11$cropcat_n), 2),
           nrow(df11)
           ),
           
  mean2016 = c(
          round(mean(df16$OGseed_percent,na.rm = T), 2),
          #round(table(df16$IncreasedOGSeed)[3]/sum(table(df16$IncreasedOGSeed)[c(2,3)]), 2),
          round(mean(df16$OGVarietiesSuccessn, na.rm = T), 2),
          round(table(df16$CertifierRequest)[2]/sum(table(df16$IncreasedOGSeed)[c(2,3)]), 2),
          "",
          round(mean(df16$barrier_availability, na.rm = T), 2),
          round(mean(df16$barrier_lackdesiredTraits, na.rm = T), 2),
          round(mean(df16$barrier_buyerReq, na.rm = T), 2),
          round(mean(df16$barrier_distrustQuality, na.rm = T), 2),
          round(mean(df16$barrier_saveOwn, na.rm = T), 2),
          round(mean(df16$TotalAcreage[df16$TotalAcreage != 0], na.rm = T), 2),
          round(mean(df16$VegiAcreage[df16$VegiAcreage != 0], na.rm = T), 2),
          round(mean(df16$FieldAcreage[df16$FieldAcreage != 0], na.rm = T), 2),
          round(mean(df16$ForageAcreage[df16$ForageAcreage != 0], na.rm = T), 2),
          round(mean(df16$cropcat_n), 2),
          nrow(df16)
          ),
  
  mean2021 = c(
          round(mean(df21$OGseed_percent,na.rm = T), 2),
          #round(table(df21$IncreasedOGSeed)[3]/sum(table(df21$IncreasedOGSeed)[c(2,3)]), 2),
          round(mean(df21$OGVarietiesSuccessn, na.rm = T), 2),
          round(table(df21$CertifierRequest)[2]/sum(table(df21$IncreasedOGSeed)[c(2,3)]), 2),
          "",
          round(mean(df21$barrier_availability, na.rm = T), 2),
          round(mean(df21$barrier_lackdesiredTraits, na.rm = T), 2),
          round(mean(df21$barrier_buyerReq, na.rm = T), 2),
          round(mean(df21$barrier_distrustQuality, na.rm = T), 2),
          round(mean(df21$barrier_saveOwn, na.rm = T), 2),
          round(mean(df21$TotalAcreage[df21$TotalAcreage != 0], na.rm = T), 2),
          round(mean(df21$VegiAcreage[df21$VegiAcreage != 0], na.rm = T), 2),
          round(mean(df21$FieldAcreage[df21$FieldAcreage != 0], na.rm = T), 2),
          round(mean(df21$ForageAcreage[df21$ForageAcreage != 0], na.rm = T), 2),
          round(mean(df21$cropcat_n), 2),
          nrow(df21)
             )
)

# Already at 100%
round(table(df11$IncreasedOGSeed)[1]/sum(table(df11$IncreasedOGSeed)[1:3]), 2)
round(table(df16$IncreasedOGSeed)[1]/sum(table(df16$IncreasedOGSeed)[1:3]), 2)
round(table(df21$IncreasedOGSeed)[1]/sum(table(df21$IncreasedOGSeed)[1:3]), 2)

write.csv(variable_df, "~/Documents/Davis/R-Projects/organicseed_adoption/tables/table1a.csv", row.names = F)
# Other parts of table 1 are in 03_innovation_level_data: line 33 for funding and supplychain_yearonly for supply chain

# FIGURES FOR PAPER -----
greens <- colorRampPalette(c("lightgrey", "#55752f"))(8) # 2014-2018
reds <- colorRampPalette(c("lightgrey", "#90353b"))(8) # 2002-2008
blues <- colorRampPalette(c("lightgrey", "#1A476f"))(8) # 2009-2013
oranges <- colorRampPalette(c("lightgrey", "#cc7000"))(8)
purples <- colorRampPalette(c("lightgrey", "#702963"))(8)

# Organic seed adoption ----

df %>% 
  mutate(survey_year = case_when(
    survey_year == 2011 ~ 2010,
    survey_year == 2016 ~ 2015,
    survey_year == 2021 ~ 2020,
  )) %>% 
  group_by(survey_year, maincrop, Region) %>% 
  mutate(avg_percent = mean(OGseed_percent_maincrop, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(Region, maincrop) %>% 
  mutate(crop_percent = round(mean(OGseed_percent_maincrop, na.rm = T))) %>% 
  select(survey_year, maincrop, Region, avg_percent, crop_percent) %>% 
  unique() %>% 
  arrange(survey_year, maincrop, Region) %>% 
  mutate(crop_percent = ifelse(duplicated(crop_percent), NA, crop_percent)) %>% 
  ggplot(aes(x = Region,
             y = crop_percent, label = paste0("x̅ = ", crop_percent),
             fill = factor(survey_year)))+
  geom_col(aes(y = avg_percent), width = .65, position = "dodge") +
  geom_point(size = 1) +
  geom_text(position=position_dodge(width=0.9), size = 2.5, hjust = 0,vjust = -.6) +
  scale_fill_manual(values = c(greens[5], reds[5], blues[5])) + 
  theme_linedraw() +
  theme_minimal() +
  facet_wrap(~maincrop) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45,hjust=1)) +
  labs(#x = "Survey year", 
    y = "Average percent of land\nplanted to organic seed", 
    #title = "Organic seed use by organic growers",
    fill = "")

ggsave("tables/figure3.png", height = 3.5, width = 7, dpi = 800)

# Self-reported increase ----
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

# Barriers ----
barrier_eachyr <- df %>% 
  pivot_longer(cols = c(barrier_seedTreatlack, barrier_buyerReq,
                        barrier_insuffQuantity,barrier_distrustQuality,
                        barrier_saveOwn), 
               names_to = "barrier_type", values_to = "barrier_strength") %>% 
  group_by(survey_year, barrier_type, barrier_strength) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(survey_year, barrier_type) %>%
  mutate(total = sum(n)) %>% 
  filter(barrier_strength == 3 | barrier_strength == 4) %>% 
  mutate(n_both = sum(n)) %>% 
  mutate(Percent = round(100*(n_both/total), 2)) %>% 
  ungroup() %>% 
  select(-barrier_strength, -n) %>% 
  unique()

order <- barrier_eachyr %>% 
  filter(survey_year == 2016) %>% 
  arrange(-Percent) 

pbarrier <- barrier_eachyr %>% 
  ggplot(aes(y = Percent, fill = factor(survey_year),
             x = forcats::fct_rev(factor(barrier_type, levels = order$barrier_type)),
             label = paste0("n = ", n_both))) +
  geom_col(position = "dodge") +
  geom_text(position=position_dodge(width=0.9), size = 3, hjust = 1.2, vjust = .5) +
  coord_flip() +
  scale_fill_manual(values = c(reds[5], blues[5], greens[5])) + 
  labs(x = "", fill = "", y = "Percent of respondents") +
  theme_linedraw() ; pbarrier

buyerreq <- df %>% 
  group_by(survey_year, maincrop, Region) %>% 
  summarize(barrier_buyerReq_avg = mean(barrier_buyerReq, na.rm = T),
            barrier_buyerReq_sd = sd(barrier_buyerReq, na.rm = T))
lackdesiredTraits <- df %>% 
  group_by(survey_year, maincrop, Region) %>% 
  summarize(barrier_lackdesiredTraits_avg = mean(barrier_lackdesiredTraits, na.rm = T),
            barrier_lackdesiredTraits_sd = sd(barrier_lackdesiredTraits, na.rm = T))
availability <- df %>% 
  group_by(survey_year, maincrop, Region) %>% 
  summarize(barrier_availability_avg = mean(barrier_availability, na.rm = T),
            barrier_availability_sd = sd(barrier_availability, na.rm = T))

df %>% 
  mutate(survey_year = case_when(
    survey_year == 2011 ~ 2010,
    survey_year == 2016 ~ 2015,
    survey_year == 2021 ~ 2020,
  )) %>% 
  #group_by(survey_year, maincrop, Region) %>% 
  #summarize(barrier_buyerReq_avg = mean(barrier_buyerReq, na.rm = T)) %>% 
  ggplot(aes(x = Region,
             fill = factor(survey_year),
             y = barrier_buyerReq))+
  geom_boxplot(position = "dodge") +
  scale_fill_manual(values = c(greens[5], reds[5], blues[5])) + 
  theme_linedraw() +
  theme_minimal() +
  facet_wrap(~maincrop) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45,hjust=1)) +
  labs(#x = "Survey year", 
    y = "Average percent of land\nplanted to organic seed", 
    #title = "Organic seed use by organic growers",
    fill = "")

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

# Certifier behavior ----

table(df$CertifierRequestn, df$survey_year)
round(100*(prop.table(table(df$CertifierRequestn, df$survey_year), margin = 2)), 1)

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

# Combining plots for Figure 3/Supplemental ----
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