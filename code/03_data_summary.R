library(VIM)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(kableExtra)
# This is the dataframe where I have identified main crops of growers, so each observation is only one grower
df <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds")

df <- df %>% 
  filter(Region != "International", maincrop != "Unknown") %>% 
  select(maincrop, OGseed_percent_maincrop, IncreasedOGSeed,
         VegiAcreage, FieldAcreage, ForageAcreage, # Total acres for each crop
         OGVegiSeed, OGFieldSeed, OGForageSeed, # % organic seed used
         TotalAcreage, 
         OGSeedOGIntegrity, 
         OGSeedOGIntegrityf, 
         OGSeedOGIntegrityn, # Belief that organic seed is important
         OGVarietiesSuccess, 
         OGVarietiesSuccessf, 
         OGVarietiesSuccessn, # Success
         CertifierRequest,
         CertifierRequestn, # Certifier has requested
         survey_year, # year variables (funding)
         cropcat_n, 
         Region, # regional variables (region and seed production)
         contains("barrier"), # barriers
         noncommercial_seedsource,
         contains("AddSteps"),
         YearsCertified, 
         data_coll_method) 
table(df$survey_year)
df.full <- select(df, OGseed_percent_maincrop,
                  OGseed_percent_maincrop, 
                  OGSeedOGIntegrityn,
                  OGVarietiesSuccessn,
                  CertifierRequestn,
                  maincrop, survey_year, Region, 
                  TotalAcreage, cropcat_n, 
                  VegiAcreage, FieldAcreage, ForageAcreage,
                  noncommercial_seedsource,
                  #YearsCertified
                  barrier_buyerReq,
                  barrier_insuffQuantity,
                  barrier_lackdesiredTraits,
                  barrier_availability, 
                  data_coll_method) %>% 
  na.omit() %>% 
  mutate(OGseed_percent_maincrop_prop = OGseed_percent_maincrop/100,
         CVseed_percent_maincrop_prop = 1-OGseed_percent_maincrop_prop)

table(df.full$survey_year)
table(df.full$survey_year, df.full$data_coll_method)

df11 <- filter(df.full, survey_year == 2011)
df16 <- filter(df.full, survey_year == 2016)
df21 <- filter(df.full, survey_year == 2021)

df$AddStepsOther[!(df$AddStepsOther %in% c("", "null")) & !is.na(df$AddStepsOther)]
# Figure 2 grower text:
df %>% 
  group_by(survey_year, Region) %>% 
  count()

# Table 1 test ----
table1.1 <- read.csv('~/Documents/Davis/R-Projects/organicseed_adoption/tables/table1_manual.csv') 

kbl(table1.1, booktabs = T, caption = 'Organic seed grower survey questions, key variables, and hypotheses', align = c('l', 'l', rep('c', 3))) %>% kable_styling(latex_options = 'scale_down') %>% row_spec(2, hline_after = T) %>% row_spec(5, hline_after = T) %>% row_spec(6, hline_after = T) %>% row_spec(26, hline_after = T) %>% row_spec(27, hline_after = T)


# ---- Text summaries ----
summary(df.full$OGseed_percent_maincrop)
table(df.full$CertifierRequestn[df.full$IncreasedOGSeed != "Already at 100%" | df.full$OGseed_percent_maincrop != 100])
summary(df.full$CertifierRequestn[df.full$IncreasedOGSeed != "Already at 100%" | df.full$OGseed_percent_maincrop != 100])
summary(df.full$barrier_availability)
summary(df.full$barrier_lackdesiredTraits)
summary(df.full$barrier_saveOwn)
summary(df.full$barrier_buyerReq)
summary(df.full$barrier_distrustQuality)
summary(df.full$noncommercial_seedsource)
summary(df.full$TotalAcreage)
summary(df.full$cropcat_n)
summary(df.full$YearsCertified)

# Where are the NAs in the data?
aggr(df, col=c('navyblue','red'), 
     numbers=TRUE, sortVars=TRUE, 
     labels=names(df), cex.axis=.7, gap=1, 
     ylab=c("Histogram of missing data","Pattern"))


# ---- Table 2: Make a table of basic distributions of each variable ----

variable_df <- data.frame(
  Questions = c("Proportion of acreage planted to conventional seed",
                "Barrier: Organic availability",
                "Barrier: Insufficient quantity of organic seed",
                "Barrier: Lack of desirable genetic traits",
                "Barrier: Buyer requirements",
                "Values organic seed",
                "Farm size (acres)",
                "      Vegetable Crop Acreage",
                "      Field Crop Acreage",
                "      Forage Crop Acreage",
                "Crop diversity",
                "Certifiers request organic seed use",
                "Seed saved or traded (%)",
                #"Years certified",
                "Number of respondents (complete data)"
  ),
                
  mean2011 = c(
    round(mean(df11$CVseed_percent_maincrop_prop,na.rm = T), 2),
    round(mean(df11$barrier_availability, na.rm = T), 2),
    round(mean(df11$barrier_insuffQuantity, na.rm = T), 2),
    round(mean(df11$barrier_lackdesiredTraits, na.rm = T), 2),
    round(mean(df11$barrier_buyerReq, na.rm = T), 2),
    round(mean(df11$OGSeedOGIntegrityn, na.rm = T), 2),
    round(mean(df11$TotalAcreage[df11$TotalAcreage != 0], na.rm = T), 2),
    round(mean(df11$VegiAcreage[df11$VegiAcreage != 0], na.rm = T), 2),
    round(mean(df11$FieldAcreage[df11$FieldAcreage != 0], na.rm = T), 2),
    round(mean(df11$ForageAcreage[df11$ForageAcreage != 0], na.rm = T), 2),
    round(mean(df11$cropcat_n), 2),
    round(table(df$CertifierRequest[df$survey_year == 2011])[2]/sum(table(df$IncreasedOGSeed[df$survey_year == 2011])[c(2,3)]), 2),
    round(mean(df11$noncommercial_seedsource, na.rm = T), 2),
    #round(mean(df11$YearsCertified, na.rm = T), 2),
    nrow(df11)
  ),
           
  mean2016 = c(
    round(mean(df16$CVseed_percent_maincrop_prop,na.rm = T), 2),
    round(mean(df16$barrier_availability, na.rm = T), 2),
    round(mean(df16$barrier_insuffQuantity, na.rm = T), 2),
    round(mean(df16$barrier_lackdesiredTraits, na.rm = T), 2),
    round(mean(df16$barrier_buyerReq, na.rm = T), 2),
    round(mean(df16$OGSeedOGIntegrityn, na.rm = T), 2),
    round(mean(df16$TotalAcreage[df16$TotalAcreage != 0], na.rm = T), 2),
    round(mean(df16$VegiAcreage[df16$VegiAcreage != 0], na.rm = T), 2),
    round(mean(df16$FieldAcreage[df16$FieldAcreage != 0], na.rm = T), 2),
    round(mean(df16$ForageAcreage[df16$ForageAcreage != 0], na.rm = T), 2),
    round(mean(df16$cropcat_n, na.rm = T), 2),
    round(table(df$CertifierRequest[df$survey_year == 2016])[2]/sum(table(df$IncreasedOGSeed[df$survey_year == 2016])[c(2,3)]), 2),
    round(mean(df16$noncommercial_seedsource, na.rm = T), 2),
    #round(mean(df16$YearsCertified, na.rm = T), 2),
    nrow(df16)
  ),
  
  mean2021 = c(
    round(mean(df21$CVseed_percent_maincrop_prop,na.rm = T), 2),
    round(mean(df21$barrier_availability, na.rm = T), 2),
    round(mean(df21$barrier_insuffQuantity, na.rm = T), 2),
    round(mean(df21$barrier_lackdesiredTraits, na.rm = T), 2),
    round(mean(df21$barrier_buyerReq, na.rm = T), 2),
    round(mean(df21$OGSeedOGIntegrityn, na.rm = T), 2),
    round(mean(df21$TotalAcreage[df21$TotalAcreage != 0], na.rm = T), 2),
    round(mean(df21$VegiAcreage[df21$VegiAcreage != 0], na.rm = T), 2),
    round(mean(df21$FieldAcreage[df21$FieldAcreage != 0], na.rm = T), 2),
    round(mean(df21$ForageAcreage[df21$ForageAcreage != 0], na.rm = T), 2),
    round(mean(df21$cropcat_n, na.rm = T), 2),
    round(table(df$CertifierRequest[df$survey_year == 2021])[2]/sum(table(df$IncreasedOGSeed[df$survey_year == 2021])[c(2,3)]), 2),
    round(mean(df21$noncommercial_seedsource, na.rm = T), 2),
    #round(mean(df21$YearsCertified, na.rm = T), 2),
    nrow(df21)
  ),
  
    sd2011 = c(
      round(sd(df11$CVseed_percent_maincrop_prop,na.rm = T), 2),
      round(sd(df11$barrier_availability, na.rm = T), 2),
      round(sd(df11$barrier_insuffQuantity, na.rm = T), 2),
      round(sd(df11$barrier_lackdesiredTraits, na.rm = T), 2),
      round(sd(df11$barrier_buyerReq, na.rm = T), 2),
      round(sd(df11$OGSeedOGIntegrityn, na.rm = T), 2),
      round(sd(df11$TotalAcreage[df11$TotalAcreage != 0], na.rm = T), 2),
      round(sd(df11$VegiAcreage[df11$VegiAcreage != 0], na.rm = T), 2),
      round(sd(df11$FieldAcreage[df11$FieldAcreage != 0], na.rm = T), 2),
      round(sd(df11$ForageAcreage[df11$ForageAcreage != 0], na.rm = T), 2),
      round(sd(df11$cropcat_n), 2),
      "",
      round(sd(df11$noncommercial_seedsource, na.rm = T), 2),
      #round(sd(df11$YearsCertified, na.rm = T), 2),
      nrow(df11)
    ),
           
  sd2016 = c(
    round(sd(df16$CVseed_percent_maincrop_prop,na.rm = T), 2),
    round(sd(df16$barrier_availability, na.rm = T), 2),
    round(sd(df16$barrier_insuffQuantity, na.rm = T), 2),
    round(sd(df16$barrier_lackdesiredTraits, na.rm = T), 2),
    round(sd(df16$barrier_buyerReq, na.rm = T), 2),
    round(sd(df16$OGSeedOGIntegrityn, na.rm = T), 2),
    round(sd(df16$TotalAcreage[df16$TotalAcreage != 0], na.rm = T), 2),
    round(sd(df16$VegiAcreage[df16$VegiAcreage != 0], na.rm = T), 2),
    round(sd(df16$FieldAcreage[df16$FieldAcreage != 0], na.rm = T), 2),
    round(sd(df16$ForageAcreage[df16$ForageAcreage != 0], na.rm = T), 2),
    round(sd(df16$cropcat_n, na.rm = T), 2),
    "",
    round(sd(df16$noncommercial_seedsource, na.rm = T), 2),
    #round(sd(df16$YearsCertified, na.rm = T), 2),
    nrow(df16)
  ),
  
  sd2021 = c(
    round(sd(df21$CVseed_percent_maincrop_prop,na.rm = T), 2),
    round(sd(df21$barrier_availability, na.rm = T), 2),
    round(sd(df21$barrier_insuffQuantity, na.rm = T), 2),
    round(sd(df21$barrier_lackdesiredTraits, na.rm = T), 2),
    round(sd(df21$barrier_buyerReq, na.rm = T), 2),
    round(sd(df21$OGSeedOGIntegrityn, na.rm = T), 2),
    round(sd(df21$TotalAcreage[df21$TotalAcreage != 0], na.rm = T), 2),
    round(sd(df21$VegiAcreage[df21$VegiAcreage != 0], na.rm = T), 2),
    round(sd(df21$FieldAcreage[df21$FieldAcreage != 0], na.rm = T), 2),
    round(sd(df21$ForageAcreage[df21$ForageAcreage != 0], na.rm = T), 2),
    round(sd(df21$cropcat_n, na.rm = T), 2),
    "",
    round(sd(df21$noncommercial_seedsource, na.rm = T), 2),
    #round(sd(df21$YearsCertified, na.rm = T), 2),
    nrow(df21)
  )
)

variable_df$stats2011 <- paste0(variable_df$mean2011, " (",
                                variable_df$sd2011, ")")
variable_df$stats2016 <- paste0(variable_df$mean2016, " (",
                                variable_df$sd2016, ")")
variable_df$stats2021 <- paste0(variable_df$mean2021, " (",
                                variable_df$sd2021, ")")

variable_df <- select(variable_df, Questions, stats2011, stats2016, 
                      stats2021)

variable_df[12,2:4] <- str_remove_all(variable_df[12,2:4], ' \\(\\)')
variable_df[14,2:4] <- str_remove_all(variable_df[14,2:4], ' \\(\\d{3}\\)')

# Already at 100%
df11 <- filter(df, survey_year == 2011)
df16 <- filter(df, survey_year == 2016)
df21 <- filter(df, survey_year == 2021)
round(table(df11$IncreasedOGSeed)[1]/sum(table(df11$IncreasedOGSeed)[1:3]), 2)
round(table(df16$IncreasedOGSeed)[1]/sum(table(df16$IncreasedOGSeed)[1:3]), 2)
round(table(df21$IncreasedOGSeed)[1]/sum(table(df21$IncreasedOGSeed)[1:3]), 2)

write.csv(variable_df, "~/Documents/Davis/R-Projects/organicseed_adoption/tables/table2.csv", row.names = F)








# ---- Supplemental Tables -- NOT USED IN PAPER ----
##
operation_subsys <- df.full %>% 
  group_by(survey_year, Region, maincrop) %>% 
  summarize(avg_acre = mean(TotalAcreage, na.rm = T),
            median_acre = median(TotalAcreage, na.rm = T),
            avg_src = mean(noncommercial_seedsource, na.rm = T),
            median_src = median(noncommercial_seedsource, na.rm = T),
            avg_cert = mean(CertifierRequestn, na.rm = T),
            median_cert = median(CertifierRequestn, na.rm = T))

write.csv(operation_subsys, 'tables/supplemental_subsystem_summary.csv', row.names = F)
operation_crop <- df.full %>% 
  group_by(survey_year, maincrop) %>% 
  summarize(avg_acre = mean(TotalAcreage, na.rm = T),
            median_acre = median(TotalAcreage, na.rm = T),
            avg_src = mean(noncommercial_seedsource, na.rm = T),
            median_src = median(noncommercial_seedsource, na.rm = T),
            avg_cert = mean(CertifierRequestn, na.rm = T),
            median_cert = median(CertifierRequestn, na.rm = T))

operation_reg <- df.full %>% 
  group_by(survey_year, Region) %>% 
  summarize(avg_acre = mean(TotalAcreage, na.rm = T),
            median_acre = median(TotalAcreage, na.rm = T),
            avg_src = mean(noncommercial_seedsource, na.rm = T),
            median_src = median(noncommercial_seedsource, na.rm = T),
            avg_cert = mean(CertifierRequestn, na.rm = T),
            median_cert = median(CertifierRequestn, na.rm = T))


# Supplemental Figure prep -- NOT USED IN PAPER ----
mako <- viridis::mako(n=9)
colors <- mako[c(2,5,8)]

# ---- Figure SM: DV distribution ----
##
df.full %>% 
  mutate(survey_year = case_when(
    survey_year == 2011 ~ 2010,
    survey_year == 2016 ~ 2015,
    survey_year == 2021 ~ 2020,
  )) %>% 
  ggplot(aes(x = CVseed_percent_maincrop_prop)) +
  geom_histogram() +
  theme_minimal() +
  labs(x = "Proportion of acres planted to conventional seed",
       y = "Count",
       title = "Distribution of conventional seed use")


### Option a: Average adoption ----
df.full %>% 
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
  geom_text(position=position_dodge(width=0.9), size = 2.5, hjust = -.3,vjust = -.4) +
  scale_fill_manual(values = colors) + 
  theme_linedraw() +
  theme_minimal() +
  facet_wrap(~maincrop) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45,hjust=1),
        text = element_text(family = "Times")) +
  labs(#x = "Survey year", 
    y = "Average percent of land\nplanted to organic seed", 
    #title = "Organic seed use by organic growers",
    fill = "")

### Option b: Distribution of adoption ----
df.full %>% 
  mutate(survey_year = case_when(
    survey_year == 2011 ~ 2010,
    survey_year == 2016 ~ 2015,
    survey_year == 2021 ~ 2020,
  )) %>% 
  ggplot(aes(x = Region,
             y = OGseed_percent_maincrop,
             fill = factor(survey_year),
             color = factor(survey_year)))+
  geom_boxplot() +
  scale_fill_manual(values = colors) + 
  scale_color_manual(values = rep('lightgray', 3)) + 
  theme_linedraw() +
  theme_minimal() +
  facet_wrap(~maincrop) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45,hjust=1),
        text = element_text(family = "Times")) +
  labs(#x = "Survey year", 
    y = "Average percent of land\nplanted to organic seed", 
    #title = "Organic seed use by organic growers",
    fill = "", color = "")


# ---- Supplemental figures ----
#
## Sentiment ----
df %>% 
  filter(!is.na(OGSeedOGIntegrityf)) %>% 
  group_by(OGSeedOGIntegrityf, survey_year) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(survey_year) %>% 
  mutate(Percent = 100*(n/sum(n))) %>% 
  ggplot(aes(fill = factor(survey_year),
             x = OGSeedOGIntegrityf,
             y = Percent)) +
  geom_col(position = "dodge") +
  #geom_text(position=position_dodge(width=0.9), size = 3, hjust = 1.2, vjust = .5) +
  coord_flip() +
  scale_fill_manual(values = c(colors)) + 
  labs(x = "", fill = "", y = "Percent of respondents") +
  theme_minimal() +
  theme(legend.position = "none")

df %>% 
  ggplot(aes(fill = factor(survey_year),
             x = Region,
             y = OGVarietiesSuccessn)) +
  geom_boxplot() +
  #geom_text(position=position_dodge(width=0.9), size = 3, hjust = 1.2, vjust = .5) +
  scale_fill_manual(values = c(greens[8], reds[8], blues[8])) + 
  labs(x = "", fill = "", y = "Percent of respondents") +
  theme_linedraw() +
  theme(legend.position = "none") +
  facet_wrap(~maincrop)

## Certifier behavior ----

table(df$CertifierRequestn, df$survey_year)
round(100*(prop.table(table(df$CertifierRequestn, df$survey_year), margin = 2)), 1)

request_noncompliant <- ifelse(df$IncreasedOGSeed == "Already at 100%",
                               NA, df$CertifierRequestn)
table(request_noncompliant, df$survey_year)
round(100*(prop.table(table(request_noncompliant, df$survey_year), margin = 2)), 1)

## Self-reported increase ----

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

## Barriers ----
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



## Farm size ----
df %>% 
  ggplot(aes(x = factor(survey_year), y = TotalAcreage,
             color = factor(survey_year))) +
  geom_boxplot() +
  labs(x = "", y = "Percent organic seed used", color = "") +
  scale_color_manual(values = c(greens[8],reds[8], blues[8])) +
  theme_linedraw() +
  theme(legend.position = "none") +
  ylim(c(0,1000))

## Size to acreage ----
df %>% 
  mutate(size_cat = case_when(
    TotalAcreage <= 7 ~ "Small (0-7)",
    TotalAcreage > 7 & TotalAcreage <= 64 ~ "Medium (8-64)",
    TotalAcreage > 64 & TotalAcreage <= 225 ~ "Large (65-225)",
    TotalAcreage >225 ~ "Extra large (225+)"
  )) %>% 
  group_by(size_cat) %>% 
  summarize(mean = mean(OGseed_percent_maincrop, na.rm =T),
            sd = sd(OGseed_percent_maincrop, na.rm =T),
            count = n())

ggplot(df, aes(x = TotalAcreage, y = OGseed_percent_maincrop)) +
  geom_point() +
  scale_x_log10()
