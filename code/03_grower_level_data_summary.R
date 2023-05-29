library(VIM)
library(tidyverse)
library(lubridate)
library(ggplot2)
# This is the dataframe where I have identified main crops of growers, so each observation is only one grower
df <- readRDS("~/Box/organicseed_adoption/df.rds") 

df <- df %>% 
  filter(Region != "International", maincrop != "Unknown") %>% 
  select(maincrop, OGseed_percent_maincrop, IncreasedOGSeed,
         VegiAcreage, FieldAcreage, ForageAcreage, # Total acres for each crop
         OGVegiSeed, OGFieldSeed, OGForageSeed, # % organic seed used
         TotalAcreage, 
         OGVarietiesSuccessn, 
         OGVarietiesSuccessf, 
         OGSeedOGIntegrityn, # Belief that organic seed is important
         CertifierRequest,
         CertifierRequestn, # Certifier has requested
         survey_year, # year variables (funding)
         cropcat_n, 
         Region, # regional variables (region and seed production)
         contains("barrier"), # barriers
         noncommercial_seedsource,
         contains("AddSteps"),
         YearsCertified) 

table(df$survey_year)

df11 <- filter(df, survey_year == 2011)
df16 <- filter(df, survey_year == 2016)
df21 <- filter(df, survey_year == 2021)

# Figure 2 grower text:
df %>% 
  group_by(survey_year, Region) %>% 
  count()

# ---- Text summaries ----
summary(df$OGseed_percent_maincrop)
table(df$CertifierRequestn[df$IncreasedOGSeed != "Already at 100%" | df$OGseed_percent_maincrop != 100])
summary(df$CertifierRequestn[df$IncreasedOGSeed != "Already at 100%" | df$OGseed_percent_maincrop != 100])
summary(df$barrier_availability)
summary(df$barrier_lackdesiredTraits)
summary(df$barrier_saveOwn)
summary(df$barrier_buyerReq)
summary(df$barrier_distrustQuality)
summary(df$noncommercial_seedsource)
summary(df$TotalAcreage)
summary(df$cropcat_n)
summary(df$YearsCertified)

# Where are the NAs in the data?
aggr(df, col=c('navyblue','red'), 
     numbers=TRUE, sortVars=TRUE, 
     labels=names(df), cex.axis=.7, gap=1, 
     ylab=c("Histogram of missing data","Pattern"))


# ---- Table 2a: Make a table of basic distributions of each variable ----

variable_df <- data.frame(
  Questions = c("Adoption: Last year, what approximate percentage of total acreage of [crops] was planted with certified organic seed? (% acreage)",
                "Operational resources: Total Acreage",
                "      Vegetable Crop Acreage",
                "      Field Crop Acreage",
                "      Forage Crop Acreage",
                "Operational practice: Non-commercial seed sourcing (% seeds)",
                "Attitudes/Legitimacy: Varieties bred for organic system management are important to the overall success of organic agriculture (1-5)",
                "Guidance: Has your certifier requested that you take greater steps to source organic seed? (% 'Yes' of those who are not already 100% compliant)"#,
                #"Years certified organic"
                ),
                
  mean2011 = c(
           round(mean(df11$OGseed_percent,na.rm = T), 2),
           round(mean(df11$TotalAcreage[df11$TotalAcreage != 0], na.rm = T), 2),
           round(mean(df11$VegiAcreage[df11$VegiAcreage != 0], na.rm = T), 2),
           round(mean(df11$FieldAcreage[df11$FieldAcreage != 0], na.rm = T), 2),
           round(mean(df11$ForageAcreage[df11$ForageAcreage != 0], na.rm = T), 2),
           round(mean(df11$noncommercial_seedsource, na.rm = T), 2),
           round(mean(df11$OGVarietiesSuccessn, na.rm = T), 2),
           round(table(df11$CertifierRequest[df11$IncreasedOGSeed != "Already at 100%" | df11$OGseed_percent_maincrop != 100])[2]/length(df11$CertifierRequestn[!is.na(df11$CertifierRequestn)]), 2)
           ),
           
  mean2016 = c(
           round(mean(df16$OGseed_percent,na.rm = T), 2),
           round(mean(df16$TotalAcreage[df16$TotalAcreage != 0], na.rm = T), 2),
           round(mean(df16$VegiAcreage[df16$VegiAcreage != 0], na.rm = T), 2),
           round(mean(df16$FieldAcreage[df16$FieldAcreage != 0], na.rm = T), 2),
           round(mean(df16$ForageAcreage[df16$ForageAcreage != 0], na.rm = T), 2),
           round(mean(df16$noncommercial_seedsource, na.rm = T), 2),
           round(mean(df16$OGVarietiesSuccessn, na.rm = T), 2),
           round(table(df16$CertifierRequest[df16$IncreasedOGSeed != "Already at 100%" | df16$OGseed_percent_maincrop != 100])[2]/length(df16$CertifierRequestn[!is.na(df16$CertifierRequestn)]), 2)
  ),
  
  mean2021 = c(
    round(mean(df21$OGseed_percent,na.rm = T), 2),
    round(mean(df21$TotalAcreage[df21$TotalAcreage != 0], na.rm = T), 2),
    round(mean(df21$VegiAcreage[df21$VegiAcreage != 0], na.rm = T), 2),
    round(mean(df21$FieldAcreage[df21$FieldAcreage != 0], na.rm = T), 2),
    round(mean(df21$ForageAcreage[df21$ForageAcreage != 0], na.rm = T), 2),
    round(mean(df21$noncommercial_seedsource, na.rm = T), 2),
    round(mean(df21$OGVarietiesSuccessn, na.rm = T), 2),
    round(table(df21$CertifierRequest[df21$IncreasedOGSeed != "Already at 100%" | df21$OGseed_percent_maincrop != 100])[2]/length(df21$CertifierRequestn[!is.na(df21$CertifierRequestn)]), 2)
  ),
    sd2011 = c(
           round(sd(df11$OGseed_percent,na.rm = T), 2),
           round(sd(df11$TotalAcreage[df11$TotalAcreage != 0], na.rm = T), 2),
           round(sd(df11$VegiAcreage[df11$VegiAcreage != 0], na.rm = T), 2),
           round(sd(df11$FieldAcreage[df11$FieldAcreage != 0], na.rm = T), 2),
           round(sd(df11$ForageAcreage[df11$ForageAcreage != 0], na.rm = T), 2),
           round(sd(df11$noncommercial_seedsource, na.rm = T), 2),
           round(sd(df11$OGVarietiesSuccessn, na.rm = T), 2),
           round(table(df11$CertifierRequest[df11$IncreasedOGSeed != "Already at 100%" | df11$OGseed_percent_maincrop != 100])[2]/length(df11$CertifierRequestn[!is.na(df11$CertifierRequestn)]), 2)
           ),
           
  sd2016 = c(
           round(sd(df16$OGseed_percent,na.rm = T), 2),
           round(sd(df16$TotalAcreage[df16$TotalAcreage != 0], na.rm = T), 2),
           round(sd(df16$VegiAcreage[df16$VegiAcreage != 0], na.rm = T), 2),
           round(sd(df16$FieldAcreage[df16$FieldAcreage != 0], na.rm = T), 2),
           round(sd(df16$ForageAcreage[df16$ForageAcreage != 0], na.rm = T), 2),
           round(sd(df16$noncommercial_seedsource, na.rm = T), 2),
           round(sd(df16$OGVarietiesSuccessn, na.rm = T), 2),
           round(table(df16$CertifierRequest[df16$IncreasedOGSeed != "Already at 100%" | df16$OGseed_percent_maincrop != 100])[2]/length(df16$CertifierRequestn[!is.na(df16$CertifierRequestn)]), 2)#,
          #round(sd(df16$YearsCertified, na.rm = T), 2)
  ),
  
  sd2021 = c(
    round(sd(df21$OGseed_percent,na.rm = T), 2),
    round(sd(df21$TotalAcreage[df21$TotalAcreage != 0], na.rm = T), 2),
    round(sd(df21$VegiAcreage[df21$VegiAcreage != 0], na.rm = T), 2),
    round(sd(df21$FieldAcreage[df21$FieldAcreage != 0], na.rm = T), 2),
    round(sd(df21$ForageAcreage[df21$ForageAcreage != 0], na.rm = T), 2),
    round(sd(df21$noncommercial_seedsource, na.rm = T), 2),
    round(sd(df21$OGVarietiesSuccessn, na.rm = T), 2),
    round(table(df21$CertifierRequest[df21$IncreasedOGSeed != "Already at 100%" | df21$OGseed_percent_maincrop != 100])[2]/length(df21$CertifierRequestn[!is.na(df21$CertifierRequestn)]), 2)
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

# Already at 100%
round(table(df11$IncreasedOGSeed)[1]/sum(table(df11$IncreasedOGSeed)[1:3]), 2)
round(table(df16$IncreasedOGSeed)[1]/sum(table(df16$IncreasedOGSeed)[1:3]), 2)
round(table(df21$IncreasedOGSeed)[1]/sum(table(df21$IncreasedOGSeed)[1:3]), 2)

write.csv(variable_df, "~/Documents/Davis/R-Projects/organicseed_adoption/tables/table2a.csv", row.names = F)
# Other parts of table 1 are in 03_innovation_level_data: line 33 for funding and supplychain_yearonly for supply chain


sm_table <- data.frame(
  'Questions' = c("Please indicate the degree to which the following was a factor to NOT use organic seed:",
                   "      Specific variety not available as organic seed",
                   "      Lack of desirable genetic traits",
                   "      Processor (buyer) requires or supplies varieties that are not available organically",
                   "      Distrust of organic seed quality"),
  'mean2011' = c("", round(mean(df11$barrier_availability, na.rm = T), 2),
                 round(mean(df11$barrier_lackdesiredTraits, na.rm = T), 2),
                 round(mean(df11$barrier_buyerReq, na.rm = T), 2),
                 round(mean(df11$barrier_distrustQuality, na.rm = T), 2)),
  'mean2016' = c("", round(mean(df16$barrier_availability, na.rm = T), 2),
                 round(mean(df16$barrier_lackdesiredTraits, na.rm = T), 2),
                 round(mean(df16$barrier_buyerReq, na.rm = T), 2),
                 round(mean(df16$barrier_distrustQuality, na.rm = T), 2)),
  'mean2021' = c("", round(mean(df21$barrier_availability, na.rm = T), 2),
                 round(mean(df21$barrier_lackdesiredTraits, na.rm = T), 2),
                 round(mean(df21$barrier_buyerReq, na.rm = T), 2),
                 round(mean(df21$barrier_distrustQuality, na.rm = T), 2)),
  'sd2011' = c("", round(sd(df11$barrier_availability, na.rm = T), 2),
                 round(sd(df11$barrier_lackdesiredTraits, na.rm = T), 2),
                 round(sd(df11$barrier_buyerReq, na.rm = T), 2),
                 round(sd(df11$barrier_distrustQuality, na.rm = T), 2)),
  'sd2016' = c("", round(sd(df16$barrier_availability, na.rm = T), 2),
                 round(sd(df16$barrier_lackdesiredTraits, na.rm = T), 2),
                 round(sd(df16$barrier_buyerReq, na.rm = T), 2),
                 round(sd(df16$barrier_distrustQuality, na.rm = T), 2)),
  'sd2021' = c("", round(sd(df21$barrier_availability, na.rm = T), 2),
                 round(sd(df21$barrier_lackdesiredTraits, na.rm = T), 2),
                 round(sd(df21$barrier_buyerReq, na.rm = T), 2),
                 round(sd(df21$barrier_distrustQuality, na.rm = T), 2))
)

sm_table$stats2011 <- paste0(sm_table$mean2011, " (",
                                sm_table$sd2011, ")")
sm_table$stats2016 <- paste0(sm_table$mean2016, " (",
                                sm_table$sd2016, ")")
sm_table$stats2021 <- paste0(sm_table$mean2021, " (",
                                sm_table$sd2021, ")")

sm_table <- select(sm_table, Questions, stats2011, stats2016, 
                      stats2021)

# ---- Text summaries ----
##
operation_subsys <- df %>% 
  group_by(survey_year, Region, maincrop) %>% 
  summarize(avg_acre = mean(TotalAcreage, na.rm = T),
            median_acre = median(TotalAcreage, na.rm = T),
            avg_src = mean(noncommercial_seedsource, na.rm = T),
            median_src = median(noncommercial_seedsource, na.rm = T),
            avg_cert = mean(CertifierRequestn, na.rm = T),
            median_cert = median(CertifierRequestn, na.rm = T))

operation_crop <- df %>% 
  group_by(survey_year, maincrop) %>% 
  summarize(avg_acre = mean(TotalAcreage, na.rm = T),
            median_acre = median(TotalAcreage, na.rm = T),
            avg_src = mean(noncommercial_seedsource, na.rm = T),
            median_src = median(noncommercial_seedsource, na.rm = T),
            avg_cert = mean(CertifierRequestn, na.rm = T),
            median_cert = median(CertifierRequestn, na.rm = T))

operation_reg <- df %>% 
  group_by(survey_year, Region) %>% 
  summarize(avg_acre = mean(TotalAcreage, na.rm = T),
            median_acre = median(TotalAcreage, na.rm = T),
            avg_src = mean(noncommercial_seedsource, na.rm = T),
            median_src = median(noncommercial_seedsource, na.rm = T),
            avg_cert = mean(CertifierRequestn, na.rm = T),
            median_cert = median(CertifierRequestn, na.rm = T))

## Figure prep ----
greens <- colorRampPalette(c("lightgrey", "#55752f"))(8) # 2014-2018
reds <- colorRampPalette(c("lightgrey", "#90353b"))(8) # 2002-2008
blues <- colorRampPalette(c("lightgrey", "#1A476f"))(8) # 2009-2013
oranges <- colorRampPalette(c("lightgrey", "#cc7000"))(8)
purples <- colorRampPalette(c("lightgrey", "#702963"))(8)

# ---- Figure S5. Organic seed adoption ----
##
### Option a: Average adoption ----
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
        axis.text.x = element_text(angle=45,hjust=1),
        text = element_text(family = "Times")) +
  labs(#x = "Survey year", 
    y = "Average percent of land\nplanted to organic seed", 
    #title = "Organic seed use by organic growers",
    fill = "")

ggsave("figures/s5_figure_opta.png", height = 3.5, width = 7, dpi = 800)

### Option b: Distribution of adoption ----
df %>% 
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
  scale_fill_manual(values = c(greens[4], reds[4], blues[4])) + 
  scale_color_manual(values = c(greens[8], reds[8], blues[8])) + 
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

ggsave("figures/s5_figure_opta.png", height = 3.5, width = 7, dpi = 800)


# ---- Supplemental figures ----
#
## Sentiment ----
df %>% 
  filter(!is.na(OGVarietiesSuccessf)) %>% 
  group_by(OGVarietiesSuccessf, survey_year) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(survey_year) %>% 
  mutate(Percent = 100*(n/sum(n))) %>% 
  ggplot(aes(fill = factor(survey_year),
             x = OGVarietiesSuccessf,
             y = Percent)) +
  geom_col(position = "dodge") +
  #geom_text(position=position_dodge(width=0.9), size = 3, hjust = 1.2, vjust = .5) +
  coord_flip() +
  scale_fill_manual(values = c(greens[8], reds[8], blues[8])) + 
  labs(x = "", fill = "", y = "Percent of respondents") +
  theme_linedraw() +
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



## See github here for older explorations of data