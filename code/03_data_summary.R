library(tidyverse)
library(lubridate)
library(ggplot2)
library(kableExtra)
# This is the dataframe where I have identified main crops of growers, so each observation is only one grower
df <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds")

## Get everything of interest
df <- df %>% 
  filter(Region != "International", maincrop != "Unknown") %>% 
  select(maincrop, OGseed_percent_maincrop, IncreasedOGSeed,
         VegiAcreage, FieldAcreage, ForageAcreage, 
         OGVegiSeed, OGFieldSeed, OGForageSeed, 
         TotalAcreage, 
         OGSeedOGIntegrity, 
         OGSeedOGIntegrityf, 
         OGSeedOGIntegrityn, 
         OGVarietiesSuccess, 
         OGVarietiesSuccessf, 
         OGVarietiesSuccessn, 
         OG_value,
         CertifierRequest,
         CertifierRequestn, 
         survey_year, 
         cropcat_n, 
         Region, 
         contains("barrier"), 
         noncommercial_seedsource,
         contains("AddSteps")) 

df.full <- select(df, OGseed_percent_maincrop,
                  #barrier_buyerReq,
                  barrier_insuffQuantity,
                  barrier_lackdesiredTraits,
                  barrier_availability, 
                  barrier_seedTreatlack,
                  OG_value,
                  CertifierRequestn,
                  maincrop, survey_year, Region, 
                  TotalAcreage, cropcat_n, 
                  VegiAcreage, FieldAcreage, ForageAcreage,
                  noncommercial_seedsource) %>% 
  na.omit() %>% 
  mutate(OGseed_percent_maincrop_prop = OGseed_percent_maincrop/100)

df11 <- filter(df.full, survey_year == 2011)
df16 <- filter(df.full, survey_year == 2016)
df21 <- filter(df.full, survey_year == 2021)


# Table 1  (Manual) ----
table1.1 <- read.csv('~/Documents/Davis/R-Projects/organicseed_adoption/tables/table1_manual.csv') 

kbl(table1.1, booktabs = T, caption = 'Organic seed grower survey questions, key variables, and hypotheses', align = c('l', 'l', rep('c', 3))) %>% kable_styling(latex_options = 'scale_down') %>% row_spec(2, hline_after = T) %>% row_spec(5, hline_after = T) %>% row_spec(6, hline_after = T) %>% row_spec(26, hline_after = T) %>% row_spec(27, hline_after = T)


# ---- Text summaries ----
summary(df.full$OGseed_percent_maincrop)
table(df.full$CertifierRequestn[df.full$IncreasedOGSeed != "Already at 100%" | df.full$OGseed_percent_maincrop != 100])
summary(df.full$CertifierRequestn[df.full$IncreasedOGSeed != "Already at 100%" | df.full$OGseed_percent_maincrop != 100])
summary(df.full$barrier_availability)
summary(df.full$barrier_lackdesiredTraits)
summary(df.full$barrier_insuffQuantity)
summary(df.full$barrier_seedTreatlack)
summary(df.full$barrier_buyerReq)
summary(df.full$barrier_distrustQuality)
summary(df.full$noncommercial_seedsource)
summary(df.full$TotalAcreage)
summary(df.full$cropcat_n)
summary(df.full$YearsCertified)

### Additional steps ----

get_add_steps <- function(add_step){
  tbl <- df %>% 
    select(colnames(df.full)[-c(16,17)], {{add_step}}) %>% 
    filter(CertifierRequestn == 1) %>% 
    na.omit() %>% 
    group_by(survey_year, {{add_step}}) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(prop = round(100*(n/sum(n))), .by = survey_year) %>% 
    filter({{add_step}} == "Yes")
  return(tbl)
}
catalog <- get_add_steps(AddStepsCat)
seedfinder <- get_add_steps(AddStepsSeedFinder)
trial <- get_add_steps(AddStepsTrial)
#get_add_steps(AddStepsTimely)
contract <- get_add_steps(AddStepsContract)

# ---- Table 2: Make a table of basic distributions of each variable ----

variable_df <- data.frame(
  Questions = c("Proportion of acreage planted to organic seed",
                "Barrier: Availability",
                "Barrier: Quality",
                "Barrier: Quantity",
                "Barrier: Form",
                "Values organic seed",
                "Farm size (acres)",
                "      Vegetable Crop Acreage",
                "      Field Crop Acreage",
                "      Forage Crop Acreage",
                "Crop diversity",
                "Certifiers request organic seed use",
                "      Research more than three seed catalogs",
                #"      Research the Organic Seed Finder website",
                "      Conduct trials of available organic varieties",
                "      Contract organic seed production",
                "Seed saved or traded (%)",
                #"Years certified",
                "Number of respondents (complete data)"
  ),
                
  mean2011 = c(
    round(mean(df11$OGseed_percent_maincrop_prop,na.rm = T), 2),
    round(mean(df11$barrier_availability, na.rm = T), 2),
    round(mean(df11$barrier_lackdesiredTraits, na.rm = T), 2),
    round(mean(df11$barrier_insuffQuantity, na.rm = T), 2),
    round(mean(df11$barrier_seedTreatlack, na.rm = T), 2),
    round(mean(df11$OG_value, na.rm = T), 2),
    round(mean(df11$TotalAcreage[df11$TotalAcreage != 0], na.rm = T), 2),
    round(mean(df11$VegiAcreage[df11$VegiAcreage != 0], na.rm = T), 2),
    round(mean(df11$FieldAcreage[df11$FieldAcreage != 0], na.rm = T), 2),
    round(mean(df11$ForageAcreage[df11$ForageAcreage != 0], na.rm = T), 2),
    round(mean(df11$cropcat_n), 2),
    round(table(df$CertifierRequest[df$survey_year == 2011])[2]/sum(table(df$IncreasedOGSeed[df$survey_year == 2011])[c(2,3)]), 2),
    catalog$prop[catalog$survey_year == 2011],
    #seedfinder$prop[seedfinder$survey_year == 2011],
    trial$prop[trial$survey_year == 2011],
    NA,
    round(mean(df11$noncommercial_seedsource, na.rm = T), 2),
    #round(mean(df11$YearsCertified, na.rm = T), 2),
    nrow(df11)
  ),
           
  mean2016 = c(
    round(mean(df16$OGseed_percent_maincrop_prop,na.rm = T), 2),
    round(mean(df16$barrier_availability, na.rm = T), 2),
    round(mean(df16$barrier_lackdesiredTraits, na.rm = T), 2),
    round(mean(df16$barrier_insuffQuantity, na.rm = T), 2),
    round(mean(df16$barrier_seedTreatlack, na.rm = T), 2),
    round(mean(df16$OG_value, na.rm = T), 2),
    round(mean(df16$TotalAcreage[df16$TotalAcreage != 0], na.rm = T), 2),
    round(mean(df16$VegiAcreage[df16$VegiAcreage != 0], na.rm = T), 2),
    round(mean(df16$FieldAcreage[df16$FieldAcreage != 0], na.rm = T), 2),
    round(mean(df16$ForageAcreage[df16$ForageAcreage != 0], na.rm = T), 2),
    round(mean(df16$cropcat_n, na.rm = T), 2),
    round(table(df$CertifierRequest[df$survey_year == 2016])[2]/sum(table(df$IncreasedOGSeed[df$survey_year == 2016])[c(2,3)]), 2),
    catalog$prop[catalog$survey_year == 2016],
    #seedfinder$prop[seedfinder$survey_year == 2016],
    trial$prop[trial$survey_year == 2016],
    contract$prop[contract$survey_year == 2016],
    round(mean(df16$noncommercial_seedsource, na.rm = T), 2),
    #round(mean(df16$YearsCertified, na.rm = T), 2),
    nrow(df16)
  ),
  
  mean2021 = c(
    round(mean(df21$OGseed_percent_maincrop_prop,na.rm = T), 2),
    round(mean(df21$barrier_availability, na.rm = T), 2),
    round(mean(df21$barrier_lackdesiredTraits, na.rm = T), 2),
    round(mean(df21$barrier_insuffQuantity, na.rm = T), 2),
    round(mean(df21$barrier_seedTreatlack, na.rm = T), 2),
    round(mean(df21$OG_value, na.rm = T), 2),
    round(mean(df21$TotalAcreage[df21$TotalAcreage != 0], na.rm = T), 2),
    round(mean(df21$VegiAcreage[df21$VegiAcreage != 0], na.rm = T), 2),
    round(mean(df21$FieldAcreage[df21$FieldAcreage != 0], na.rm = T), 2),
    round(mean(df21$ForageAcreage[df21$ForageAcreage != 0], na.rm = T), 2),
    round(mean(df21$cropcat_n, na.rm = T), 2),
    round(table(df$CertifierRequest[df$survey_year == 2021])[2]/sum(table(df$IncreasedOGSeed[df$survey_year == 2021])[c(2,3)]), 2),
    catalog$prop[catalog$survey_year == 2021],
    #seedfinder$prop[seedfinder$survey_year == 2021],
    trial$prop[trial$survey_year == 2021],
    contract$prop[contract$survey_year == 2021],
    round(mean(df21$noncommercial_seedsource, na.rm = T), 2),
    #round(mean(df21$YearsCertified, na.rm = T), 2),
    nrow(df21)
  ),
  
    sd2011 = c(
      round(sd(df11$OGseed_percent_maincrop_prop,na.rm = T), 2),
      round(sd(df11$barrier_availability, na.rm = T), 2),
      round(sd(df11$barrier_lackdesiredTraits, na.rm = T), 2),
      round(sd(df11$barrier_insuffQuantity, na.rm = T), 2),
      round(sd(df11$barrier_seedTreatlack, na.rm = T), 2),
      round(sd(df11$OG_value, na.rm = T), 2),
      round(sd(df11$TotalAcreage[df11$TotalAcreage != 0], na.rm = T), 2),
      round(sd(df11$VegiAcreage[df11$VegiAcreage != 0], na.rm = T), 2),
      round(sd(df11$FieldAcreage[df11$FieldAcreage != 0], na.rm = T), 2),
      round(sd(df11$ForageAcreage[df11$ForageAcreage != 0], na.rm = T), 2),
      round(sd(df11$cropcat_n), 2),
      "","","","",#"",
      round(sd(df11$noncommercial_seedsource, na.rm = T), 2),
      #round(sd(df11$YearsCertified, na.rm = T), 2),
      nrow(df11)
    ),
           
  sd2016 = c(
    round(sd(df16$OGseed_percent_maincrop_prop,na.rm = T), 2),
    round(sd(df16$barrier_availability, na.rm = T), 2),
    round(sd(df16$barrier_lackdesiredTraits, na.rm = T), 2),
    round(sd(df16$barrier_insuffQuantity, na.rm = T), 2),
    round(sd(df16$barrier_seedTreatlack, na.rm = T), 2),
    round(sd(df16$OG_value, na.rm = T), 2),
    round(sd(df16$TotalAcreage[df16$TotalAcreage != 0], na.rm = T), 2),
    round(sd(df16$VegiAcreage[df16$VegiAcreage != 0], na.rm = T), 2),
    round(sd(df16$FieldAcreage[df16$FieldAcreage != 0], na.rm = T), 2),
    round(sd(df16$ForageAcreage[df16$ForageAcreage != 0], na.rm = T), 2),
    round(sd(df16$cropcat_n, na.rm = T), 2),
    "","","","",#"",
    round(sd(df16$noncommercial_seedsource, na.rm = T), 2),
    #round(sd(df16$YearsCertified, na.rm = T), 2),
    nrow(df16)
  ),
  
  sd2021 = c(
    round(sd(df21$OGseed_percent_maincrop_prop,na.rm = T), 2),
    round(sd(df21$barrier_availability, na.rm = T), 2),
    round(sd(df21$barrier_lackdesiredTraits, na.rm = T), 2),
    round(sd(df21$barrier_insuffQuantity, na.rm = T), 2),
    round(sd(df21$barrier_seedTreatlack, na.rm = T), 2),
    round(sd(df21$OG_value, na.rm = T), 2),
    round(sd(df21$TotalAcreage[df21$TotalAcreage != 0], na.rm = T), 2),
    round(sd(df21$VegiAcreage[df21$VegiAcreage != 0], na.rm = T), 2),
    round(sd(df21$FieldAcreage[df21$FieldAcreage != 0], na.rm = T), 2),
    round(sd(df21$ForageAcreage[df21$ForageAcreage != 0], na.rm = T), 2),
    round(sd(df21$cropcat_n, na.rm = T), 2),
    "","","","",#"",
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
variable_df[13,2:4] <- str_remove_all(variable_df[13,2:4], ' \\(\\)')
variable_df[14,2:4] <- str_remove_all(variable_df[14,2:4], ' \\(\\)')
variable_df[15,2:4] <- str_remove_all(variable_df[15,2:4], ' \\(\\)')
variable_df[17,2:4] <- str_remove_all(variable_df[17,2:4], ' \\(\\d{3}\\)')

# Already at 100%
df11 <- filter(df, survey_year == 2011)
df16 <- filter(df, survey_year == 2016)
df21 <- filter(df, survey_year == 2021)
round(table(df11$IncreasedOGSeed)[1]/sum(table(df11$IncreasedOGSeed)[1:3]), 2)
round(table(df16$IncreasedOGSeed)[1]/sum(table(df16$IncreasedOGSeed)[1:3]), 2)
round(table(df21$IncreasedOGSeed)[1]/sum(table(df21$IncreasedOGSeed)[1:3]), 2)

write.csv(variable_df, "~/Documents/Davis/R-Projects/organicseed_adoption/tables/table2.csv", row.names = F)
