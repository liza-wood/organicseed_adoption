library(dplyr)
library(betareg)
source('code/functions.R')
core_vars_notime <- c(#"Barrier: Availability",
  #"Barrier: Buyer requirements",
  #"Barrier: Quality",
  #"Barrier: Quantity",
  #"Barrier: Form",
  "Ability",
  "Duty",
  "Certifier request",
  "Farm size (log acres)",
  "Crop diversity", 
  "Seed saved or traded (\\%)",
  "Years certified organic",
  "Forage crops (vs. Field)",
  "Vegetable crops (vs. Field)",
  "Western region (vs. North Central)",
  "Northeastern region (vs. North Central)",
  "Southern region (vs. North Central)",
  "Ability * Duty")


df20 <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled20.rds")  %>% 
  select(OGseed_percent_maincrop_prop,
         OG_value_s,
         OG_value_minus1,
         barrier_all_rev_s,
         barrier_all_rev_minus1,
         CertifierRequestn,
         noncommercial_seedsource_s,
         TotalAcreage,
         acreage_log_s,
         cropcat_n_s,
         years_certified_s,
         maincrop,
         Region,
         survey_year,
         RespondentID) %>% 
  na.omit()

df21 <- read.csv("~/Dropbox/SOSInteractiveSurvey/NORA19_merged.csv")
omit_answers <- c("No answer", "no answer", "Partial complete", "partial complete",
                  "Skipped due to branching", "skipped due to branching")

df21_split <- df21[!df21$Q03C_1 %in% omit_answers | 
                     !df21$Q03C_2 %in% omit_answers,]
df21_split <- df21_split[df21_split$Q03C_1 != "0" &  df21_split$Q03C_2 != "0",]
split_ids <- df21_split$RespID

df20$split <- ifelse(df20$RespondentID %in% split_ids, T, F)
#table(df20$split)

df20 %>% 
  group_by(split) %>% 
  summarize(mean = mean(TotalAcreage),
            median = median(TotalAcreage))
t.test(x = df20$TotalAcreage[df20$split == T],
       y = df20$TotalAcreage[df20$split == F])
t.test(x = df20$OGseed_percent_maincrop_prop[df20$split == T],
       y = df20$OGseed_percent_maincrop_prop[df20$split == F])

s = 0.5
n = nrow(df20)
x = df20$OGseed_percent_maincrop_prop
rounded = (x*(n-1)+s)/n
df20$OGseed_percent_maincrop_prop <- (df20$OGseed_percent_maincrop_prop * (n-1) + s)/n
set.seed(19)
m_ix20 <- betareg(OGseed_percent_maincrop_prop ~ 
                    barrier_all_rev_s +
                    OG_value_s + 
                    barrier_all_rev_s*OG_value_s +
                    CertifierRequestn + 
                    acreage_log_s + 
                    cropcat_n_s +
                    noncommercial_seedsource_s +
                    years_certified_s + 
                    maincrop +
                    Region + split,
                  data = df20, link = "logit", link.phi = NULL, type = "ML")
summary(m_ix20)
saveRDS(m_ix20, 'tables/append_models_2020_split.rds')


# Panel
df <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds")
panel <- df[df$RespondentID %in% c('3571160026', '41315213'),]
panel$GrowVegis
panel$GrowField
panel$GrowForage
panel$FieldAcreage
panel$ForageAcreage
panel$OGFieldSeed
panel$OGForageSeed
panel$OGseed_percent_maincrop

panel$TotalAcreage
