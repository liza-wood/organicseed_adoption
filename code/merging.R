# merging
library(dplyr)
library(stringr)

setwd("~/Documents/Davis/R-Projects/organicseed/")
codebook <- read.csv("data/codebook_3panel.csv")
setwd("~/Dropbox/SOSInteractiveSurvey/")
df11f <- readRDS("data2011_cleaned.rds") %>% select(codebook$qcode_11_16)
df16f <- readRDS("data2016_cleaned.rds") %>% select(codebook$qcode_11_16)
df21f <- readRDS("data2021_cleaned.rds")
zipcode <- data.table::fread("zipcode.csv")
getzipcode <- zipcode$state
names(getzipcode) <- zipcode$zip
df21f$State <- unname(getzipcode[as.character(df21f[,"Q04A"])])
df21$State <- unname(getzipcode[as.character(df21[,"Q04A"])])

for(i in 1:ncol(df21f)){
  for(j in 1:nrow(codebook)){
  colnames(df21f)[i] <- ifelse(colnames(df21f)[i] == codebook$qcode_21[j],
                               codebook$qcode_11_16[j], colnames(df21f)[i])
  }
}

df21f <- select(df21f, codebook$qcode_11_16)

df11f$survey_year = 2011
df16f$survey_year = 2016
df21f$survey_year = 2021

western <- toupper(c("CA", "OR", "WA", "ID", "NV", "AZ", "UT", "NM", "CO", "WY", "MT", 
             "AK", "HI","California", "Oregon", "Washington", "Idaho", "Nevada",
             "Arizona", "Utah", "New Mexico", "Colorado", "Wyoming", "Montana",
             "Alaska", "Hawaii"))
northcentral <- toupper(c("ND", "SD", "KS", "NE", "MN", "IA", "MO", "WI", "IL", "MI", 
                  "IN", "OH", "North Dakota", "South Dakota", "Kansas",
                  "Nebraska", "Minnesota", "Iowa", "Missouri", "Wisconsin", 
                  "Illinois", "Michigan", "Indiana", "Ohio"))
southern <- toupper(c("TX", "OK", "AR", "LA", "KY", "TN", "MS", "AL", "FL", "GA", "SC",
              "NC", "VA", "Texas", "Oklahoma", "Arkansas", "Louisiana", 
              "Kentucky", "Tennessee", "Mississippi", "Alabama", "Florida", 
              "Georgia", "South Carolina", "North Carolina", "Virginia"))
northeast <- toupper(c("WV", "MD", "DE", "NJ", "PA", "NY", "CT", "RI", "MA", "VT", 
               "NH", "ME", "West Virginia", "Maryland", "Delaware", 
               "New Jersey", "Pennsylvania", "New York", "Connecticut", 
               "Rhode Island", "Massachusetts", "Vermont", "New Hampshire", "Maine"))
missp_neast <- c("PENNSYLVANIA, 15535", "PENNSYLVANIA", "PENNSYLVANA", "PA.")
missp_ncentral <- c("WISCONCSIN", "WISCONSN", "WISCOSIN", "WTSCONSIN", "WISONSIN",
                    "WISCONSON", "OHIO AND MICHIGAN")
missp_west <- c("WASHINGTON STATE", "WASHINGTON, CALIFORNIA", "WYO.", "WASHINGTON,",
                "WASHINGTON AND IDAHO", "WA, OR", "WA.", "WA, CO, HI, CA, TX, GA, AZ",
                "WA AND RUSSIAN FEDERATION", "UTAH, ARKANSAS", "OR, CA")
df.full <- merge(df11f, df21f, all = T)
df.full <- merge(df.full, df16f, all = T)

table(is.na(df.full$State))

df.full <- df.full %>% 
  mutate(State = toupper(State)) %>% 
  mutate(Region = case_when(
    State %in% western | State %in% missp_west  ~ "West",
    State %in% northcentral | State %in% missp_ncentral ~ "North Central",
    State %in% southern ~ "South",
    State %in% northeast| State %in% missp_neast ~ "Northeast",
    T ~ NA_character_))

table(is.na(df.full$Region))
table(df.full$State[is.na(df$Region)])

write.csv(df.full, "~/Box/organicseed/merged_df_3panel.csv", row.names = F)

setwd("~/Documents/Davis/R-Projects/organicseed/")
codebook <- read.csv("data/codebook_2panel.csv")
setwd("~/Dropbox/SOSInteractiveSurvey/")
df11f <- readRDS("data2011_cleaned.rds") %>% select(codebook$qcode_11_16)
df16f <- readRDS("data2016_cleaned.rds") %>% select(codebook$qcode_11_16)

df.full <- merge(df11f, df16f, all = T)

df.full <- df.full %>% 
  mutate(State = toupper(State)) %>% 
  mutate(Region = case_when(
    State %in% western | State %in% missp_west  ~ "West",
    State %in% northcentral | State %in% missp_ncentral ~ "North Central",
    State %in% southern ~ "South",
    State %in% northeast| State %in% missp_neast ~ "Northeast",
    T ~ NA_character_))

write.csv(df.full, "~/Box/organicseed/merged_df_2panel.csv", row.names = F)
