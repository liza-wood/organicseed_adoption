# Merging together three years worth of data
library(dplyr)
library(stringr)

setwd("~/Documents/Davis/R-Projects/organicseed_adoption/")
codebook <- read.csv("data/codebook_3panel.csv")
setwd("~/Dropbox/SOSInteractiveSurvey/")
df11f <- readRDS("data2011_cleaned.rds") %>% 
  cbind(select(read.csv("2011SurveyData.csv", skip = 2, header = T), 
               NoOGSeedTreat, NoOGSeedProcessor,     
               NoOGSeedQuant, NoOGSeedSave, NoOGSeedQual,          
               NoOGSeedPrice, NoOGSeedVar , NoOGSeedTrait, SeedInterest,
               AddStepsOther)) %>% 
  select(codebook$qcode_11_16, IncreasedOGSeed)
df16f <- readRDS("data2016_cleaned.rds") %>% select(codebook$qcode_11_16, IncreasedOGVegSeed,
                                                    IncreasedOGFieldSeed, IncreasedOGForageSeed,
                                                    AddStepsTimely, AddStepsContract, AddStepsOther)
df21f <- readRDS("data2021_cleaned.rds")
zipcode <- data.table::fread("zipcode.csv")
getzipcode <- zipcode$state
names(getzipcode) <- zipcode$zip
df21f$State <- unname(getzipcode[as.character(df21f[,"Q04A"])])

# Up stream I see a few errors, I think the one who said certified in 205 means 2005
df21f$Q01A[df21f$YearsCertified > 100 & !is.na(df21f$YearsCertified)]
df21f$YearsCertified[df21f$Q01A == 205] <- 2021-2005
df21f$Q01A[df21f$Q01A == 205] <- 2005
# As for the 1908, which is probable when they established, I am going to assign them the upper limit for certification in their state
yrs <- df21f$Q01A[df21f$State %in% c("IA", "Iowa", "IOWA")] 
yrs <- as.numeric(str_remove_all(yrs, "No answer"))
sort(yrs)
df21f$YearsCertified[df21f$Q01A == 1908] <- 2021-1994
df21f$Q01A[df21f$Q01A == 1908] <- 1994

for(i in 1:ncol(df21f)){
  for(j in 1:nrow(codebook)){
  colnames(df21f)[i] <- ifelse(colnames(df21f)[i] == codebook$qcode_21[j],
                               codebook$qcode_11_16[j], colnames(df21f)[i])
  }
}

df21f <- select(df21f, codebook$qcode_11_16, 
                NoOGSeedTreat, NoOGSeedProcessor,
                NoOGSeedQuant, NoOGSeedSave, NoOGSeedQual,NoOGSeedPrice,
                NoOGSeedVar, NoOGSeedTrait,
                IncreasedOGVegSeed,
                IncreasedOGFieldSeed, IncreasedOGForageSeed,
                AddStepsTimely, AddStepsContract)

df11f$survey_year = 2011
df16f$survey_year = 2016
df21f$survey_year = 2021


df.full <- merge(df11f, df21f, all = T)
df.full <- merge(df.full, df16f, all = T)

table(is.na(df.full$State))

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
missp_neast <- c("PENNSYLVANIA, 15535", "PENNSYLVANIA", "PENNSYLVANA", "PA.", "MAINE ",
                 "MASS", "MASSACHUSETTS UNTIL 2010, ONTARIO SINCE THEN", "ME.",
                 "NH - NEW HAMPSHIRE")
missp_ncentral <- c("WISCONCSIN", "WISCONSN", "WISCOSIN", "WTSCONSIN", "WISONSIN",
                    "WISCONSON", "OHIO AND MICHIGAN", "ILLINIOS", "INDIANA AND MICHIGAN",
                    "KANSAS AND MISSOURI", "KANSAS, MISSOURI", "MICH", "MICHGAN", "MN.",
                    "NORT DAKOTA", "CA, KY, MO, MS")
missp_west <- c("AZ,  CALIFORNIA,   ALGODONES MEXICO", "CAL", "CALIF", "CALIFORNIA ",
                "CALIFORNIA AND OREGON", "CALIFORNIA-ARIZONA", "CO, WY",
                "WASHINGTON STATE", "WASHINGTON, CALIFORNIA", "WYO.", "WASHINGTON,",
                "WASHINGTON AND IDAHO", "WA, OR", "WA.", "WA, CO, HI, CA, TX, GA, AZ",
                "WA AND RUSSIAN FEDERATION", "UTAH, ARKANSAS", "OR, CA", "WYO")
missp_south <- c("NM,VA")
other <- c("DDD", "GGG", "X", "XXX", "", "N/A", "NULL")
international <- c("PR", "PUERTO RICO", "SK")

df.full <- df.full %>% 
  mutate(State = toupper(State)) %>% 
  mutate(State = str_remove_all(State, "\\.")) %>% 
  mutate(Region = case_when(
    State %in% western | State %in% missp_west  ~ "West",
    State %in% northcentral | State %in% missp_ncentral ~ "North Central",
    State %in% southern | State %in% missp_south ~ "South",
    State %in% northeast| State %in% missp_neast ~ "Northeast",
    State %in% international ~ "International",
    State %in% other ~ NA_character_,
    T ~ NA_character_))

table(is.na(df.full$Region))
table(df.full$State[is.na(df.full$Region)])

write.csv(df.full, "~/Box/organicseed_adoption/merged_df_3panel.csv", row.names = F)
