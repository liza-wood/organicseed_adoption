
#install.packages('usmap') USING THE NEWEST
library(usmap)
library(tidyverse)
source("code/functions.R")

# Older version of usmap doesn;t seperate out the lat/long on us_map() function, 
# but then usmap_transform() doesn't work at all
# Newer version does let usmap_transform but then doesn't separate lat/long here
# So use the old version to save the us_map, then use new version for replicability

#packageurl <- "https://cran.r-project.org/src/contrib/Archive/usmap/usmap_0.5.2.tar.gz"
#install.packages(packageurl, repos=NULL, type="source")
#library(usmap)
#us_states_og <- usmap::us_map()
#saveRDS(us_states_og, 'data/us_states_og.rds')
us_states_og <- readRDS('data/us_states_og.rds')
us_states_og$full <- tools::toTitleCase(us_states_og$full)

model_ids <- readRDS('data/model_ids.RDS')

df11 <- read.csv("~/Dropbox/SOSInteractiveSurvey/2011SurveyData.csv") %>% 
  slice(-c(1:2)) %>% 
  filter(RespondentID %in% model_ids) %>% 
  select(`In.which.state.do.you.farm...i..Please.fill.in....i.`)
colnames(df11) <- "State"
df16 <- read.csv("~/Dropbox/SOSInteractiveSurvey/2016SurveyData.csv") %>% 
  slice(-c(1:2)) %>% 
  filter(RespondentID %in% model_ids) %>% 
  select(`X.1`, `X.8`)  
colnames(df16) <- c("State", "Zip")
df21 <- read.csv("~/Dropbox/SOSInteractiveSurvey/data2021_cleaned.csv") %>% 
  filter(RespID %in% model_ids) %>%
  select(Q04A) 
colnames(df21) <- "Zip"

# I made these lists from online databases
us.latlong <- read.csv("~/Box/osa_networks/data_combined/zipcodes/us-zip.csv") %>% 
  mutate(country = "USA") %>% 
  select(-city) %>% 
  rename("State" = state)
ca.latlong <- read.csv("~/Box/osa_networks/data_combined/zipcodes/ca-zip.csv") %>% 
  mutate(country = "Canada") %>% 
  select(-city) %>% 
  rename("State" = state)

latlong <- rbind(us.latlong, ca.latlong)

df16 <- df16 %>% filter(Zip != "") %>% 
  left_join(latlong, by = c("Zip" = "postcode", "State")) %>% 
  filter(!is.na(lat)) 
df21 <- df21 %>% 
  filter(!(Zip %in% c("partial complete", "no answer", 
                      "No answer", "skipped due to branching"))) %>% 
  left_join(latlong, by = c("Zip" = "postcode")) %>% 
  filter(!is.na(lat)) 

# df11 has no zips, so I will just randomly sample zips from the other years based on state
statecounts11 <- df11 %>% 
  group_by(State) %>% 
  count() %>% 
  filter(State != "")

df11_random_locations <- data.frame()
for(i in statecounts11$State){
  onestate_count <- filter(statecounts11, State == i)
  if(onestate_count$State %in% "PW"){
    next
  }
  onestate_loc16 <- filter(df16, State == i)
  onestate_loc21 <- filter(df21, State == i)
  onestate_loc <- rbind(onestate_loc16, onestate_loc21)
  if(nrow(onestate_loc) == 0){
    onestate_loc <- filter(latlong, State == i)
    onestate_sample <- sample_n(onestate_loc, onestate_count$n, replace = T) %>% 
      select(State, postcode, lat, long, country) %>% 
      rename("Zip" = postcode)
  } else {
    onestate_sample <- sample_n(onestate_loc, onestate_count$n, replace = T)
  }
  df11_random_locations <- rbind(df11_random_locations, onestate_sample)
}


# Mapping the blocks ----
western <- c("CA", "OR", "WA", "ID", "NV", "AZ", "UT", "NM", "CO", "WY", "MT", 
                     "AK", "HI","California", "Oregon", "Washington", "Idaho", "Nevada",
                     "Arizona", "Utah", "New Mexico", "Colorado", "Wyoming", "Montana",
                     "Alaska", "Hawaii")
northcentral <- c("ND", "SD", "KS", "NE", "MN", "IA", "MO", "WI", "IL", "MI", 
                          "IN", "OH", "North Dakota", "South Dakota", "Kansas",
                          "Nebraska", "Minnesota", "Iowa", "Missouri", "Wisconsin", 
                          "Illinois", "Michigan", "Indiana", "Ohio")
southern <- c("TX", "OK", "AR", "LA", "KY", "TN", "MS", "AL", "FL", "GA", "SC",
                      "NC", "VA",  "PR", "Texas", "Oklahoma", "Arkansas", "Louisiana", 
                      "Kentucky", "Tennessee", "Mississippi", "Alabama", "Florida", 
                      "Georgia", "South Carolina", "North Carolina", "Virginia")
northeast <- c("WV", "MD", "DE", "NJ", "PA", "NY", "CT", "RI", "MA", "VT", 
                       "NH", "ME", "West Virginia", "Maryland", "Delaware", 
                       "New Jersey", "Pennsylvania", "New York", "Connecticut", 
                       "Rhode Island", "Massachusetts", "Vermont", "New Hampshire", 
               "Maine", "District of Columbia")

df11_random_locations$year = 2011
df16$year = 2016
df21$year = 2021

df <- rbind(df11_random_locations, df16, df21)
df <- df %>% 
  mutate(Region = case_when(
    State %in% western ~ "West",
    State %in% northcentral ~ "North Central",
    State %in% southern ~ "South",
    State %in% northeast ~ "Northeast",
    T ~ NA_character_
  )) %>% 
  filter(!is.na(Region)) %>% 
  filter(State != "PR") # Unfortunately usmap does not have PR, so excluding

mapdf <- region_text(df)
map <- make_map(df, mapdf)
# This one is better than previous because it has AK, but HI is all of place?
map

ggsave("~/Documents/Davis/R-Projects/organicseed_adoption/figures/figure1.png", height = 5.5, width = 6.5, dpi = 800)



