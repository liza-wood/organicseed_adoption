library(usmap)
library(tidyverse)
us_states_og <- usmap::us_map()
us_states_og$full <- tools::toTitleCase(us_states_og$full)

greens <- colorRampPalette(c("lightgrey", "#55752f"))(8) # 2014-2018
reds <- colorRampPalette(c("lightgrey", "#90353b"))(8) # 2002-2008
blues <- colorRampPalette(c("lightgrey", "#1A476f"))(8) # 2009-2013

mako <- viridis::mako(n=9)

setwd("~/Box/osa_networks/data_combined/")
df11 <- read.csv("~/Dropbox/SOSInteractiveSurvey/2011SurveyData.csv") %>% 
  slice(-c(1:2)) %>% 
  select(`In.which.state.do.you.farm...i..Please.fill.in....i.`)
colnames(df11) <- "State"
df16 <- read.csv("~/Dropbox/SOSInteractiveSurvey/2016SurveyData.csv") %>% 
  slice(-c(1:2)) %>% 
  select(`X.1`, `X.8`) 
colnames(df16) <- c("State", "Zip")
df21 <- read.csv("~/Dropbox/SOSInteractiveSurvey/data2021_cleaned.csv") %>%
  select(Q04A)
colnames(df21) <- "Zip"

# I made these lists from online databases
us.latlong <- read.csv("zipcodes/us-zip.csv") %>% 
  mutate(country = "USA") %>% 
  select(-city) %>% 
  rename("State" = state)
ca.latlong <- read.csv("zipcodes/ca-zip.csv") %>% 
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
  filter(!is.na(Region))


region_text <- function(df){
  n.by.state <- df %>% 
    group_by(Region, State) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(StateN = sum(n), 
           Stateprop = n/StateN,
           StatePercent = paste(round(100*(n/StateN), 1), "%"),
           StateN = sum(n), ) %>% 
    rename("Staten" = n) 
  n.by.region <- df %>% 
      group_by(Region) %>% 
      count() %>% 
      ungroup() %>% 
      mutate(RegionN = sum(n), 
             Regionprop = n/RegionN,
             RegionPercent = paste(round(100*(n/RegionN), 1), "%")) %>% 
    rename("Regionn" = n) %>% 
    ungroup()
    
  counts <- left_join(n.by.state, n.by.region)
  
  return(counts)
}

make_map <- function(df, mapdf){
  # This one is finnicky but works
  us_states <- left_join(us_states_og, mapdf, by = c("abbr" = "State")) %>% 
    rename("State" = "full") %>% 
    mutate(State = trimws(State)) 
  
  # For the states that weren't in the dataset
  us_states <- us_states %>% 
    mutate(Region = case_when(
      State %in% western ~ "West",
      State %in% northcentral ~ "North Central",
      State %in% southern ~ "South",
      State %in% northeast ~ "Northeast",
      T ~ NA_character_)) %>% 
    mutate(Regionn = case_when(
      is.na(Regionn) & Region == "Northeast" ~ 
        unique(mapdf$Regionn[mapdf$Region == "Northeast"]),
      is.na(Regionn) & Region == "West" ~ 
        unique(mapdf$Regionn[mapdf$Region == "West"]),
      is.na(Regionn) & Region == "South" ~ 
        unique(mapdf$Regionn[mapdf$Region == "South"]),
      is.na(Regionn) & Region == "North Central" ~ 
        unique(mapdf$Regionn[mapdf$Region == "North Central"]),
      T ~ Regionn),
      RegionPercent = case_when(
        is.na(RegionPercent) & Region == "Northeast" ~ unique(mapdf$RegionPercent[mapdf$Region == "Northeast"]),
        is.na(RegionPercent) & Region == "West" ~ unique(mapdf$RegionPercent[mapdf$Region == "West"]),
        is.na(RegionPercent) & Region == "North Central" ~ unique(mapdf$RegionPercent[mapdf$Region == "North Central"]),
        is.na(RegionPercent) & Region == "South" ~ unique(mapdf$RegionPercent[mapdf$Region == "South"]),
        T ~ RegionPercent))
  
  # To plot points on the map
  loc <- data.frame(lon = df$long, lat = df$lat, year = df$year)
  transformed <- usmap_transform(loc) %>% filter(lat < 51 & lat > 21)
  transformed <- transformed %>% 
    mutate(year = case_when(year == 2011 ~ 2010, year == 2016 ~ 2015,
                            year == 2021 ~ 2020, T ~ NA)) 
  #plot_usmap() +
  #  geom_point(data = transformed, aes(x = x, y = y), colour = "#2d677f", shape=21, size = 2)
  
  # To plot regional blocks
  counts <- us_states %>% select(Region, Regionn) %>% 
    unique() %>% arrange(Regionn)
  
  us_states$n <- factor(us_states$Regionn)
  levels(us_states$n) <- c(paste(counts[1,1], counts[1,2], sep = ": "),
                           paste(counts[2,1], counts[2,2], sep = ": "),
                           paste(counts[3,1], counts[3,2], sep = ": "),
                           paste(counts[4,1], counts[4,2], sep = ": "))
  
  p <- ggplot() +
    geom_polygon(data = us_states, fill = "white", size = 0.2,
                 color = "black",
                 aes(x = x, y = y, 
                     #color = factor(Regionn), 
                     group = group)) + 
    geom_point(data = transformed, aes(x = x, y = y, colour = factor(year)) , 
               alpha = .4,shape=20, size = 2,
               position=position_jitter(width=10000, height=10000)) +
    scale_color_manual(values = mako[c(2,5,8)]) + 
    labs(title = "",
         color = "",
         x = "", y = "") + 
    theme_light(base_size = 10)  +
    theme(
      #legend.position="bottom", # was bottom
      legend.position = c(0.5, 0.05),
      legend.direction = "horizontal",
      panel.grid = element_blank(),
      line = element_blank(),
      rect = element_blank(),
      axis.text = element_blank(),
      text = element_text(family = "Times", size = 14)
      #axis.title = element_text(family = FONTTYPE, size = TITLESIZE)
    ) +
    guides(color = guide_legend(override.aes = list(size = 10)))
  return(p)
  
}

mapdf <- region_text(df)
map <- make_map(df, mapdf)
map

ggsave("~/Documents/Davis/R-Projects/organicseed_adoption/figures/map.png", height = 4.5, width = 5, dpi = 800)

# Olg regional maps -- fx will not work for them anymore

df11mapdf <- region_text(df11_random_locations)
map10 <- make_map(df11_random_locations, df11mapdf, rep("black",4), "2010")

df16mapdf <- region_text(df16)
map15 <- make_map(df16, df16mapdf, reds[c(6,2,4,8)], "2015")

df21mapdf <- region_text(df21)
map20 <- make_map(df21, df21mapdf, blues[c(6,2,4,8)], "2020")

cowplot::plot_grid(map10, map15, map20, nrow = 1)
