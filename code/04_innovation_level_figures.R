# FIGURES FOR PAPER

## global color settings ----
blues <- RColorBrewer::brewer.pal(8, "PuBu")
blues <- colorRampPalette(c("lightgrey", "#1A476f"))(8)
greens <- RColorBrewer::brewer.pal(8, "Greens")
greens <- colorRampPalette(c("lightgrey", "#55752f"))(8)
reds <- RColorBrewer::brewer.pal(8, "Oranges")
reds <- colorRampPalette(c("lightgrey", "#90353b"))(8)

# Figure 1a and summaries for funding ----
## These are data gathered by Jared for the SOS report
funds_organic <- read.csv("~/Dropbox/SOSInteractiveSurvey/local_data/public-funding-Dec21.csv") %>% 
  filter(year > 2003 & year < 2019) %>% 
  mutate(year_range = case_when(
    year %in% 2004:2008 ~ "2004-2008",
    year %in% 2009:2013 ~ "2009-2013",
    year %in% 2014:2018 ~ "2014-2018",
    T ~ NA_character_
  )) %>% 
  mutate(source_aggregate = case_when(
    funding_source %in% c("FAFO", "OFRF", "Other Non-Federal Funds", "Clif Bar Family Foundation") ~ "non-public",
    T ~ "public"
  )) %>% 
  mutate(federal = case_when(
    funding_source %in% c("FAFO", "OFRF", "Other Non-Federal Funds", "Clif Bar Family Foundation", "SARE") ~ F,
    T ~ T
  )) 
## These are data that I scraped to detect organic projetcs from the federal finding database
#fed_breed <- read.csv("~/Box/lgu/data_raw/federal_funding/breeding_awards_combined.csv")
## These are all of the federal funding awards
fed_all <- read.csv("~/Box/lgu/data_raw/federal_funding/allawards.csv") %>% 
  filter(Award.Dollars > 0) %>% 
  #mutate(breeding = case_when(
  #  Proposal.Number %in% fed_breed$Proposal.Number ~ T,
  #  T ~ F
  #)) %>% 
  #mutate(organic = case_when(
  #  str_detect(Program.Name, "[Oo]rganic") ~ T,
  #  T ~ F
  #)) %>% 
  mutate(year = year(Award.Date))  %>% 
  filter(year > 2003 & year < 2019) %>% 
  mutate(year_range = case_when(
    year %in% 2004:2008 ~ "2004-2008",
    year %in% 2009:2013 ~ "2009-2013",
    year %in% 2014:2018 ~ "2014-2018",
    T ~ NA_character_
  ))
#fed_organicbreed <- filter(fed_all, breeding == T & organic == T)


year_color <- data.frame(year = 2004:2018,
                         year_color = c(rep(greens[8], 5),
                                        rep(reds[8], 5),
                                        rep(blues[8], 5))) %>% 
  mutate(year_range = case_when(
    year %in% 2004:2008 ~ "2004-2008",
    year %in% 2009:2013 ~ "2009-2013",
    year %in% 2014:2018 ~ "2014-2018",
    T ~ NA_character_
  ))

# Compare my measure for organic breeding funds from federal (using regex) versus what we used for SOS report:
## This is the SOS report
#sos_funds_crop <- funds_organic %>% 
#  filter(year > 2003 & year < 2019) %>% 
#  mutate(year_range = case_when(
#    year %in% 2004:2008 ~ "2004-2008",
#    year %in% 2009:2013 ~ "2009-2013",
#    year %in% 2014:2018 ~ "2014-2018",
#    T ~ NA_character_
#  )) %>% 
#  group_by(year_range, year, proj_crop) %>% 
#  summarize(annual_total = sum(funding_amt)) 
#sos_funds_crop %>% 
#  ggplot(aes(x = year, y = annual_total, fill = proj_crop)) +
#  geom_col()
#
### This is the federal database dive
#fed_funds <- fed_organicbreed %>% 
#  group_by(year) %>% 
#  summarize(annual_total = sum(Award.Dollars))
#fed_funds %>% 
#  ggplot(aes(x = year, y = annual_total)) +
#  geom_col() 
#

## This is looking at organic breeding as portion of all federal funding, according to the USDA database only and my regex, #not the SOS generated list.
#fed_summary <- fed_all %>% 
#  group_by(year, breeding, organic) %>% 
#  summarize(breeding_sum = sum(Award.Dollars, na.rm = T)) %>% 
#  ungroup() %>% 
#  group_by(year) %>% 
#  mutate(annual_sum = sum(breeding_sum, na.rm = T)) %>% 
#  filter(breeding == T & organic == T) %>% 
#  unique() %>% 
#  mutate(Percent = round(100*(breeding_sum/annual_sum), 1)) %>% 
#  left_join(year_color)
#ggplot(fed_summary, aes(x = year, y = breeding_sum/100000, 
#                        label = paste0(Percent, "%"))) +
#  geom_col(fill = fed_summary$year_color) +
#  geom_text(size = 3, vjust = 2) +
#  theme_linedraw() +
#  theme(legend.position = "none") +
#  labs(x = "", y = "Federal funding by $100 thousand\n(% of total funding)")

# This is not satisfying, so let's identify the federal fundings from the SOS list and look at this amount as a portion of tital federal funds 
sosfunds_yearly <- funds_organic %>% 
  group_by(year_range, proj_crop) %>% 
  mutate(average_by_project = sum(funding_amt)/n()) %>% 
  mutate(annual_average = sum(funding_amt)/5)

sosfunds_yearrange <- funds_organic %>% 
  group_by(year_range, proj_crop) %>% 
  mutate(average_by_project = sum(funding_amt)/n()) %>% 
  mutate(annual_average = sum(funding_amt)/5)

fed_yearly_summary <- fed_all %>% 
  group_by(year) %>% 
  summarize(annual_sum = sum(Award.Dollars, na.rm = T)) %>% 
  left_join(select(year_color, year, year_color)) %>% 
  unique()

fed_yearrange_summary <- fed_all %>% 
  group_by(year_range) %>% 
  summarize(annual_sum = sum(Award.Dollars, na.rm = T)) %>% 
  left_join(select(year_color, year_range, year_color)) %>% 
  unique()

combined_crops <- sosfunds_yearly %>% 
  group_by(year_range, year, proj_crop) %>% 
  summarise(organic_total_bycrop = sum(funding_amt))
combined_yearly <- sosfunds_yearly %>% 
  group_by(year_range, year) %>% 
  summarize(organic_total_yearly = sum(funding_amt)) %>% 
  left_join(fed_yearly_summary) %>% 
  mutate(Percent = round(100*(organic_total_yearly/annual_sum), 1)) %>% 
  right_join(combined_crops) %>% 
  mutate(Percenttext = paste0(Percent, "%"))
nyrs <- combined_yearly %>% 
  group_by(year) %>% 
  count() %>% 
  mutate(counts = n-1) 
remaining <- which(!(1:nrow(combined_yearly) %in% c(1,4,7,10,13,16,20,23,26,30,33,36,39,41,43)))
combined_yearly$Percenttext[remaining] <- ""

pfunding <- combined_yearly %>% 
  ggplot(aes(x = year_range, y = organic_total_bycrop/1000000, 
             label = Percenttext, fill = proj_crop)) +
  geom_col() + # fill = fed_summary_elab$year_color
  geom_text(size = 3, position = position_stack(vjust = 1.05)) +
  theme_linedraw() +
  #theme(legend.position = "none") +
  labs(x = "", y = "Funding in millions\n(% of total federal funding)"); pfunding

# OR
sosfunds_yearrange_region <- funds_organic %>% 
  group_by(year_range, proj_crop, location) %>% 
  mutate(organic_total_bycrop = sum(funding_amt)/n())

fed_yearrange_region_summary <- fed_all %>% 
  group_by(year_range, location) %>% 
  summarize(annual_sum = sum(Award.Dollars, na.rm = T)) %>% 
  left_join(select(year_color, year_range, year_color)) %>% 
  unique()

combined_cropsreg <- funds_organic %>% 
  group_by(year_range, proj_crop, location) %>% 
  summarise(organic_total_bycrop = sum(funding_amt))
combined_location <- funds_organic %>% 
  group_by(year_range, location) %>% 
  summarize(organic_total_location = sum(funding_amt)) %>% 
  left_join(fed_yearly_summary) %>% 
  mutate(Percent = round(100*(organic_total_yearly/annual_sum), 1)) %>% 
  right_join(combined_crops) %>% 
  mutate(Percenttext = paste0(Percent, "%"))
nyrs <- combined_yearly %>% 
  group_by(year) %>% 
  count() %>% 
  mutate(counts = n-1) 
remaining <- which(!(1:nrow(combined_yearly) %in% c(1,4,7,10,13,16,20,23,26,30,33,36,39,41,43)))
combined_yearly$Percenttext[remaining] <- ""

sosfunds_yearrange_region %>% 
  ggplot(aes(x = year, y = organic_total_bycrop/1000000, 
              fill = proj_crop)) +
  geom_col() + # fill = fed_summary_elab$year_color
  #geom_text(size = 3, position = position_stack(vjust = 1.05)) +
  theme_linedraw() +
  #theme(legend.position = "none") +
  labs(x = "", y = "Funding in millions\n(% of total federal funding)") + 
  facet_wrap(~location)



# Average percentages over the years
100*(sum(fed_summary$organic_total[fed_summary$year_range == "2004-2008"])/sum(fed_summary$annual_sum[fed_summary$year_range == "2004-2008"]))
100*(sum(fed_summary$organic_total[fed_summary$year_range == "2009-2013"])/sum(fed_summary$annual_sum[fed_summary$year_range == "2009-2013"]))
100*(sum(fed_summary$organic_total[fed_summary$year_range == "2014-2018"])/sum(fed_summary$annual_sum[fed_summary$year_range == "2014-2018"]))

# Some details from different sources
funding_sources_amt <- funds_organic %>% 
  group_by(year_range) %>% 
  mutate(yr_range_total = sum(funding_amt, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year_range, source_aggregate) %>% 
  summarize(yr_range_total = yr_range_total, 
            source_sum = sum(funding_amt),
            Percent = round(100*(source_sum/yr_range_total), 1)) %>% 
  unique()

funding_sources_number <- funds_organic %>% 
  group_by(year_range) %>% 
  mutate(n_projects = n()) %>% 
  ungroup() %>% 
  group_by(year_range, source_aggregate) %>% 
  summarize(n_projects = n_projects, 
            source_n = n(),
            Percent = round(100*(source_n/n_projects), 1)) %>% 
  unique()

# Summaries about market proxies ----
## 1. Certifier perceptions is just straight from the survey Kiki sent
## 2. Seed producers and companies -- still could be improved (e.g. database for seed companies?)

regional_summary <- read.csv("~/Box/organicseed/regional_data.csv")

regional_summary %>% 
  ggplot(aes(x = year_range, y = nsupplychainactors)) + 
  geom_col() +
  facet_wrap(~Region, scales = "free_y")

# Want to add these together ---
dnb %>% 
  mutate(year_range = case_when(
    is.na(founded) ~ NA_character_,
    founded < 2000 ~ "pre-2000",
    T ~ founded
  ))

pmarket_dnb <- dnb %>% 
  filter(!is.na(Region)) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = founded, fill = Region),
           position = position_dodge2(preserve = "single")) +
  theme_linedraw() +
  #theme(legend.position = "none") +
  labs(x = "", y = "Funding in millions\n(% of total federal funding)"); pmarket_dnb

pmarket_dnb2 <- ggplot() +
  geom_bar(data = dnb, mapping = aes(x = founded, fill = Region)) +
  theme_linedraw() +
  #theme(legend.position = "none") +
  labs(x = "", y = "Funding in millions\n(% of total federal funding)"); pmarket_dnb2

# This is all the companies, but with their crops
survey_companies <- p_co_crop_year %>% 
  select(Operation, Region, maincrop, income20_c) %>% 
  unique()

survey_companies$income20_cf <- factor(survey_companies$income20_c, levels = c("Under $50,000",
                                                                               "$50,000-$299,999",
                                                                               "$300,000-$1 million",
                                                                               "Over $1 million"))
pmarket_survey <- survey_companies %>% 
  filter(!(Region %in% c("Canada", "International", "Other"))) %>% 
  filter(!is.na(income20_cf)) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = income20_cf, fill = Region),
           position = position_dodge2(preserve = "single")) +
  theme_linedraw() +
  #theme(legend.position = "none") +
  labs(x = "", y = "Count of companies"); pmarket_survey

pmarket_survey <- survey_companies %>% 
  filter(income20_c != "" & !is.na(income20_cf)) %>% 
  filter(!(Region %in% c("Canada", "International", "Other"))) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = , fill = maincrop),
           position = position_dodge2(preserve = "single")) +
  theme_linedraw() +
  #theme(legend.position = "none") +
  labs(x = "", y = "Count of companies"); pmarket_survey

# 7 = "$300,000-$1 million"
# 6 = "$75,000-$99,999",
# 5 = "$100,000-$149,999",
# 4 = "$150,000-$299,999",
