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

#write.csv(operation_subsys, 'tables/supplemental_subsystem_summary.csv', row.names = F)
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
