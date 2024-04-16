# Increased OG Seed ----
source('code/functions.R')
df.full <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled.rds") %>% 
  select(RespondentID,
         OGseed_percent_maincrop_prop,
         OG_value_s,
         CertifierRequestn,
         noncommercial_seedsource_s,
         acreage_log_s,
         cropcat_n_s,
         #barrier_buyerReq_s,
         barrier_insuffQuantity_s,
         barrier_lackdesiredTraits_s,
         barrier_availability_s,
         maincrop,
         Region,
         survey_year) %>% 
  na.omit()
df <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds") %>% 
  filter(RespondentID %in% df.full$RespondentID) %>% 
  filter(!is.na(IncreasedOGSeed))
table(df$IncreasedOGSeed, df$CertifierRequest)
df$IncreasedOGSeedf <- factor(df$IncreasedOGSeed, c("No: About the same % or decreased",
                                                    "Yes: Increased the %",
                                                    "Already at 100%"))
df$CertifierRequestf <- factor(df$CertifierRequest)



df %>% 
  group_by(CertifierRequestf, IncreasedOGSeedf) %>% 
  count() %>%
  ungroup() %>% 
  group_by(CertifierRequestf) %>% 
  mutate(prop = round(n/sum(n), 2)) %>% 
  ggplot(aes(x = CertifierRequestf, y = IncreasedOGSeedf, alpha = prop)) +
  geom_point(size = 20)

df <- df %>% 
  mutate(Region = factor(Region, levels = c("North Central", "West", 
                                            "Northeast", "South")),
         survey_year = factor(survey_year, c(2016, 2011, 2021))) %>% 
  mutate(OGseed_percent_maincrop_prop = OGseed_percent_maincrop/100,
         acreage_log = log(TotalAcreage),
         acreage_log_s = c(scale(acreage_log, scale = F)),
         cropcat_n_s = c(scale(cropcat_n, scale = T)),
         OG_value_s = c(scale(OG_value, scale = T)),
         survey_year = factor(survey_year),
         barrier_availability_s = c(scale(barrier_availability, scale = T)),
         barrier_lackdesiredTraits_s = c(scale(barrier_lackdesiredTraits, scale = T)),
         barrier_insuffQuantity_s = c(scale(barrier_insuffQuantity, scale = T)),
         noncommercial_seedsource_s = c(scale(noncommercial_seedsource, 
                                              scale = T))
  ) %>% 
  select(colnames(df.full), IncreasedOGSeedf)

library(nnet)

m <- multinom(IncreasedOGSeedf ~ 
            barrier_availability_s +
            barrier_lackdesiredTraits_s +
            barrier_insuffQuantity_s +
            survey_year +
            OG_value_s + 
            acreage_log_s + 
            cropcat_n_s +
            CertifierRequestn +
            noncommercial_seedsource_s +
            maincrop +
            Region, 
          data = df)
summary(m)
table(df$IncreasedOGSeedf)
#m <- glm(IncreasedOGSeedf ~ 
#            barrier_availability_s +
#            barrier_lackdesiredTraits_s +
#            barrier_insuffQuantity_s +
#            survey_year +
#            OG_value_s + 
#            acreage_log_s + 
#            cropcat_n_s +
#            CertifierRequestn +
#            noncommercial_seedsource_s +
#            maincrop +
#            Region, 
#          data = df, family = binomial(link = "logit"))
#summary(m)

saveRDS(m, 'tables/append_increase_model.RDS')
