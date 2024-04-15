library(dplyr)
library(ggplot2)
library(betareg)
source('code/functions.R')
# Model using only certifier data

df.full <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds") %>% 
  filter(Region != "International", maincrop != "Other") %>% 
  select(RespondentID, 
         OGseed_percent_maincrop, 
         barrier_availability,
         barrier_insuffQuantity,
         barrier_lackdesiredTraits,
         barrier_buyerReq,
         barrier_price,
         OG_value,
         CertifierRequestn, 
         maincrop, survey_year, Region, 
         TotalAcreage, cropcat_n,
         noncommercial_seedsource,
         certifier_codename
  ) %>% 
  na.omit() %>%  # do this now so that it scales around the sample we are using
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
                                              scale = T)),
         barrier_buyerReq_s = c(scale(barrier_buyerReq, scale = T)),
         barrier_price_s = c(scale(barrier_price, scale = T))
  )

cert_n <- df.full %>% 
  group_by(certifier_codename) %>% count() %>% 
  arrange(-n)

df.full$certifier_codename <- ifelse(df.full$certifier_codename %in% 
                                       cert_n$certifier_codename[cert_n$n > 20],
                                     df.full$certifier_codename, "other_certifier")
table(df.full$certifier_codename)
lvls <- names(table(df.full$certifier_codename))
df.full$certifier_codename <- factor(df.full$certifier_codename, lvls)
table(df.full$certifier_codename)
# Relationship between certifier request use and not:
df.full %>% 
  filter(OGseed_percent_maincrop_prop <= .99) %>% 
  group_by(certifier_codename) %>% 
  summarize(mean_request_n = mean(CertifierRequestn))

df.full %>% 
  ggplot(aes(x = certifier_codename, y = OGseed_percent_maincrop_prop))+
  geom_boxplot()

df.full %>% 
  filter(OGseed_percent_maincrop_prop <= .99) %>% 
  group_by(certifier_codename) %>% 
  count()

lvls <- c("certifier34", "other_certifier","certifier2","certifier24","certifier27",
          "certifier42","certifier7")
df.full$certifier_codename <- factor(df.full$certifier_codename, lvls)

df.full_rounded <- df.full
df.full_rounded$OGseed_percent_maincrop_prop <- ifelse(df.full_rounded$OGseed_percent_maincrop_prop == 1, 0.999, ifelse(df.full_rounded$OGseed_percent_maincrop_prop == 0, 0.001, df.full_rounded$OGseed_percent_maincrop_prop))

df.full_rounded$maincrop <- factor(df.full_rounded$maincrop, c("Field", "Forage", "Vegetables"))

set.seed(19)
m_cert <- betareg(OGseed_percent_maincrop_prop ~ 
               barrier_availability_s +
               barrier_lackdesiredTraits_s +
               barrier_insuffQuantity_s +
               survey_year +
               OG_value_s + 
               acreage_log_s + 
               cropcat_n_s +
               noncommercial_seedsource_s +
               CertifierRequestn +
               maincrop +
               Region +
               certifier_codename, 
             data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m_cert)
m_cert_df <- prep_betreg_data(m_cert)
plot_betareg_models(m_cert_df, 1)

m <- betareg(OGseed_percent_maincrop_prop ~ 
                    barrier_availability_s +
                    barrier_lackdesiredTraits_s +
                    barrier_insuffQuantity_s +
                    survey_year +
                    OG_value_s + 
                    acreage_log_s + 
                    cropcat_n_s +
                    noncommercial_seedsource_s +
                    CertifierRequestn +
                    maincrop +
                    Region, 
                  data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m)
m_df <- prep_betreg_data(m)
plot_betareg_models(m_df, 1)

saveRDS(list(m_cert, m), 'tables/append_certifier_model.RDS')
# With certifier request
#Log-likelihood: 631.5 on 24 Df
#Pseudo R-squared: 0.2698
#Number of iterations: 34 (BFGS) + 2 (Fisher scoring) 

# Without certifier request
#Log-likelihood: 625.5 on 23 Df
#Pseudo R-squared: 0.2404
#Number of iterations: 33 (BFGS) + 2 (Fisher scoring) 

# Compared to the non certifier model
# Type of estimator: ML (maximum likelihood)
# Log-likelihood:  2266 on 18 Df
# Pseudo R-squared: 0.2437
# Number of iterations: 28 (BFGS) + 2 (Fisher scoring) 
# AIC: -4495.492
# BIC: -4397.058

# Mediation analysis ----

set.seed(19)
# total effect
s1 <- betareg(OGseed_percent_maincrop_prop ~ 
                barrier_availability_s  +
                barrier_lackdesiredTraits_s  +
                barrier_insuffQuantity  +
                survey_year +
                OG_value_s  + 
                acreage_log_s  + 
                cropcat_n_s  +
                CertifierRequestn +
                noncommercial_seedsource_s  +
                maincrop +
                Region, 
              data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(s1)
# first part indirect
s2 <- glm(CertifierRequestn  ~ 
           barrier_availability_s  +
           barrier_lackdesiredTraits_s  +
           barrier_insuffQuantity +
           survey_year +
           OG_value_s  + 
           acreage_log_s  + 
           cropcat_n_s  +
           CertifierRequestn +
           noncommercial_seedsource_s  +
           maincrop +
           Region +
           certifier_codename, 
         data = df.full_rounded, family = binomial)
summary(s2)

# Mediation happening: Assuming there are significant relationships from Steps 1 through 2, one proceeds to Step 4
s4 <- betareg(OGseed_percent_maincrop_prop ~ 
                barrier_availability_s  +
                barrier_lackdesiredTraits_s  +
                barrier_insuffQuantity_s  +
                survey_year +
                OG_value_s  + 
                acreage_log_s  + 
                cropcat_n_s  +
                CertifierRequestn +
                noncommercial_seedsource_s  +
                maincrop +
                Region +
                certifier_codename, 
              data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(s4)

levels(df.full$certifier_codename)
results1 <- mediation::mediate(s2, s4, treat= 'certifier_codename', 
                               mediator= 'CertifierRequestn' ,boot=TRUE, sims=100,
                               control.value = 'certifier34',
                               treat.value = 'certifier24')
summary(results1)

sum1 <- data.frame("Names" = c("ACME (avg)", 
                               "ADE (avg)",
                               "Prop. Mediated (avg) ", 
                               "Totel Effect"),
                   "Estimate" = round(c(results1$d.avg, results1$z.avg,
                                        results1$n.avg, results1$tau.coef), 3),
                   "p-value" = c(results1$d.avg.p, results1$z.avg.p, 
                                 results1$n.avg.p, results1$tau.p))

write.csv(sum1, 'tables/append_mediation1.csv', row.names = F)