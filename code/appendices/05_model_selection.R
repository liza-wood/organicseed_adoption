library(dplyr)
library(tidyr)
library(betareg)
library(ggplot2)
library(cowplot)
library(jtools)
source('code/functions.R')

df.full <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled.rds") %>% 
  select(OGseed_percent_maincrop_prop,
         OG_value_s,
         CertifierRequestn,
         noncommercial_seedsource_s,
         acreage_log_s,
         cropcat_n_s,
         #barrier_buyerReq_s,
         barrier_insuffQuantity_s,
         barrier_lackdesiredTraits_s,
         barrier_availability_s,
         barrier_seedTreatlack_s,
         maincrop,
         Region,
         survey_year) %>% 
  na.omit()

df.full_rounded <- df.full
df.full_rounded$OGseed_percent_maincrop_prop <- ifelse(df.full_rounded$OGseed_percent_maincrop_prop == 1, 0.999, ifelse(df.full_rounded$OGseed_percent_maincrop_prop == 0, 0.001, df.full_rounded$OGseed_percent_maincrop_prop))


# Model comparison ----

m0 <- betareg(OGseed_percent_maincrop_prop ~ 1, 
              data = df.full_rounded,
              link = "logit", link.phi = NULL, type = "ML")
summary(m0)

# Key system variables only
#m1 <- inla(OGseed_percent_maincrop_prop ~ 
#             maincrop +
#             Region +
#             survey_year, 
#           data = df.full_rounded,
#           family = "beta",
#           control.compute = list(cpo= T, waic= T))

m1 <- betareg(OGseed_percent_maincrop_prop ~ 
             maincrop +
             Region +
             survey_year, 
           data = df.full_rounded, 
           link = "logit", link.phi = NULL, type = "ML")

# Key operation variables
#m2 <- inla(OGseed_percent_maincrop_prop ~ 
#             OG_value_s + 
#             acreage_log_s + 
#             cropcat_n_s,
#           data = df.full_rounded,
#           family = "beta",
#           control.compute = list(cpo= T, waic= T))

m2 <- betareg(OGseed_percent_maincrop_prop ~ 
             OG_value_s + 
             acreage_log_s + 
             cropcat_n_s,
           data = df.full_rounded, 
           link = "logit", link.phi = NULL, type = "ML")

# Adding barriers
#m3 <- inla(OGseed_percent_maincrop_prop ~ 
#             OG_value_s + 
#             acreage_log_s + 
#             cropcat_n_s +
#             barrier_insuffQuantity_s +
#             barrier_lackdesiredTraits_s +
#             barrier_availability_s,
#           data = df.full_rounded,
#           family = "beta",
#           control.compute = list(cpo= T, waic= T))

m3 <- betareg(OGseed_percent_maincrop_prop ~ 
             OG_value_s + 
             acreage_log_s + 
             cropcat_n_s +
             barrier_availability_s +
             barrier_lackdesiredTraits_s +
             barrier_insuffQuantity_s +
            barrier_seedTreatlack_s,
           data = df.full_rounded, 
           link = "logit", link.phi = NULL, type = "ML")

# More 'system' controls
#m4 <- inla(OGseed_percent_maincrop_prop ~ 
#             OG_value_s + 
#             acreage_log_s + 
#             cropcat_n_s +
#             barrier_insuffQuantity_s +
#             barrier_lackdesiredTraits_s +
#             barrier_availability_s +
#             maincrop +
#             Region +
#             survey_year, 
#           data = df.full_rounded,
#           family = "beta",
#           control.compute = list(cpo= T, waic= T))

m4 <- betareg(OGseed_percent_maincrop_prop ~ 
             OG_value_s + 
             acreage_log_s + 
             cropcat_n_s +
               barrier_availability_s +
               barrier_lackdesiredTraits_s +
               barrier_insuffQuantity_s +
               barrier_seedTreatlack_s +
             maincrop +
             Region +
             survey_year, 
           data = df.full_rounded, 
           link = "logit", link.phi = NULL, type = "ML")

# Controls
#m5 <- inla(OGseed_percent_maincrop_prop ~ 
#             OG_value_s + 
#             acreage_log_s + 
#             cropcat_n_s +
#             barrier_insuffQuantity_s +
#             barrier_lackdesiredTraits_s +
#             barrier_availability_s +
#             maincrop +
#             Region +
#             survey_year +
#             CertifierRequestn + 
#             noncommercial_seedsource_s, 
#             #years_certified_s, 
#           data = df.full_rounded,
#           family = "beta",
#           control.compute = list(cpo= T, waic= T))

m5 <- betareg(OGseed_percent_maincrop_prop ~ 
                OG_value_s + 
             acreage_log_s + 
             cropcat_n_s +
               barrier_availability_s +
               barrier_lackdesiredTraits_s +
               barrier_insuffQuantity_s +
               barrier_seedTreatlack_s +
             maincrop +
             Region +
             survey_year +
             CertifierRequestn + 
             noncommercial_seedsource_s, 
           #years_certified_s, 
           data = df.full_rounded, 
           link = "logit", link.phi = NULL, type = "ML")

# Model results ----
#m1df <- prep_inla_data(m1) %>% mutate(model = 1)
#m2df <- prep_inla_data(m2) %>% mutate(model = 2)
#m3df <- prep_inla_data(m3) %>% mutate(model = 3)
#m4df <- prep_inla_data(m4) %>% mutate(model = 4)
#m5df <- prep_inla_data(m5) %>% mutate(model = 5)

# Can I try stargazer here to get significance symbols
saveRDS(list(m1,m2,m3,m4,m5), 'tables/append_model_compare.RDS')

m1df <- prep_betreg_data(m1) %>% mutate(model = 1)
m2df <- prep_betreg_data(m2) %>% mutate(model = 2)
m3df <- prep_betreg_data(m3) %>% mutate(model = 3)
m4df <- prep_betreg_data(m4) %>% mutate(model = 4)
m5df <- prep_betreg_data(m5) %>% mutate(model = 5)

compare_df <- m1df %>% 
  rbind(m2df) %>% 
  rbind(m3df) %>% 
  rbind(m4df) %>% 
  rbind(m5df) 

plot_betareg_models(compare_df, 5)

tablea1 <- m5df %>% 
  select(term, mean, se) %>% 
  left_join(m4df, by = 'term') %>% 
  select(term, contains('mean'), contains('se')) %>% 
  left_join(m3df, by = 'term') %>% 
  select(term, contains('mean'), contains('se')) %>%
  left_join(m2df, by = 'term') %>% 
  select(term, contains('mean'), contains('se')) %>% 
  left_join(m1df, by = 'term') %>% 
  select(term, contains('mean'), contains('se')) %>% 
  arrange(factor(term, coef_order))

colnames(tablea1) <- c("Term", paste('Mean', 5:1), paste("SE", 5:1))
tablea1 <- select(tablea1, Term, `Mean 1`, `SE 1`, `Mean 2`, `SE 2`,
                     `Mean 3`, `SE 3`, `Mean 4`, `SE 4`, `Mean 5`, `SE 5`)
tablea1[,2:ncol(tablea1)] <- sapply(tablea1[,2:ncol(tablea1)], function(x){round(x,3)})

write.csv(tablea1, 'tables/append_model_comparet1.csv', row.names = F)

# Compare goodness of fit measures ----
# Wants lowest value
aic = c(AIC(m1),
         AIC(m2),
         AIC(m3),
         AIC(m4),
         AIC(m5)) # lowest

bic = c(BIC(m1),
        BIC(m2),
        BIC(m3),
        BIC(m4), # lowest
        BIC(m5)) 

# Wants highest value
mlik = c(m1$loglik,
         m2$loglik,
         m3$loglik,
         m4$loglik, # highest
         m5$loglik) # highest

# CPO is a predictive measure -- want to make sure no failures in the measure
#table(m1$cpo$failure > 0)
#table(m2$cpo$failure > 0)
#table(m3$cpo$failure > 0)
#table(m4$cpo$failure > 0)
#table(m5$cpo$failure > 0)

# Then look for highest
# https://groups.google.com/g/r-inla-discussion-group/c/cWwXBO11jWM?pli=1
#avglogcpo = c(mean(log(m1$cpo$cpo), na.rm = T),
#              mean(log(m2$cpo$cpo), na.rm = T),
#              mean(log(m3$cpo$cpo), na.rm = T),
#              mean(log(m4$cpo$cpo), na.rm = T), # Very close
#              mean(log(m5$cpo$cpo), na.rm = T)) # Very close

tablea3x <- data.frame(model = paste('Model', 1:5),
           aic, bic, mlik)
tablea3x$aic <- round(tablea3x$aic)
tablea3x$bic <- round(tablea3x$bic)
tablea3x$mlik <- round(tablea3x$mlik)

write.csv(tablea3x, 'tables/append_model_comparet2.csv', row.names = F)

colnames(tablea3x) <- c("Model", "AIC", "BIC", "Log likelihood") ; 
kbl(tablea3x, booktabs = T, caption = 'Goodness of fit across models') 

