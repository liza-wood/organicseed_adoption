# Setup ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(jtools)
#library(INLA)
library(betareg)
source('code/functions.R')
setwd("~/Documents/Davis/R-Projects/organicseed_adoption/")
df.full <- readRDS("~/Box/organicseed_adoption/df_scaled.rds") %>% 
  mutate(acreage_s = acreage_log,
         cropcat_n_s = cropcat_n) %>% 
  select(CVseed_percent_maincrop_prop,
         legitimation_user_s,
         CertifierRequestn_s,
         noncommercial_seedsource_s,
         #acreage_log_s,
         acreage_s,
         cropcat_n_s,
         barrier_buyerReq_s,
         barrier_insuffQuantity_s,
         barrier_lackdesiredTraits_s,
         barrier_availability_s,
         maincrop,
         Region,
         survey_year) 


# Round the numbers
df.full_rounded <- df.full
df.full_rounded$CVseed_percent_maincrop_prop <- ifelse(df.full$CVseed_percent_maincrop_prop == 0, 0.001,
                                                       ifelse(df.full$CVseed_percent_maincrop_prop == 1, 0.999,df.full$CVseed_percent_maincrop_prop))

# Model comparison ----
#m0 <- inla(CVseed_percent_maincrop_prop ~ 1, 
#           data = df.full_rounded,
#           family = "beta",
#           control.compute = list(cpo= T, waic= T))
#summary(m0)
m0 <- betareg(CVseed_percent_maincrop_prop ~ 1, 
              data = df.full_rounded,
              link = "logit", link.phi = NULL, type = "ML")
summary(m0)

# Key system variables only
#m1 <- inla(CVseed_percent_maincrop_prop ~ 
#             maincrop +
#             Region +
#             survey_year, 
#           data = df.full_rounded,
#           family = "beta",
#           control.compute = list(cpo= T, waic= T))

m1 <- betareg(CVseed_percent_maincrop_prop ~ 
             maincrop +
             Region +
             survey_year, 
           data = df.full_rounded, 
           link = "logit", link.phi = NULL, type = "ML")

# Key operation variables
#m2 <- inla(CVseed_percent_maincrop_prop ~ 
#             legitimation_user_s + 
#             acreage_s + 
#             cropcat_n_s,
#           data = df.full_rounded,
#           family = "beta",
#           control.compute = list(cpo= T, waic= T))

m2 <- betareg(CVseed_percent_maincrop_prop ~ 
             legitimation_user_s + 
             acreage_s + 
             cropcat_n_s,
           data = df.full_rounded, 
           link = "logit", link.phi = NULL, type = "ML")

# Adding barriers
#m3 <- inla(CVseed_percent_maincrop_prop ~ 
#             legitimation_user_s + 
#             acreage_s + 
#             cropcat_n_s +
#             barrier_buyerReq_s +
#             barrier_insuffQuantity_s +
#             barrier_lackdesiredTraits_s +
#             barrier_availability_s,
#           data = df.full_rounded,
#           family = "beta",
#           control.compute = list(cpo= T, waic= T))

m3 <- betareg(CVseed_percent_maincrop_prop ~ 
             legitimation_user_s + 
             acreage_s + 
             cropcat_n_s +
             barrier_buyerReq_s +
             barrier_insuffQuantity_s +
             barrier_lackdesiredTraits_s +
             barrier_availability_s,
           data = df.full_rounded, 
           link = "logit", link.phi = NULL, type = "ML")

# More 'system' controls
#m4 <- inla(CVseed_percent_maincrop_prop ~ 
#             legitimation_user_s + 
#             acreage_s + 
#             cropcat_n_s +
#             barrier_buyerReq_s +
#             barrier_insuffQuantity_s +
#             barrier_lackdesiredTraits_s +
#             barrier_availability_s +
#             maincrop +
#             Region +
#             survey_year, 
#           data = df.full_rounded,
#           family = "beta",
#           control.compute = list(cpo= T, waic= T))

m4 <- betareg(CVseed_percent_maincrop_prop ~ 
             legitimation_user_s + 
             acreage_s + 
             cropcat_n_s +
             barrier_buyerReq_s +
             barrier_insuffQuantity_s +
             barrier_lackdesiredTraits_s +
             barrier_availability_s +
             maincrop +
             Region +
             survey_year, 
           data = df.full_rounded, 
           link = "logit", link.phi = NULL, type = "ML")

# Controls
#m5 <- inla(CVseed_percent_maincrop_prop ~ 
#             legitimation_user_s + 
#             acreage_s + 
#             cropcat_n_s +
#             barrier_buyerReq_s +
#             barrier_insuffQuantity_s +
#             barrier_lackdesiredTraits_s +
#             barrier_availability_s +
#             maincrop +
#             Region +
#             survey_year +
#             CertifierRequestn_s + 
#             noncommercial_seedsource_s, 
#             #years_certified_s, 
#           data = df.full_rounded,
#           family = "beta",
#           control.compute = list(cpo= T, waic= T))

m5 <- betareg(CVseed_percent_maincrop_prop ~ 
             legitimation_user_s + 
             acreage_s + 
             cropcat_n_s +
             barrier_buyerReq_s +
             barrier_insuffQuantity_s +
             barrier_lackdesiredTraits_s +
             barrier_availability_s +
             maincrop +
             Region +
             survey_year +
             CertifierRequestn_s + 
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

plot_inla_models(compare_df, 5)

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

write.csv(tablea1, 'tables/tablea1.csv', row.names = F)

colnames(tablea1) <- c("Term", rep(c("Mean", "SE"), 5)) ; 
tablea1[is.na(tablea1)] <- "" ; 
kbl(tablea1, booktabs = T, caption = 'Model comparison results') %>% kable_styling(latex_options = "scale_down") %>% 
  add_header_above(c(" " = 1, "Model 1" = 2, "Model 2" = 2, "Model 3" = 2, "Model 4" = 2, "Model 5" = 2))

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

tablea2 <- data.frame(model = paste('Model', 1:5),
           aic, bic, mlik)
tablea2$aic <- round(tablea2$aic)
tablea2$bic <- round(tablea2$bic)
tablea2$mlik <- round(tablea2$mlik)

write.csv(tablea2, 'tables/tablea2.csv', row.names = F)

colnames(tablea2) <- c("Model", "AIC", "BIC", "Log likelihood") ; 
kbl(tablea2, booktabs = T, caption = 'Goodness of fit across models') 

# https://becarioprecario.bitbucket.io/inla-gitbook/ch-INLA.html
#ggplot(m1$summary.linear.predictor, 
#       aes(x = 1:nrow(m1$summary.linear.predictor), y = m4$cpo$cpo)) +
#  geom_point()
#
#ggplot(m1$summary.linear.predictor, 
#       aes(x = 1:nrow(m1$summary.linear.predictor), y = m4$cpo$pit)) +
#  geom_point()

# Easy way
plot(residuals(m1))
plot(residuals(m2))
plot(residuals(m3))
plot(residuals(m4))
plot(residuals(m5))

# Manual way
residm1 <- df.full$CVseed_percent_maincrop_prop - predict(m1)
residm2 <- df.full$CVseed_percent_maincrop_prop - predict(m2)
residm3 <- df.full$CVseed_percent_maincrop_prop - predict(m3)
residm4 <- df.full$CVseed_percent_maincrop_prop - predict(m4)
residm5 <- df.full$CVseed_percent_maincrop_prop - predict(m5)

plot_resid <- function(resid, modelname){
  ggplot(df.full, aes(x = 1:nrow(df.full), y = resid)) +
    geom_point(alpha = .6) +
    geom_smooth() +
    theme_minimal() +
    labs(x = "Observation", y = "Residual value", title = modelname) +
    ylim(c(-.8, .8)) +
    theme(text = element_text(family = "Times"),
          plot.title = element_text(hjust = 0.5, size = 12))
}

plot_resid(residm1, "Model 1")
plot_resid(residm2, "Model 2") 
plot_resid(residm3, "Model 3")
plot_resid(residm4, "Model 4") 
plot_resid(residm5, "Model 5")

# Misc other comparison for INLA ----
# Check compare to censoring
# censoring which rounds values down to 0 or 1... for whatever reason
cens <- 0.05
df.full$CVseed_percent_maincrop_prop[df.full$CVseed_percent_maincrop_prop <= cens] <- 0
df.full$CVseed_percent_maincrop_prop[df.full$CVseed_percent_maincrop_prop >= 1-cens] <- 1
m_cens <- inla(CVseed_percent_maincrop_prop ~ legitimation_user_s + 
             CertifierRequestn_s + 
             noncommercial_seedsource_s + 
             acreage_s + 
             cropcat_n_s +
             barrier_buyerReq_s +
             barrier_insuffQuantity_s +
             barrier_lackdesiredTraits_s +
             barrier_availability_s +
             maincrop +
             Region +
             survey_year, 
           data = df.full,
           family = "beta",
           control.compute = list(cpo=TRUE, waic=TRUE),
           control.family = list(beta.censor.value = cens)
)


m5df$model = 'rounded'
m_censdf <- prep_inla_data(m_cens)
m_censdf$model = 'censored'
m_compare <- rbind(m5df, m_censdf)
plot_inla_models(m_compare, 2)

