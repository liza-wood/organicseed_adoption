# Setup ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(jtools)
library(INLA)
source('code/functions.R')
setwd("~/Documents/Davis/R-Projects/organicseed_adoption/")
df.full <- readRDS("~/Box/organicseed_adoption/df_scaled.rds") %>% 
  mutate(acreage_s = c(scale(acreage_log, center = T, scale = F))) %>% 
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
m0 <- inla(CVseed_percent_maincrop_prop ~ 1, 
           data = df.full_rounded,
           family = "beta",
           control.compute = list(cpo= T, waic= T))
summary(m0)

# Key system variables only
m1 <- inla(CVseed_percent_maincrop_prop ~ 
             maincrop +
             Region +
             survey_year, 
           data = df.full_rounded,
           family = "beta",
           control.compute = list(cpo= T, waic= T))

# Key operation variables
m2 <- inla(CVseed_percent_maincrop_prop ~ 
             legitimation_user_s + 
             acreage_s + 
             cropcat_n_s,
           data = df.full_rounded,
           family = "beta",
           control.compute = list(cpo= T, waic= T))

# Adding barriers
m3 <- inla(CVseed_percent_maincrop_prop ~ 
             legitimation_user_s + 
             acreage_s + 
             cropcat_n_s +
             barrier_buyerReq_s +
             barrier_insuffQuantity_s +
             barrier_lackdesiredTraits_s +
             barrier_availability_s,
           data = df.full_rounded,
           family = "beta",
           control.compute = list(cpo= T, waic= T))

# More 'system' controls
m4 <- inla(CVseed_percent_maincrop_prop ~ 
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
           family = "beta",
           control.compute = list(cpo= T, waic= T))

# Controls
m5 <- inla(CVseed_percent_maincrop_prop ~ 
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
           family = "beta",
           control.compute = list(cpo= T, waic= T))

# Model results ----
m1df <- prep_inla_data(m1) %>% mutate(model = 1)
m2df <- prep_inla_data(m2) %>% mutate(model = 2)
m3df <- prep_inla_data(m3) %>% mutate(model = 3)
m4df <- prep_inla_data(m4) %>% mutate(model = 4)
m5df <- prep_inla_data(m5) %>% mutate(model = 5)

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
waic = c(m1$waic$waic,
         m2$waic$waic,
         m3$waic$waic,
         m4$waic$waic,
         m5$waic$waic) # lowest

# Wants highest value
mlik = c(m1$mlik[1],
         m2$mlik[1],
         m3$mlik[1],
         m4$mlik[1], # highest
         m5$mlik[1])

# CPO is a predictive measure -- want to make sure no failures in the measure
table(m1$cpo$failure > 0)
table(m2$cpo$failure > 0)
table(m3$cpo$failure > 0)
table(m4$cpo$failure > 0)
table(m5$cpo$failure > 0)

# Then look for highest
# https://groups.google.com/g/r-inla-discussion-group/c/cWwXBO11jWM?pli=1
avglogcpo = c(mean(log(m1$cpo$cpo), na.rm = T),
              mean(log(m2$cpo$cpo), na.rm = T),
              mean(log(m3$cpo$cpo), na.rm = T),
              mean(log(m4$cpo$cpo), na.rm = T), # Very close
              mean(log(m5$cpo$cpo), na.rm = T)) # Very close

tablea2 <- data.frame(model = paste('Model', 1:5),
           waic, mlik, avglogcpo)
tablea2$waic <- round(tablea2$waic)
tablea2$mlik <- round(tablea2$mlik)
tablea2$avglogcpo <- round(tablea2$avglogcpo, 2)

write.csv(tablea2, 'tables/tablea2.csv', row.names = F)

colnames(tablea2) <- c("Model", "WAIC", "Maximum Likelihood", "CPO") ; 
kbl(tablea2, booktabs = T, caption = 'Goodness of fit across models') 

# https://becarioprecario.bitbucket.io/inla-gitbook/ch-INLA.html
ggplot(m1$summary.linear.predictor, 
       aes(x = 1:nrow(m1$summary.linear.predictor), y = m4$cpo$cpo)) +
  geom_point()

ggplot(m1$summary.linear.predictor, 
       aes(x = 1:nrow(m1$summary.linear.predictor), y = m4$cpo$pit)) +
  geom_point()

residm1 <- df.full$CVseed_percent_maincrop_prop - m1$summary.fitted.values$mean
residm2 <- df.full$CVseed_percent_maincrop_prop - m2$summary.fitted.values$mean
residm3 <- df.full$CVseed_percent_maincrop_prop - m3$summary.fitted.values$mean
residm4 <- df.full$CVseed_percent_maincrop_prop - m4$summary.fitted.values$mean
residm5 <- df.full$CVseed_percent_maincrop_prop - m5$summary.fitted.values$mean

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

# Check using betareg
library(betareg)
betaregfit <- betareg(CVseed_percent_maincrop_prop ~ 
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
                      data = df.full_rounded)
summary(betaregfit)
#plot(residuals(betaregfit))

df2011 <- filter(df.full_rounded, survey_year == 2011)
df2016 <- filter(df.full_rounded, survey_year == 2016)
df2021 <- filter(df.full_rounded, survey_year == 2021)