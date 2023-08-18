library(dplyr)
library(tidyr)
library(ggplot2)
library(jtools)
library(INLA)
setwd("~/Documents/Davis/R-Projects/organicseed_adoption/")
df.full <- readRDS("~/Box/organicseed_adoption/df_full.rds") %>% 
  select(CVseed_percent_maincrop_prop,
         legitimation_user_s,
         CertifierRequestn_s,
         noncommercial_seedsource_s,
         acreage_log_s,
         acreage_s,
         cropcat_n_s,
         years_certified_s,
         barrier_buyerReq_s,
         barrier_insuffQuantity_s,
         barrier_lackdesiredTraits_s,
         barrier_availability_s,
         maincrop,
         Region,
         survey_year) %>% 
  na.omit()

# Round the numbers
df.full_rounded <- df.full
df.full_rounded$CVseed_percent_maincrop_prop <- ifelse(df.full$CVseed_percent_maincrop_prop == 0, 0.001,
                                                       ifelse(df.full$CVseed_percent_maincrop_prop == 1, 0.999,df.full$CVseed_percent_maincrop_prop))


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

# Wants lowest value
m1$waic$waic
m2$waic$waic
m3$waic$waic
m4$waic$waic
m5$waic$waic # lowest

# Wants highest value
m1$mlik
m2$mlik
m3$mlik
m4$mlik # highest
m5$mlik

# CPO is a predictive measure -- want to make sure no failures in the measure
table(m1$cpo$failure > 0)
table(m2$cpo$failure > 0)
table(m3$cpo$failure > 0)
table(m4$cpo$failure > 0)
table(m5$cpo$failure > 0)

# Then look for highest
# https://groups.google.com/g/r-inla-discussion-group/c/cWwXBO11jWM?pli=1
mean(log(m1$cpo$cpo), na.rm = T)
mean(log(m2$cpo$cpo), na.rm = T)
mean(log(m3$cpo$cpo), na.rm = T)
mean(log(m4$cpo$cpo), na.rm = T) # Very close
mean(log(m5$cpo$cpo), na.rm = T) # Very close

# https://becarioprecario.bitbucket.io/inla-gitbook/ch-INLA.html
ggplot(m1$summary.linear.predictor, 
       aes(x = 1:nrow(m1$summary.linear.predictor), y = m4$cpo$cpo)) +
  geom_point()

ggplot(m1$summary.linear.predictor, 
       aes(x = 1:nrow(m1$summary.linear.predictor), y = m4$cpo$pit)) +
  geom_point()

residm4 <- df.full$CVseed_percent_maincrop_prop - m4$summary.fitted.values$mean
residm5 <- df.full$CVseed_percent_maincrop_prop - m5$summary.fitted.values$mean

plot_resid <- function(resid, modelname){
  ggplot(df.full, aes(x = 1:nrow(df.full), y = resid)) +
    geom_point() +
    geom_smooth() +
    theme_minimal() +
    labs(x = "Observation", y = "Residual value", title = modelname)
}

plot_resid(residm4, "Model 4") 
plot_resid(residm5, "Model 5")

m1df <- prep_inla_data(m1) %>% mutate(model = 1)
m2df <- prep_inla_data(m2) %>% mutate(model = 2)
m3df <- prep_inla_data(m3) %>% mutate(model = 3)
m4df <- prep_inla_data(m4) %>% mutate(model = 4)
m5df <- prep_inla_data(m5) %>% mutate(model = 5)
m1df %>% 
  rbind(m2df) %>% 
  rbind(m3df) %>% 
  rbind(m4df) %>% 
  rbind(m5df) %>% 
  ggplot(aes(x = mean, 
             y = term, 
             color = factor(model), 
             shape = factor(model))) +
  ggstance::geom_pointrangeh(aes(xmin = LCI, 
                                 xmax = UCI),
                             position = ggstance::position_dodgev(height = 1), 
                             fill = "white", fatten = 2, size = 1) +
  geom_vline(xintercept = 0, linetype = 2, size = 0.25) + 
  scale_colour_manual(values = viridis::mako(n = 5),
                      guide = guide_legend(reverse = TRUE)) + 
  scale_shape_manual(values = 21:25,
                     guide = guide_legend(reverse = TRUE)) +
  jtools::theme_nice(base_family = "Times") + 
  jtools::drop_y_gridlines() + 
  theme(axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        text = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        legend.justification = 0.3,
        panel.grid.major.x = element_line(linetype = "solid")) + 
  labs(x = "Coefficient Estimate", shape = "", color = "", y = "")

df <- prep_inla_data(m4) %>% 
  select(term, mean, LCI, UCI) %>% 
  left_join(prep_inla_data(m3), by = 'term') %>% 
  select(term, contains('mean'), contains('LCI'), contains('UCI')) %>% 
  left_join(prep_inla_data(m2), by = 'term') %>% 
  select(term, contains('mean'), contains('LCI'), contains('UCI')) %>%
  left_join(prep_inla_data(m1), by = 'term') %>% 
  select(term, contains('mean'), contains('LCI'), contains('UCI'))

# Check compare to censoring
# censoring so that values can actually be at 0 o 1
cens <- 0.001
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
summary(m_cens)
m5df <- prep_inla_data(m5)
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