# Mediation
# initially read: https://web.pdx.edu/~newsomj/semclass/ho_mediation.pdf
# then followed: https://tilburgsciencehub.com/topics/analyze/regression/linear-regression/mediation-analysis/#0
library(dplyr)
library(betareg)
source('code/functions.R')
df.full <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled.rds") %>% 
  select(OGseed_percent_maincrop_prop,
         OG_value_s,
         #barrier_allowable_rev_s,
         barrier_all_rev_s,
         CertifierRequestn,
         noncommercial_seedsource_s,
         acreage_log_s,
         cropcat_n_s,
         years_certified_s,
         maincrop,
         Region,
         survey_year) %>% 
  na.omit()
# to play with power BI write.csv(df.full, '~/OneDrive - University of Exeter/data/organicseed_adoption/df.csv', row.names = F)
table(df.full$survey_year)

df.full_rounded <- df.full
df.full_rounded$OGseed_percent_maincrop_prop <- ifelse(df.full_rounded$OGseed_percent_maincrop_prop == 1, 0.999, ifelse(df.full_rounded$OGseed_percent_maincrop_prop == 0, 0.001, df.full_rounded$OGseed_percent_maincrop_prop))

# Farm size and values----
s1 <- betareg(OGseed_percent_maincrop_prop ~ 
                   #barrier_allowable_rev_s +
                   barrier_all_rev_s +
                   OG_value_s + 
                   CertifierRequestn + 
                   #acreage_log_s + 
                   cropcat_n_s +
                   noncommercial_seedsource_s +
                   years_certified_s + 
                   maincrop +
                   Region +
                   survey_year,
               data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(s1)
# first part indirect
s2 <- lm(acreage_log_s  ~ 
           barrier_all_rev_s +
           OG_value_s + 
           CertifierRequestn + 
           #acreage_log_s + 
           cropcat_n_s +
           noncommercial_seedsource_s +
           years_certified_s + 
           maincrop +
           Region +
           survey_year,
         data = df.full_rounded)
summary(s2)

s3 <- betareg(OGseed_percent_maincrop_prop ~ 
                #barrier_allowable_rev_s +
                barrier_all_rev_s +
                #OG_value_s + 
                CertifierRequestn + 
                acreage_log_s + 
                cropcat_n_s +
                noncommercial_seedsource_s +
                years_certified_s + 
                maincrop +
                Region +
                survey_year,
              data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(s3)

s4 <- betareg(OGseed_percent_maincrop_prop ~ 
                #barrier_allowable_rev_s +
                barrier_all_rev_s +
                OG_value_s + 
                CertifierRequestn + 
                acreage_log_s + 
                cropcat_n_s +
                noncommercial_seedsource_s +
                years_certified_s + 
                maincrop +
                Region +
                survey_year,
              data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")

summary(s4)
value_sum <- summary(df.full$OG_value_s)
results1 <- mediation::mediate(s2, s4, treat= 'OG_value_s', 
                               mediator= 'acreage_log_s' ,boot=TRUE, sims=500,
                               control.value = value_sum[[2]],
                               treat.value = value_sum[[5]])
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


# Farm size and barriers ----

set.seed(19)
# total effect
s1 <- betareg(OGseed_percent_maincrop_prop ~ 
                #barrier_allowable_rev_s +
                #barrier_all_rev_s +
                OG_value_s + 
                CertifierRequestn + 
                acreage_log_s + 
                cropcat_n_s +
                noncommercial_seedsource_s +
                years_certified_s + 
                maincrop +
                Region +
                survey_year,
             data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(s1)
# first part indirect
s2 <- lm(barrier_all_rev_s  ~ 
           #barrier_all_rev_s +
           OG_value_s + 
           CertifierRequestn + 
           acreage_log_s + 
           cropcat_n_s +
           noncommercial_seedsource_s +
           years_certified_s + 
           maincrop +
           Region +
           survey_year, 
              data = df.full_rounded)
summary(s2)

# Mediation happening: Assuming there are significant relationships from Steps 1 through 2, one proceeds to Step 4
# In the Step 4 model, some form of mediation is supported if the effect of M (path b) remains significant after controlling for X. If X is no longer significant when M is controlled, the finding supports full mediation. If X is still significant (i.e., both X and M both significantly predict Y), the finding supports partial mediation.
# # second part indirect
s4 <- betareg(OGseed_percent_maincrop_prop ~ 
                #barrier_allowable_rev_s +
                barrier_all_rev_s +
                OG_value_s + 
                CertifierRequestn + 
                acreage_log_s + 
                cropcat_n_s +
                noncommercial_seedsource_s +
                years_certified_s + 
                maincrop +
                Region +
                survey_year,
              data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(s4)


acre_sum <- summary(df.full$acreage_log_s)
results2 <- mediation::mediate(s2, s4, treat= 'acreage_log_s', 
                   mediator= 'barrier_all_rev_s' ,boot=TRUE, sims=1000,
                   control.value = acre_sum[[2]],
                   treat.value = acre_sum[[5]])
summary(results2)

sum2 <- data.frame("Names" = c("ACME (avg)", 
                       "ADE (avg)",
                       "Prop. Mediated (avg) ", 
                       "Totel Effect"),
           "Estimate" = round(c(results2$d.avg, results2$z.avg,
                          results2$n.avg, results2$tau.coef), 3),
           "p-value" = c(results2$d.avg.p, results2$z.avg.p, 
                         results2$n.avg.p, results2$tau.p))

write.csv(sum2, 'tables/append_mediation2.csv', row.names = F)

## Indirect effect: take coefficient from full model and subtract it from coefficient #from model without mediator (barrier)
#s4$coefficients$mean[8] - s1$coefficients$mean[7]
## Or: ^ these should be the same but they aren;t?
#s4$coefficients$mean[4]*s2$coefficients[7]
#
#b1 = s1$coefficients$mean[[7]]
#b2 = s2$coefficients[[7]]
#b3 = s4$coefficients$mean[[4]]
#b4 = s4$coefficients$mean[[8]] # If B4 is not significant or smaller than before in B1#, suggests some mediation -- slightly smaller, still significant; partial mediation
#
#diff1 = b1 - b4 # total minus direct effect
## The relative size of the mediated effect can be assessed using the proportion #mediated, which represents the size of the indirect effect estimated relative to the #total effect estimated.
#
#indirect_effect = b2*b3
#total_effect = b1
#
#100*(indirect_effect/total_effect)
#
## To check if the mediation effect is statistically significant and not only relevant #as is showcased in the proportion test, the Sobel Test is often conducted. To conduct #a Sobel test in R, we will use the bda package. The follwowing syntax is used to #conduct a sobel test:
#
#bda::mediation.test(df.full_rounded$barrier_insuffQuantity_s,
#                    df.full_rounded$OGseed_percent_maincrop_prop,
#                    df.full_rounded$acreage_log_s)
## Recent research has used Bootstrapping over the Sobel Test when assessing the #statistical significance of a mediation effect. Bootstrapping has several advantages #over the Sobel Test when assessing mediation effects

# Crop diversity and availabilty ----

set.seed(19)
# total effect
s1 <- betareg(OGseed_percent_maincrop_prop ~ 
                #barrier_availability_s +
                barrier_lackdesiredTraits_s +
                barrier_insuffQuantity_s +
                barrier_seedTreatlack_s +
                survey_year +
                OG_value_s + 
                acreage_log_s + 
                cropcat_n_s +
                CertifierRequestn +
                noncommercial_seedsource_s +
                maincrop +
                Region, 
              data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(s1)
# first part indirect
s2 <- lm(barrier_availability_s ~ 
           #barrier_availability +
           barrier_lackdesiredTraits_s +
           barrier_insuffQuantity_s +
           barrier_seedTreatlack_s +
           survey_year +
           OG_value_s + 
           acreage_log_s + 
           cropcat_n_s +
           CertifierRequestn +
           noncommercial_seedsource_s +
           maincrop +
           Region, 
         data = df.full_rounded)
summary(s2)

s4 <- betareg(OGseed_percent_maincrop_prop ~ 
                barrier_availability_s  +
                barrier_lackdesiredTraits_s  +
                barrier_insuffQuantity_s  +
                barrier_seedTreatlack_s +
                survey_year +
                OG_value_s  + 
                acreage_log_s  + 
                cropcat_n_s  +
                CertifierRequestn +
                noncommercial_seedsource_s  +
                maincrop +
                Region, 
              data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")

b1 = s1$coefficients$mean[[8]]
b2 = s2$coefficients[[8]]
b3 = s4$coefficients$mean[[2]]
b4 = s4$coefficients$mean[[9]] # If B4 is not significant or smaller than before in B1, suggests some mediation -- slightly smaller, still significant; partial mediation

diff2 = b1 - b4 # total minus direct effect

indirect_effect = b2*b3
total_effect = b1

100*(indirect_effect/total_effect)

bda::mediation.test(df.full_rounded$barrier_availability,
                    df.full_rounded$OGseed_percent_maincrop_prop,
                    df.full_rounded$cropcat_n)

crop_sum <- summary(df.full$cropcat_n_s)
results3 <- mediation::mediate(s2, s4, treat= 'cropcat_n_s', 
                              mediator= 'barrier_availability_s' ,
                              boot=TRUE, sims=1000,
                              control.value = crop_sum[[2]],
                              treat.value = crop_sum[[5]])
summary(results3)

sum3 <- data.frame("Names" = c("ACME (avg)", 
                               "ADE (avg)",
                               "Prop. Mediated (avg) ", 
                               "Totel Effect"),
                   "Estimate" = round(c(results3$d.avg, results3$z.avg,
                                  results3$n.avg, results3$tau.coef), 3),
                   "p-value" = c(results3$d.avg.p, results3$z.avg.p, 
                                 results3$n.avg.p, results3$tau.p))

write.csv(sum3, 'tables/append_mediation3.csv', row.names = F)


# Farm size and quantity by year ----
df11 <- filter(df.full_rounded, survey_year == 2011) %>% 
  na.omit() %>% 
  mutate(acreage_log = log(TotalAcreage),
         acreage_log_s = c(scale(acreage_log, scale = F)),
         cropcat_n_s = c(scale(cropcat_n, scale = T)),
         OG_value_s = c(scale(OG_value, scale = T)),
         barrier_availability_s = c(scale(barrier_availability, scale = T)),
         barrier_lackdesiredTraits_s = c(scale(barrier_lackdesiredTraits, scale = T)),
         barrier_insuffQuantity_s = c(scale(barrier_insuffQuantity, scale = T)),
         barrier_seedTreatlack_s = c(scale(barrier_seedTreatlack, scale = T)),
         noncommercial_seedsource_s = c(scale(noncommercial_seedsource, 
                                              scale = T))
  )

df16 <- filter(df.full_rounded, survey_year == 2016) %>% 
  mutate(acreage_log = log(TotalAcreage),
         acreage_log_s = c(scale(acreage_log, scale = F)),
         cropcat_n_s = c(scale(cropcat_n, scale = T)),
         OG_value_s = c(scale(OG_value, scale = T)),
         barrier_availability_s = c(scale(barrier_availability, scale = T)),
         barrier_lackdesiredTraits_s = c(scale(barrier_lackdesiredTraits, scale = T)),
         barrier_insuffQuantity_s = c(scale(barrier_insuffQuantity, scale = T)),
         barrier_seedTreatlack_s = c(scale(barrier_seedTreatlack, scale = T)),
         noncommercial_seedsource_s = c(scale(noncommercial_seedsource, 
                                              scale = T))
  )
df21 <- filter(df.full_rounded, survey_year == 2021) %>% 
  mutate(acreage_log = log(TotalAcreage),
         acreage_log_s = c(scale(acreage_log, scale = F)),
         cropcat_n_s = c(scale(cropcat_n, scale = T)),
         OG_value_s = c(scale(OG_value, scale = T)),
         barrier_availability_s = c(scale(barrier_availability, scale = T)),
         barrier_lackdesiredTraits_s = c(scale(barrier_lackdesiredTraits, scale = T)),
         barrier_insuffQuantity_s = c(scale(barrier_insuffQuantity, scale = T)),
         barrier_seedTreatlack_s = c(scale(barrier_seedTreatlack, scale = T)),
         noncommercial_seedsource_s = c(scale(noncommercial_seedsource, 
                                              scale = T))
  )

mediation_fx1 <- function(df){
  s1 <- betareg(OGseed_percent_maincrop_prop ~ 
                  barrier_availability_s  +
                  barrier_lackdesiredTraits_s  +
                  # barrier_insuffQuantity  +
                  barrier_seedTreatlack_s +
                  OG_value_s  + 
                  acreage_log_s  + 
                  cropcat_n_s  +
                  CertifierRequestn +
                  noncommercial_seedsource_s  +
                  maincrop +
                  Region, 
                data = df, link = "logit", link.phi = NULL, type = "ML")
  s2 <- lm(barrier_insuffQuantity_s  ~ 
             barrier_availability_s  +
             barrier_lackdesiredTraits_s  +
             #barrier_insuffQuantity +
             barrier_seedTreatlack_s +
             OG_value_s  + 
             acreage_log_s  + 
             cropcat_n_s  +
             CertifierRequestn +
             noncommercial_seedsource_s  +
             maincrop +
             Region, 
           data = df)
  
  s4 <- betareg(OGseed_percent_maincrop_prop ~ 
                  barrier_availability_s  +
                  barrier_lackdesiredTraits_s  +
                  barrier_insuffQuantity_s  +
                  barrier_seedTreatlack_s +
                  OG_value_s  + 
                  acreage_log_s  + 
                  cropcat_n_s  +
                  CertifierRequestn +
                  noncommercial_seedsource_s  +
                  maincrop +
                  Region, 
                data = df, link = "logit", link.phi = NULL, type = "ML")
  
  acre_sum <- summary(df$acreage_log_s)
  results <- mediation::mediate(s2, s4, treat= 'acreage_log_s', 
                                mediator= 'barrier_insuffQuantity_s' ,boot=TRUE, sims=500,
                                control.value = acre_sum[[2]],
                                treat.value = acre_sum[[5]])
  sum <- data.frame("Names" = c("ACME (avg)", 
                                "ADE (avg)",
                                "Prop. Mediated (avg) ", 
                                "Totel Effect"),
                    "Estimate" = round(c(results$d.avg, results$z.avg,
                                         results$n.avg, results$tau.coef), 3),
                    "p-value" = c(results$d.avg.p, results$z.avg.p, 
                                  results$n.avg.p, results$tau.p))
  return(sum)
}
sum11 <- mediation_fx1(df11)
sum16 <- mediation_fx1(df16)
sum21 <- mediation_fx1(df21)

# Crop diversity and availabilty by time ----
mediation_fx2 <- function(df){
  s1 <- betareg(OGseed_percent_maincrop_prop ~ 
                  #barrier_availability_s  +
                  barrier_lackdesiredTraits_s  +
                  barrier_insuffQuantity_s  +
                  barrier_seedTreatlack_s +
                  OG_value_s  + 
                  acreage_log_s  + 
                  cropcat_n_s  +
                  CertifierRequestn +
                  noncommercial_seedsource_s  +
                  maincrop +
                  Region, 
                data = df, link = "logit", link.phi = NULL, type = "ML")
  s2 <- lm(barrier_availability_s  ~ 
             #barrier_availability_s  +
             barrier_lackdesiredTraits_s  +
             barrier_insuffQuantity_s +
             barrier_seedTreatlack_s +
             OG_value_s  + 
             acreage_log_s  + 
             cropcat_n_s  +
             CertifierRequestn +
             noncommercial_seedsource_s  +
             maincrop +
             Region, 
           data = df)
  
  s4 <- betareg(OGseed_percent_maincrop_prop ~ 
                  barrier_availability_s  +
                  barrier_lackdesiredTraits_s  +
                  barrier_insuffQuantity_s  +
                  barrier_seedTreatlack_s +
                  OG_value_s  + 
                  acreage_log_s  + 
                  cropcat_n_s  +
                  CertifierRequestn +
                  noncommercial_seedsource_s  +
                  maincrop +
                  Region, 
                data = df, link = "logit", link.phi = NULL, type = "ML")
  
  crop_sum <- summary(df$cropcat_n_s)
  results <- mediation::mediate(s2, s4, treat= 'cropcat_n_s', 
                                 mediator= 'barrier_availability_s' ,
                                 boot=TRUE, sims=1000,
                                 control.value = crop_sum[[2]],
                                 treat.value = crop_sum[[5]])
  sum <- data.frame("Names" = c("ACME (avg)", 
                                "ADE (avg)",
                                "Prop. Mediated (avg) ", 
                                "Totel Effect"),
                    "Estimate" = round(c(results$d.avg, results$z.avg,
                                         results$n.avg, results$tau.coef), 3),
                    "p-value" = c(results$d.avg.p, results$z.avg.p, 
                                  results$n.avg.p, results$tau.p))
  return(sum)
}
sum11 <- mediation_fx2(df11)
sum16 <- mediation_fx2(df16)
sum21 <- mediation_fx2(df21)

# Simple version ----
# total effect
s1 <- betareg(OGseed_percent_maincrop_prop ~ 
                acreage_log, 
              data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(s1)
# first part indirect
s2 <- lm(barrier_insuffQuantity ~ 
           acreage_log, 
         data = df.full_rounded)
summary(s2)

s4 <- betareg(OGseed_percent_maincrop_prop ~ 
                barrier_insuffQuantity +
                acreage_log, 
              data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(s4)

# Indirect effect is 
b1 = s1$coefficients$mean[[2]]
b2 = s2$coefficients[[2]]
b3 = s4$coefficients$mean[[2]]
b4 = s4$coefficients$mean[[3]] # If B4 is not significant or smaller than before in B1, suggests some mediation -- slightly smaller, still significant; partial mediation

indirect_effect = b2*b3
total_effect = b1

100*(indirect_effect/total_effect)

# Recent research has used Bootstrapping over the Sobel Test when assessing the statistical significance of a mediation effect. Bootstrapping has several advantages over the Sobel Test when assessing mediation effects


results <- mediation::mediate(s2, s4, treat= 'acreage_log', 
                              mediator= 'barrier_insuffQuantity' ,boot=TRUE, sims=500)
summary(results)
