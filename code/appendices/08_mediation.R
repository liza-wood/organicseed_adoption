# Mediation
# initially read: https://web.pdx.edu/~newsomj/semclass/ho_mediation.pdf
# then followed: https://tilburgsciencehub.com/topics/analyze/regression/linear-regression/mediation-analysis/#0
library(dplyr)
library(betareg)
source('code/functions.R')
df.full <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled.rds") %>% 
  select(RespondentID,
         OGseed_percent_maincrop_prop,
         OG_value_s,
         CertifierRequestn,
         noncommercial_seedsource_s,
         acreage_log_s,
         cropcat_n_s,
         barrier_insuffQuantity_s,
         barrier_lackdesiredTraits_s,
         barrier_availability_s,
         maincrop,
         Region,
         survey_year) %>% 
  na.omit()

df.full_rounded <- df.full
df.full_rounded$OGseed_percent_maincrop_prop <- ifelse(df.full_rounded$OGseed_percent_maincrop_prop == 1, 0.999, ifelse(df.full_rounded$OGseed_percent_maincrop_prop == 0, 0.001, df.full_rounded$OGseed_percent_maincrop_prop))


# Farm size and quantity ----

set.seed(19)
# total effect
s1 <- betareg(OGseed_percent_maincrop_prop ~ 
                barrier_availability_s  +
                barrier_lackdesiredTraits_s  +
               # barrier_insuffQuantity  +
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
s2 <- lm(barrier_insuffQuantity_s  ~ 
           barrier_availability_s  +
           barrier_lackdesiredTraits_s  +
           #barrier_insuffQuantity +
           survey_year +
           OG_value_s  + 
           acreage_log_s  + 
           cropcat_n_s  +
           CertifierRequestn +
           noncommercial_seedsource_s  +
           maincrop +
           Region, 
              data = df.full_rounded)
summary(s2)

# Mediation happening: Assuming there are significant relationships from Steps 1 through 2, one proceeds to Step 4
# In the Step 4 model, some form of mediation is supported if the effect of M (path b) remains significant after controlling for X. If X is no longer significant when M is controlled, the finding supports full mediation. If X is still significant (i.e., both X and M both significantly predict Y), the finding supports partial mediation.
# # second part indirect
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
                Region, 
              data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(s4)

# Indirect effect: take coefficient from full model and subtract it from coefficient from model without mediator (barrier)
s4$coefficients$mean[8] - s1$coefficients$mean[7]
# Or: ^ these should be the same but they aren;t?
s4$coefficients$mean[4]*s2$coefficients[7]

b1 = s1$coefficients$mean[[7]]
b2 = s2$coefficients[[7]]
b3 = s4$coefficients$mean[[4]]
b4 = s4$coefficients$mean[[8]] # If B4 is not significant or smaller than before in B1, suggests some mediation -- slightly smaller, still significant; partial mediation

diff1 = b1 - b4 # total minus direct effect
# The relative size of the mediated effect can be assessed using the proportion mediated, which represents the size of the indirect effect estimated relative to the total effect estimated.

indirect_effect = b2*b3
total_effect = b1

100*(indirect_effect/total_effect)

# To check if the mediation effect is statistically significant and not only relevant as is showcased in the proportion test, the Sobel Test is often conducted. To conduct a Sobel test in R, we will use the bda package. The follwowing syntax is used to conduct a sobel test:

bda::mediation.test(df.full_rounded$barrier_insuffQuantity_s,
                    df.full_rounded$OGseed_percent_maincrop_prop,
                    df.full_rounded$acreage_log_s)
# Recent research has used Bootstrapping over the Sobel Test when assessing the statistical significance of a mediation effect. Bootstrapping has several advantages over the Sobel Test when assessing mediation effects

acre_sum <- summary(df.full$acreage_log_s)
results1 <- mediation::mediate(s2, s4, treat= 'acreage_log_s', 
                   mediator= 'barrier_insuffQuantity_s' ,boot=TRUE, sims=1000,
                   control.value = acre_sum[[2]],
                   treat.value = acre_sum[[5]])
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

# Crop diversity and availabilty ----

set.seed(19)
# total effect
s1 <- betareg(OGseed_percent_maincrop_prop ~ 
                #barrier_availability +
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
              data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(s1)
# first part indirect
s2 <- lm(barrier_availability_s ~ 
           #barrier_availability +
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
         data = df.full_rounded)
summary(s2)

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
results2 <- mediation::mediate(s2, s4, treat= 'cropcat_n_s', 
                              mediator= 'barrier_availability_s' ,
                              boot=TRUE, sims=1000,
                              control.value = crop_sum[[2]],
                              treat.value = crop_sum[[5]])
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
