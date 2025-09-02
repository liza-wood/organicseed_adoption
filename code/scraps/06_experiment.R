
# First just looking at things descriptively for correlations 
library(dplyr)
library(ggplot2)
df <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled.rds") %>% 
  dplyr::select(OGseed_percent_maincrop_prop,
         OG_value,
         CertifierRequestf,
         noncommercial_seedsource,
         TotalAcreage,
         acreage_log,
         cropcat_n,
         #barrier_buyerReq_s,
         #barrier_insuffQuantity,
         #barrier_lackdesiredTraits,
         barrier_availability,
         #barrier_seedTreatlack,
         barrier_allowable,
         maincrop,
         Region,
         survey_year) %>% 
  na.omit()

# Correlations of size, duty, and certain barriers

# SIZE AND VALUE: significant and negative but effect size is small
df$OG_value_round <- round(df$OG_value)
cor(df$acreage_log, df$OG_value)
summary(lm(OG_value_round ~ acreage_log, df))
summary(lm(OG_value_round ~ TotalAcreage, df))
ggplot(df, aes(x = OG_value, y = acreage_log)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = 'lm')
ggplot(df, aes(x = OG_value, y = TotalAcreage)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = 'lm') +
  ylim(0, 7500)

# SIZE AND AVAILABILITY: No clear/immediate relationship between size and availability
#cor(df$acreage_log, df$barrier_availability)
summary(lm(barrier_availability ~ acreage_log, df))
ggplot(df, aes(x = barrier_availability, y = acreage_log)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = 'lm')

# Re-run beta models ----

dfs <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled.rds") %>% 
  dplyr::select(OGseed_percent_maincrop_prop,
         OG_value_s,
         #CertifierRequestf,
         CertifierRequestn,
         noncommercial_seedsource_s,
         acreage_log_s,
         cropcat_n_s,
         barrier_allowable_s,
         barrier_allowable_rev_s,
         #barrier_nonallowable_s,
         #barrier_insuffQuantity_s,
         #barrier_lackdesiredTraits_s,
         #barrier_availability_s,
         #barrier_seedTreatlack_s,
         maincrop,
         Region,
         survey_year, YearsCertified) %>% 
  na.omit()

dfs_rounded <- dfs
dfs_rounded$OGseed_percent_maincrop_prop <- ifelse(dfs_rounded$OGseed_percent_maincrop_prop == 1, 0.999, ifelse(dfs_rounded$OGseed_percent_maincrop_prop == 0, 0.001, dfs_rounded$OGseed_percent_maincrop_prop))

#dfs_rounded$NoEnforcement <- ifelse(dfs_rounded$CertifierRequestn == 1, 0, 1)
#dfs_rounded$barrier_allowable_sum_s <- dfs_rounded$barrier_allowable_sum_s/4
set.seed(19)
library(betareg)
#cor(dfs$barrier_allowable_s, dfs$barrier_nonallowable_s)
cor(dfs$barrier_allowable_s, dfs$OG_value_s)
summary(lm(barrier_allowable_s ~ OG_value_s, dfs))
m_null <- betareg(OGseed_percent_maincrop_prop ~ 
                     barrier_allowable_rev_s +
                     OG_value_s,
                   data = dfs_rounded, link = "logit", link.phi = "log", type = "ML")
summary(m_null)
m_noix1 <- betareg(OGseed_percent_maincrop_prop ~ 
                    barrier_allowable_rev_s +
                    #barrier_nonallowable_s +
                    #OG_value_s + 
                    #barrier_allowable_s*OG_value_s +
                    #CertifierRequestn*OG_value_s +
                    #barrier_allowable_s*CertifierRequestn +
                    #barrier_nonallowable_s*OG_value_s +
                    #barrier_nonallowable_s*CertifierRequestn +
                    #barrier_allowable_s*acreage_log_s +
                    #CertifierRequestf + 
                    CertifierRequestn + 
                    acreage_log_s + 
                    cropcat_n_s +
                    YearsCertified + 
                    noncommercial_seedsource_s +
                    maincrop +
                    Region +
                    survey_year, 
                  data = dfs_rounded, link = "logit", link.phi = "log", type = "ML")
summary(m_noix1)
m_noix2 <- betareg(OGseed_percent_maincrop_prop ~ 
                     #barrier_allowable_s +
                     #barrier_nonallowable_s +
                     OG_value_s + 
                     #barrier_allowable_s*OG_value_s +
                     #CertifierRequestn*OG_value_s +
                     #barrier_allowable_s*CertifierRequestn +
                     #barrier_nonallowable_s*OG_value_s +
                     #barrier_nonallowable_s*CertifierRequestn +
                     #barrier_allowable_s*acreage_log_s +
                     #CertifierRequestf + 
                     CertifierRequestn + 
                     acreage_log_s + 
                     cropcat_n_s +
                     YearsCertified + 
                     noncommercial_seedsource_s +
                     maincrop +
                     Region +
                     survey_year, 
                   data = dfs_rounded, link = "logit", link.phi = "log", type = "ML")
summary(m_noix2)
m_noix <- betareg(OGseed_percent_maincrop_prop ~ 
                       barrier_allowable_rev_s +
                       #barrier_nonallowable_s +
                       OG_value_s + 
                       #barrier_allowable_s*OG_value_s +
                       #CertifierRequestn*OG_value_s +
                       #barrier_allowable_s*CertifierRequestn +
                       #barrier_nonallowable_s*OG_value_s +
                       #barrier_nonallowable_s*CertifierRequestn +
                       #barrier_allowable_s*acreage_log_s +
                       #CertifierRequestf + 
                       CertifierRequestn + 
                       acreage_log_s + 
                       cropcat_n_s +
                       YearsCertified + 
                       noncommercial_seedsource_s +
                       maincrop +
                       Region +
                       survey_year, 
             data = dfs_rounded, link = "logit", link.phi = "log", type = "ML")
summary(m_noix)
m_ix <- betareg(OGseed_percent_maincrop_prop ~ 
                  barrier_allowable_rev_s +
                  #barrier_nonallowable_s +
                  OG_value_s + 
                  OG_value_s*barrier_allowable_rev_s +
                  #CertifierRequestn*OG_value_s +
                  #barrier_allowable_s*CertifierRequestn +
                  #barrier_nonallowable_s*OG_value_s +
                  #barrier_nonallowable_s*CertifierRequestn +
                  #CertifierRequestf + 
                  CertifierRequestn + 
                  acreage_log_s + 
                  cropcat_n_s +
                  YearsCertified + 
                  noncommercial_seedsource_s +
                  maincrop +
                  Region +
                  survey_year,  
                  data = dfs_rounded, link = "logit", link.phi = "log", type = "ML")
summary(m_ix)
stargazer::stargazer(m_noix1, m_noix2, m_noix, m_ix, type = 'html', out = 'tables/test_gazer_betrareg.html')
# When together, allowable is -.18 and non-allowable is -.16
# Just allowable, no interaction -0.22
# Just non-allowable, no interaction -0.15

bara <- quantile(dfs_rounded$barrier_allowable_s)[[4]]
bar <- quantile(dfs_rounded$barrier_allowable_s)[[3]]
barb <- quantile(dfs_rounded$barrier_allowable_s)[[2]]
summary(dfs_rounded$barrier_allowable_s)
vala <- quantile(dfs_rounded$OG_value_s)[[4]]
val <- quantile(dfs_rounded$OG_value_s)[[3]]
valb <- quantile(dfs_rounded$OG_value_s)[[2]]
summary(dfs_rounded$OG_value_s)
mylist <- list(barrier_allowable_s=c(1,3),
               OG_value_s=c(-2,-1,0,1,2))
emmeans::emmip(m_ix, barrier_allowable_s ~ OG_value_s,at=mylist, CIs=TRUE)

# Playing with certifier request
df_noncomp <- dfs_rounded[dfs_rounded$CertifierRequestf != "Fully compliant",]
m_noix <- betareg(OGseed_percent_maincrop_prop ~ 
                    barrier_allowable_sum_s +
                    #barrier_nonallowable_s +
                    OG_value_s + 
                    #barrier_allowable_s*OG_value_s +
                    #CertifierRequestn*OG_value_s +
                    #barrier_allowable_s*CertifierRequestn +
                    #barrier_nonallowable_s*OG_value_s +
                    #barrier_nonallowable_s*CertifierRequestn +
                    #barrier_allowable_s*acreage_log_s +
                    CertifierRequestf + 
                    acreage_log_s + 
                    cropcat_n_s +
                    YearsCertified + 
                    noncommercial_seedsource_s +
                    maincrop +
                    Region +
                    survey_year, 
                  data = df_noncomp, link = "logit", link.phi = "log", type = "ML")
summary(m_noix)
m_ix <- betareg(OGseed_percent_maincrop_prop ~ 
                  barrier_allowable_s +
                  #barrier_nonallowable_s +
                  OG_value_s + 
                  OG_value_s*barrier_allowable_s +
                  #CertifierRequestn*OG_value_s +
                  #barrier_allowable_s*CertifierRequestn +
                  #barrier_nonallowable_s*OG_value_s +
                  #barrier_nonallowable_s*CertifierRequestn +
                  CertifierRequestf + 
                  acreage_log_s + 
                  cropcat_n_s +
                  YearsCertified + 
                  noncommercial_seedsource_s +
                  maincrop +
                  Region +
                  survey_year,  
                data = df_noncomp, link = "logit", link.phi = "log", type = "ML")
summary(m_ix)
m_2ix <- betareg(OGseed_percent_maincrop_prop ~ 
                  barrier_allowable_s +
                  #barrier_nonallowable_s +
                  OG_value_s + 
                  OG_value_s*barrier_allowable_s +
                  #CertifierRequestn*OG_value_s +
                  barrier_allowable_s*CertifierRequestf +
                  #barrier_nonallowable_s*OG_value_s +
                  #barrier_nonallowable_s*CertifierRequestn +
                  CertifierRequestf + 
                  acreage_log_s + 
                  cropcat_n_s +
                  YearsCertified + 
                  noncommercial_seedsource_s +
                  maincrop +
                  Region +
                  survey_year,  
                data = df_noncomp, link = "logit", link.phi = "log", type = "ML")
summary(m_2ix)
stargazer::stargazer(m_noix, m_ix, type = 'html', out = 'tables/test_gazer_betrareg.html')


# Playing with allorable and non
m_a_controlonly <- betareg(OGseed_percent_maincrop_prop ~ 
                      NoEnforcement + 
                      acreage_log_s + 
                      cropcat_n_s +
                      YearsCertified + 
                      noncommercial_seedsource_s +
                      maincrop +
                      Region +
                      survey_year, 
                    data = dfs_rounded, link = "logit", link.phi = "log", type = "ML")

m_a_nocontrol <- betareg(OGseed_percent_maincrop_prop ~ 
                           barrier_allowable_s +
                           #barrier_nonallowable_s +
                           OG_value_s + 
                           maincrop +
                           Region +
                           survey_year, 
                         data = dfs_rounded, link = "logit", link.phi = "log", type = "ML")

m_a_nocontrolorfe <- betareg(OGseed_percent_maincrop_prop ~ 
                           barrier_allowable_s +
                           #barrier_nonallowable_s +
                           OG_value_s, 
                         data = dfs_rounded, link = "logit", link.phi = "log", type = "ML")



m_a_noix <- betareg(OGseed_percent_maincrop_prop ~ 
                    barrier_allowable_s +
                    #barrier_nonallowable_s +
                    OG_value_s + 
                    #barrier_allowable_s*OG_value_s +
                    #CertifierRequestn*OG_value_s +
                    #barrier_allowable_s*CertifierRequestn +
                    #barrier_nonallowable_s*OG_value_s +
                    #barrier_nonallowable_s*CertifierRequestn +
                    #barrier_allowable_s*acreage_log_s +
                    NoEnforcement + 
                    acreage_log_s + 
                    cropcat_n_s +
                    YearsCertified + 
                    noncommercial_seedsource_s +
                    maincrop +
                    Region +
                    survey_year, 
                  data = dfs_rounded, link = "logit", link.phi = "log", type = "ML")
m_a_ix <- betareg(OGseed_percent_maincrop_prop ~ 
                  barrier_allowable_s +
                  #barrier_nonallowable_s +
                  OG_value_s + 
                  OG_value_s*barrier_allowable_s +
                  #CertifierRequestn*OG_value_s +
                  #barrier_allowable_s*CertifierRequestn +
                  #barrier_nonallowable_s*OG_value_s +
                  #barrier_nonallowable_s*CertifierRequestn +
                  #barrier_allowable_s*acreage_log_s +
                  NoEnforcement + 
                  acreage_log_s + 
                  cropcat_n_s +
                  YearsCertified + 
                  noncommercial_seedsource_s +
                  maincrop +
                  Region +
                  survey_year,  
                data = dfs_rounded, link = "logit", link.phi = "log", type = "ML")
m_na_noix <- betareg(OGseed_percent_maincrop_prop ~ 
                      #barrier_allowable_s +
                      barrier_nonallowable_s +
                      OG_value_s + 
                      #barrier_allowable_s*OG_value_s +
                      #CertifierRequestn*OG_value_s +
                      #barrier_allowable_s*CertifierRequestn +
                      #barrier_nonallowable_s*OG_value_s +
                      #barrier_nonallowable_s*CertifierRequestn +
                      #barrier_allowable_s*acreage_log_s +
                      NoEnforcement + 
                      acreage_log_s + 
                      cropcat_n_s +
                      YearsCertified + 
                      noncommercial_seedsource_s +
                      maincrop +
                      Region +
                      survey_year, 
                    data = dfs_rounded, link = "logit", link.phi = "log", type = "ML")
m_na_ix <- betareg(OGseed_percent_maincrop_prop ~ 
                    #barrier_allowable_s +
                    barrier_nonallowable_s +
                    OG_value_s + 
                    #OG_value_s*barrier_allowable_s +
                    #CertifierRequestn*OG_value_s +
                    #barrier_allowable_s*CertifierRequestn +
                    barrier_nonallowable_s*OG_value_s +
                    #barrier_nonallowable_s*CertifierRequestn +
                    #barrier_allowable_s*acreage_log_s +
                    NoEnforcement + 
                    acreage_log_s + 
                    cropcat_n_s +
                    YearsCertified + 
                    noncommercial_seedsource_s +
                    maincrop +
                    Region +
                    survey_year,  
                  data = dfs_rounded, link = "logit", link.phi = "log", type = "ML")
m_both_noix <- betareg(OGseed_percent_maincrop_prop ~ 
                      barrier_allowable_s +
                      barrier_nonallowable_s +
                      OG_value_s + 
                      #barrier_allowable_s*OG_value_s +
                      #CertifierRequestn*OG_value_s +
                      #barrier_allowable_s*CertifierRequestn +
                      #barrier_nonallowable_s*OG_value_s +
                      #barrier_nonallowable_s*CertifierRequestn +
                      #barrier_allowable_s*acreage_log_s +
                      NoEnforcement + 
                      acreage_log_s + 
                      cropcat_n_s +
                      YearsCertified + 
                      noncommercial_seedsource_s +
                      maincrop +
                      Region +
                      survey_year, 
                    data = dfs_rounded, link = "logit", link.phi = "log", type = "ML")
m_both_ix <- betareg(OGseed_percent_maincrop_prop ~ 
                     barrier_allowable_s +
                     barrier_nonallowable_s +
                     OG_value_s + 
                     #OG_value_s*barrier_allowable_s +
                     #CertifierRequestn*OG_value_s +
                     #barrier_allowable_s*CertifierRequestn +
                     #barrier_nonallowable_s*OG_value_s +
                     #barrier_nonallowable_s*CertifierRequestn +
                     #barrier_allowable_s*acreage_log_s +
                     barrier_allowable_s*barrier_nonallowable_s*OG_value_s+
                     NoEnforcement + 
                     acreage_log_s + 
                     cropcat_n_s +
                     YearsCertified + 
                     noncommercial_seedsource_s +
                     maincrop +
                     Region +
                     survey_year,  
                   data = dfs_rounded, link = "logit", link.phi = "log", type = "ML")

stargazer::stargazer(#m_a_controlonly, 
                     #m_a_nocontrol, 
                     #m_a_nocontrolorfe,
                     m_a_noix, m_a_ix,
                     m_na_noix, m_na_ix,
                     m_both_noix, #m_both_ix,
                     type = 'html', out = 'tables/test_gazer_betrareg.html')

summary(dfs_rounded$barrier_allowable_s)
bar_a1 <- quantile(dfs_rounded$barrier_allowable_s)[[4]]
bar_a2 <- quantile(dfs_rounded$barrier_allowable_s)[[2]]
summary(dfs_rounded$OG_value_s)
val_1 <- quantile(dfs_rounded$OG_value_s)[[4]]
val_2 <- quantile(dfs_rounded$OG_value_s)[[2]]

mylist <- list(barrier_allowable_s=c(1, 3),
               OG_value_s=c(-1,0,1,2))
emmeans::emmip(m_a_ix, OG_value_s~barrier_allowable_s,at=mylist, CIs=TRUE)
emmeans::emmip(m_a_ix, barrier_allowable_s~ OG_value_s,at=mylist, CIs=TRUE)

mylist <- list(barrier_allowable_s=c(1, 3),
               barrier_nonallowable_s=c(1, 3))
emmeans::emmip(m_both_ix, barrier_nonallowable_s~barrier_allowable_s,at=mylist, CIs=TRUE)

summary(dfs_rounded$barrier_nonallowable_s)
bar_na1 <- 3
bar_na2 <- 1

mylist <- list(barrier_allowable_s=c(1, 3),
               barrier_nonallowable_s=c(1, 3),
               OG_value_s=c(-1,0,1, 2))

emmip(m_both_ix, barrier_nonallowable_s ~ barrier_allowable_s | OG_value_s,
      at = mylist, CI = T)

df10 <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled10.rds") 
df15 <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled15.rds") 
df20 <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled20.rds") 
df10$OGseed_percent_maincrop_prop <- ifelse(df10$OGseed_percent_maincrop_prop == 1, 0.999, ifelse(df10$OGseed_percent_maincrop_prop == 0, 0.001, df10$OGseed_percent_maincrop_prop))
df15$OGseed_percent_maincrop_prop <- ifelse(df15$OGseed_percent_maincrop_prop == 1, 0.999, ifelse(df15$OGseed_percent_maincrop_prop == 0, 0.001, df15$OGseed_percent_maincrop_prop))
df20$OGseed_percent_maincrop_prop <- ifelse(df20$OGseed_percent_maincrop_prop == 1, 0.999, ifelse(df20$OGseed_percent_maincrop_prop == 0, 0.001, df20$OGseed_percent_maincrop_prop))

df10$NoEnforcement <- ifelse(df10$CertifierRequestn == 1, 0, 1)
m10_noix <- betareg(OGseed_percent_maincrop_prop ~ 
                 barrier_allowable_s +
                 #barrier_nonallowable_s +
                 OG_value_s + 
                 #barrier_allowable_s*OG_value_s +
                 #CertifierRequestn*OG_value_s +
                 #barrier_allowable_s*CertifierRequestn +
                 #barrier_nonallowable_s*OG_value_s +
                 #barrier_nonallowable_s*CertifierRequestn +
                 #barrier_allowable_s*acreage_log_s +
                 #NoEnforcement + 
                 acreage_log_s + 
                 cropcat_n_s +
                 YearsCertified + 
                 noncommercial_seedsource_s +
                 maincrop +
                 Region,
             data = df10, link = "logit", link.phi = "log", type = "ML")
m10_ix <- betareg(OGseed_percent_maincrop_prop ~ 
                      barrier_allowable_s +
                      #barrier_nonallowable_s +
                      OG_value_s + 
                      barrier_allowable_s*OG_value_s +
                      #CertifierRequestn*OG_value_s +
                      #barrier_allowable_s*CertifierRequestn +
                      #barrier_nonallowable_s*OG_value_s +
                      #barrier_nonallowable_s*CertifierRequestn +
                      #barrier_allowable_s*acreage_log_s +
                      #NoEnforcement + 
                      acreage_log_s + 
                      cropcat_n_s +
                      YearsCertified + 
                      noncommercial_seedsource_s +
                      maincrop +
                      Region,
                    data = df10, link = "logit", link.phi = "log", type = "ML")
df15$NoEnforcement <- ifelse(df15$CertifierRequestn == 1, 0, 1)
m15_noix <- betareg(OGseed_percent_maincrop_prop ~ 
                 barrier_allowable_s +
                 #barrier_nonallowable_s +
                 OG_value_s + 
                 #barrier_allowable_s*OG_value_s +
                 #CertifierRequestn*OG_value_s +
                 #barrier_allowable_s*CertifierRequestn +
                 #barrier_nonallowable_s*OG_value_s +
                 #barrier_nonallowable_s*CertifierRequestn +
                 #barrier_allowable_s*acreage_log_s +
                 #NoEnforcement + 
                 acreage_log_s + 
                 cropcat_n_s +
                 YearsCertified + 
                 noncommercial_seedsource_s +
                 maincrop +
                 Region,
               data = df15, link = "logit", link.phi = "log", type = "ML")
m15_ix <- betareg(OGseed_percent_maincrop_prop ~ 
                      barrier_allowable_s +
                      #barrier_nonallowable_s +
                      OG_value_s + 
                      barrier_allowable_s*OG_value_s +
                      #CertifierRequestn*OG_value_s +
                      #barrier_allowable_s*CertifierRequestn +
                      #barrier_nonallowable_s*OG_value_s +
                      #barrier_nonallowable_s*CertifierRequestn +
                      #barrier_allowable_s*acreage_log_s +
                      #NoEnforcement + 
                      acreage_log_s + 
                      cropcat_n_s +
                      YearsCertified + 
                      noncommercial_seedsource_s +
                      maincrop +
                      Region,
                    data = df15, link = "logit", link.phi = "log", type = "ML")
df20$NoEnforcement <- ifelse(df20$CertifierRequestn == 1, 0, 1)
m20_noix <- betareg(OGseed_percent_maincrop_prop ~ 
                 barrier_allowable_s +
                 #barrier_nonallowable_s +
                 OG_value_s + 
                 #barrier_allowable_s*OG_value_s +
                 #CertifierRequestn*OG_value_s +
                 #barrier_allowable_s*CertifierRequestn +
                 #barrier_nonallowable_s*OG_value_s +
                 #barrier_nonallowable_s*CertifierRequestn +
                 #barrier_allowable_s*acreage_log_s +
                 #NoEnforcement + 
                 acreage_log_s + 
                 cropcat_n_s +
                 YearsCertified + 
                 noncommercial_seedsource_s +
                 maincrop +
                 Region,
               data = df20, link = "logit", link.phi = "log", type = "ML")
m20_ix <- betareg(OGseed_percent_maincrop_prop ~ 
                 barrier_allowable_s +
                 #barrier_nonallowable_s +
                 OG_value_s + 
                 barrier_allowable_s*OG_value_s +
                 #CertifierRequestn*OG_value_s +
                 #barrier_allowable_s*CertifierRequestn +
                 #barrier_nonallowable_s*OG_value_s +
                 #barrier_nonallowable_s*CertifierRequestn +
                 #barrier_allowable_s*acreage_log_s +
                 #NoEnforcement + 
                 acreage_log_s + 
                 cropcat_n_s +
                 YearsCertified + 
                 noncommercial_seedsource_s +
                 maincrop +
                 Region,
               data = df20, link = "logit", link.phi = "log", type = "ML")
stargazer::stargazer(m10_noix, m10_ix,
                     m15_noix, m15_ix, 
                     m20_noix, m20_ix, type = 'html', out = 'tables/test_gazer_betrareg_yrly.html')


# Run increase models ----
dfs2 <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled_increase.rds") %>% 
  dplyr::select(IncreasedOGSeed_f,
         OG_value_s,
         CertifierRequestn,
         noncommercial_seedsource_s,
         acreage_log_s,
         cropcat_n_s,
         barrier_allowable_s,
         barrier_nonallowable_s,
         #barrier_insuffQuantity_s,
         #barrier_lackdesiredTraits_s,
         #barrier_availability_s,
         #barrier_seedTreatlack_s,
         maincrop,
         Region,
         survey_year,
         YearsCertified) %>% 
  na.omit()

levels(dfs2$IncreasedOGSeed_f)

dfs2$NoEnforcement <- ifelse(dfs2$CertifierRequestn == 1, 0, 1)

library(nnet)
mm_noix <- multinom(IncreasedOGSeed_f ~ 
                 barrier_allowable_s +
                 #barrier_nonallowable_s +
                 OG_value_s + 
                 #barrier_allowable_s*OG_value_s +
                 #CertifierRequestn*OG_value_s +
                 #barrier_allowable_s*CertifierRequestn +
                 #barrier_nonallowable_s*OG_value_s +
                 #barrier_nonallowable_s*CertifierRequestn +
                 #barrier_allowable_s*acreage_log_s +
                 NoEnforcement + 
                 acreage_log_s + 
                 cropcat_n_s +
                 YearsCertified + 
                 noncommercial_seedsource_s +
                 maincrop +
                 Region +
                 survey_year, dfs2)
mm_ix <- multinom(IncreasedOGSeed_f ~ 
                      barrier_allowable_s +
                      #barrier_nonallowable_s +
                      OG_value_s + 
                      barrier_allowable_s*OG_value_s +
                      #CertifierRequestn*OG_value_s +
                      #barrier_allowable_s*CertifierRequestn +
                      #barrier_nonallowable_s*OG_value_s +
                      #barrier_nonallowable_s*CertifierRequestn +
                      #barrier_allowable_s*acreage_log_s +
                      NoEnforcement + 
                      acreage_log_s + 
                      cropcat_n_s +
                      YearsCertified + 
                      noncommercial_seedsource_s +
                      maincrop +
                      Region +
                      survey_year, dfs2)

mm_3ix <- multinom(IncreasedOGSeed_f ~ 
                    barrier_allowable_s +
                    #barrier_nonallowable_s +
                    OG_value_s + 
                    barrier_allowable_s*OG_value_s*NoEnforcement +
                    #CertifierRequestn*OG_value_s +
                    #barrier_allowable_s*CertifierRequestn +
                    #barrier_nonallowable_s*OG_value_s +
                    #barrier_nonallowable_s*CertifierRequestn +
                    #barrier_allowable_s*acreage_log_s +
                    NoEnforcement + 
                    acreage_log_s + 
                    cropcat_n_s +
                    YearsCertified + 
                    noncommercial_seedsource_s +
                    maincrop +
                    Region +
                    survey_year, dfs2)

stargazer::stargazer(mm_noix, mm_ix, type = 'html', out = 'tables/test_gazer_multinom.html')

library(MASS)
polr.model <- polr(IncreasedOGSeed_f ~ 
                     barrier_allowable_s +
                     barrier_nonallowable_s +
                     OG_value_s + 
                     #barrier_allowable_s*OG_value_s +
                     #CertifierRequestn*OG_value_s +
                     barrier_allowable_s*CertifierRequestn +
                     #barrier_nonallowable_s*OG_value_s +
                     barrier_nonallowable_s*CertifierRequestn +
                     acreage_log_s + 
                     cropcat_n_s +
                     YearsCertified +
                     noncommercial_seedsource_s +
                     maincrop +
                     Region +
                     survey_year, dfs2, Hess = T) 
# We use Hess = T in order to return the Hessian matrix, which allows us to look at the model's summary
summary(polr.model)
# Save an object as the coefficient table
ctable <- coef(summary(polr.model))
# Calculate and store p values
p <- round((pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2),3)
# Bind these objects into one table
table <- cbind(ctable, "p value" = p)
table
brant::brant(polr.model) # but these are not parallel, so maybe multinom is better

library(VGAM)

# a non-proportional model
vgam.model.nonprop <- vglm(ordered(IncreasedOGSeed_f) ~ 
                             barrier_allowable_s +
                             barrier_nonallowable_s +
                             OG_value_s + 
                             #barrier_allowable_s*OG_value_s +
                             #CertifierRequestn*OG_value_s +
                             barrier_allowable_s*CertifierRequestn +
                             #barrier_nonallowable_s*OG_value_s +
                             barrier_nonallowable_s*CertifierRequestn +
                             acreage_log_s + 
                             cropcat_n_s +
                             YearsCertified + 
                             noncommercial_seedsource_s +
                             maincrop +
                             Region +
                             survey_year, dfs2,
                           family=cumulative(parallel = F, reverse = T))

summary(vgam.model.nonprop)


sm_prop <- vglm(ordered(IncreasedOGSeed_f) ~ 
                  barrier_allowable_s +
                  barrier_nonallowable_s +
                  OG_value_s + 
                  #barrier_allowable_s*OG_value_s +
                  #CertifierRequestn*OG_value_s +
                  barrier_allowable_s*CertifierRequestn +
                  #barrier_nonallowable_s*OG_value_s +
                  barrier_nonallowable_s*CertifierRequestn +
                  acreage_log_s + 
                  cropcat_n_s +
                  YearsCertified + 
                  noncommercial_seedsource_s +
                  maincrop +
                  Region +
                  survey_year, dfs2,
          family=cratio(parallel = F))

sm_nonprop <- vglm(ordered(IncreasedOGSeed_f) ~ 
                     barrier_allowable_s +
                     barrier_nonallowable_s +
                     OG_value_s + 
                     #barrier_allowable_s*OG_value_s +
                     #CertifierRequestn*OG_value_s +
                     barrier_allowable_s*CertifierRequestn +
                     #barrier_nonallowable_s*OG_value_s +
                     barrier_nonallowable_s*CertifierRequestn +
                     acreage_log_s + 
                     cropcat_n_s +
                     YearsCertified + 
                     noncommercial_seedsource_s +
                     maincrop +
                     Region +
                     survey_year, dfs2,
                     family=cratio(parallel = F))
summary(sm_nonprop)

# Is propotional odds appropriate -- no?
dev <- deviance(sm_prop) - deviance(sm_nonprop)
degf <- sm_prop@df.residual - sm_nonprop@df.residual
p.value <- 1 - pchisq(q = dev , df = degf)
p.value

dfs10 <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled_increase10.rds") 
dfs15 <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled_increase15.rds") 
dfs20 <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled_increase20.rds") 
table(dfs10$IncreasedOGSeed_f)
dfs10$IncreasedOGSeed_f[dfs10$IncreasedOGSeed_f == 'Already at 100%'] <- 'Yes: Increased the %'
dfs10$IncreasedOGSeed_f <- factor(dfs10$IncreasedOGSeed_f, c('No: About the same % or decreased',
                                                             'Yes: Increased the %'))
mm10 <- multinom(IncreasedOGSeed_f ~ 
                   barrier_allowable_s +
                   #barrier_nonallowable_s +
                   OG_value_s + 
                   #barrier_allowable_s*OG_value_s +
                   #CertifierRequestn*OG_value_s +
                   barrier_allowable_s*CertifierRequestn +
                   #barrier_nonallowable_s*OG_value_s +
                   #barrier_nonallowable_s*CertifierRequestn +
                   acreage_log_s + 
                   cropcat_n_s +
                   YearsCertified + 
                   noncommercial_seedsource_s +
                   maincrop +
                   Region, dfs10)
mm15 <- multinom(IncreasedOGSeed_f ~ 
                   barrier_allowable_s +
                   #barrier_nonallowable_s +
                   OG_value_s + 
                   #barrier_allowable_s*OG_value_s +
                   #CertifierRequestn*OG_value_s +
                   barrier_allowable_s*CertifierRequestn +
                   #barrier_nonallowable_s*OG_value_s +
                   #barrier_nonallowable_s*CertifierRequestn +
                   acreage_log_s + 
                   cropcat_n_s +
                   YearsCertified + 
                   noncommercial_seedsource_s +
                   maincrop +
                   Region, dfs15)

mm20 <- multinom(IncreasedOGSeed_f ~ 
                   barrier_allowable_s +
                   #barrier_nonallowable_s +
                   OG_value_s + 
                   #barrier_allowable_s*OG_value_s +
                   #CertifierRequestn*OG_value_s +
                   barrier_allowable_s*CertifierRequestn +
                   #barrier_nonallowable_s*OG_value_s +
                   #barrier_nonallowable_s*CertifierRequestn +
                   acreage_log_s + 
                   cropcat_n_s +
                   YearsCertified + 
                   noncommercial_seedsource_s +
                   maincrop +
                   Region, dfs20)
stargazer::stargazer(mm10, mm15, mm20, type = 'html', out = 'tables/test_gazer_multinom_yrly.html')
levels(dfs15$IncreasedOGSeed_f)


dfs2_b1 <- dfs2[dfs2$IncreasedOGSeed_f %in% c('No: About the same % or decreased',
                                                 'Yes: Increased the %'),]
dfs2_b2 <- dfs2
dfs2_b2$IncreasedOGSeed_f <- ifelse(dfs2_b2$IncreasedOGSeed_f %in% 
                                       c('No: About the same % or decreased',
                                         'Yes: Increased the %'), 'Not 100%', 
                                     'At 100%')

bm_1 <- glm(IncreasedOGSeed_f ~ 
                barrier_allowable_s +
              CertifierRequestn + 
                #barrier_nonallowable_s +
                OG_value_s + 
                barrier_allowable_s*OG_value_s +
                #CertifierRequestn*OG_value_s +
                #barrier_allowable_s*CertifierRequestn +
                #barrier_nonallowable_s*OG_value_s +
                #barrier_nonallowable_s*CertifierRequestn +
                acreage_log_s + 
                cropcat_n_s +
                YearsCertified + 
                noncommercial_seedsource_s +
                maincrop +
              survey_year + 
                Region, dfs2_b1, family = 'binomial')

stargazer::stargazer(bm_1, type = 'html', out = 'tables/test_gazer_bm1.html')

library(lme4)
bm_1 <- glmer(IncreasedOGSeed_f ~ 
              barrier_allowable_s +
              CertifierRequestn + 
              #barrier_nonallowable_s +
              OG_value_s + 
              barrier_allowable_s*OG_value_s +
              #CertifierRequestn*OG_value_s +
              #barrier_allowable_s*CertifierRequestn +
              #barrier_nonallowable_s*OG_value_s +
              #barrier_nonallowable_s*CertifierRequestn +
              acreage_log_s + 
              cropcat_n_s +
              YearsCertified + 
              noncommercial_seedsource_s +
              (1|maincrop) +
              (1|survey_year) + 
              (1|Region), dfs2_b1, family = 'binomial')

stargazer::stargazer(bm_1, type = 'html', out = 'tables/test_gazer_bm1.html')

dfs10_b1 <- dfs10[dfs10$IncreasedOGSeed_f %in% c('No: About the same % or decreased',
                                                 'Yes: Increased the %'),]
dfs15_b1 <- dfs15[dfs15$IncreasedOGSeed_f %in% c('No: About the same % or decreased',
                                                 'Yes: Increased the %'),]
dfs20_b1 <- dfs20[dfs20$IncreasedOGSeed_f %in% c('No: About the same % or decreased',
                                                 'Yes: Increased the %'),]
dfs15_b2 <- dfs15
dfs15_b2$IncreasedOGSeed_f <- ifelse(dfs15_b2$IncreasedOGSeed_f %in% 
                                       c('No: About the same % or decreased',
                                         'Yes: Increased the %'), 'Not 100%', 
                                     'At 100%')
dfs15_b2$IncreasedOGSeed_f <- factor(dfs15_b2$IncreasedOGSeed_f, c('Not 100%',
                                                                   'At 100%'))
dfs20_b2 <- dfs20
dfs20_b2$IncreasedOGSeed_f <- ifelse(dfs20_b2$IncreasedOGSeed_f %in% 
                                       c('No: About the same % or decreased',
                                         'Yes: Increased the %'), 'Not 100%', 
                                     'At 100%')
dfs20_b2$IncreasedOGSeed_f <- factor(dfs20_b2$IncreasedOGSeed_f, c('Not 100%',
                                                                   'At 100%'))

bm10_1 <- glm(IncreasedOGSeed_f ~ 
                barrier_allowable_s +
                CertifierRequestn +
                #barrier_nonallowable_s +
                OG_value_s + 
                barrier_allowable_s*OG_value_s +
                #CertifierRequestn*OG_value_s +
                #barrier_allowable_s*CertifierRequestn +
                #barrier_nonallowable_s*OG_value_s +
                #barrier_nonallowable_s*CertifierRequestn +
                acreage_log_s + 
                cropcat_n_s +
                YearsCertified + 
                noncommercial_seedsource_s +
                maincrop +
                Region, dfs10_b1, family = 'binomial')
bm15_1 <- glm(IncreasedOGSeed_f ~ 
                barrier_allowable_s +
                CertifierRequestn +
                #barrier_nonallowable_s +
                OG_value_s + 
                barrier_allowable_s*OG_value_s +
                #CertifierRequestn*OG_value_s +
                #barrier_allowable_s*CertifierRequestn +
                #barrier_nonallowable_s*OG_value_s +
                #barrier_nonallowable_s*CertifierRequestn +
                acreage_log_s + 
                cropcat_n_s +
                YearsCertified + 
                noncommercial_seedsource_s +
                maincrop +
                Region, dfs15_b1, family = 'binomial')

bm15_2 <- glm(IncreasedOGSeed_f ~ 
                barrier_allowable_s +
                CertifierRequestn +
                #barrier_nonallowable_s +
                OG_value_s + 
                barrier_allowable_s*OG_value_s +
                #CertifierRequestn*OG_value_s +
                #barrier_allowable_s*CertifierRequestn +
                #barrier_nonallowable_s*OG_value_s +
                #barrier_nonallowable_s*CertifierRequestn +
                acreage_log_s + 
                cropcat_n_s +
                YearsCertified + 
                noncommercial_seedsource_s +
                maincrop +
                Region, dfs15_b2, family = 'binomial')

bm20_1 <- glm(IncreasedOGSeed_f ~ 
                barrier_allowable_s +
                CertifierRequestn +
                #barrier_nonallowable_s +
                OG_value_s + 
                barrier_allowable_s*OG_value_s +
                #CertifierRequestn*OG_value_s +
                #barrier_allowable_s*CertifierRequestn +
                #barrier_nonallowable_s*OG_value_s +
                #barrier_nonallowable_s*CertifierRequestn +
                acreage_log_s + 
                cropcat_n_s +
                YearsCertified + 
                noncommercial_seedsource_s +
                maincrop +
                Region, dfs20_b1, family = 'binomial')

bm20_2 <- glm(IncreasedOGSeed_f ~ 
                barrier_allowable_s +
                CertifierRequestn +
                #barrier_nonallowable_s +
                OG_value_s + 
                barrier_allowable_s*OG_value_s +
                #CertifierRequestn*OG_value_s +
                #barrier_allowable_s*CertifierRequestn +
                #barrier_nonallowable_s*OG_value_s +
                #barrier_nonallowable_s*CertifierRequestn +
                acreage_log_s + 
                cropcat_n_s +
                YearsCertified + 
                noncommercial_seedsource_s +
                maincrop +
                Region, dfs20_b2, family = 'binomial')
stargazer::stargazer(bm10_1, bm15_1, bm20_1, type = 'html', out = 'tables/test_gazer_binomial1_yrly.html')
stargazer::stargazer(bm15_2, bm20_2, type = 'html', out = 'tables/test_gazer_binomial2_yrly.html')
levels(dfs15$IncreasedOGSeed_f)

levels(dfs10$IncreasedOGSeed_f)
pm10 <- glm(IncreasedOGSeed_f ~ 
              barrier_allowable_s +
              barrier_nonallowable_s +
              OG_value_s + 
              #barrier_allowable_s*OG_value_s +
              #CertifierRequestn*OG_value_s +
              barrier_allowable_s*CertifierRequestn +
              #barrier_nonallowable_s*OG_value_s +
              barrier_nonallowable_s*CertifierRequestn +
              acreage_log_s + 
              cropcat_n_s +
              YearsCertified + 
              noncommercial_seedsource_s +
              maincrop +
              Region, dfs10, family = "binomial")
summary(pm10)
pm15 <- polr(IncreasedOGSeed_f ~ 
               barrier_allowable_s +
               barrier_nonallowable_s +
               OG_value_s + 
               #barrier_allowable_s*OG_value_s +
               #CertifierRequestn*OG_value_s +
               barrier_allowable_s*CertifierRequestn +
               #barrier_nonallowable_s*OG_value_s +
               barrier_nonallowable_s*CertifierRequestn +
               acreage_log_s + 
               cropcat_n_s +
               YearsCertified + 
               noncommercial_seedsource_s +
               maincrop +
               Region, dfs15, Hess = T) 
# We use Hess = T in order to return the Hessian matrix, which allows us to look at the model's summary
summary(pm15)

pm20 <- polr(IncreasedOGSeed_f ~ 
               barrier_allowable_s +
               barrier_nonallowable_s +
               OG_value_s + 
               #barrier_allowable_s*OG_value_s +
               #CertifierRequestn*OG_value_s +
               barrier_allowable_s*CertifierRequestn +
               #barrier_nonallowable_s*OG_value_s +
               barrier_nonallowable_s*CertifierRequestn +
               acreage_log_s + 
               cropcat_n_s +
               YearsCertified + 
               noncommercial_seedsource_s +
               maincrop +
               Region, dfs20, Hess = T) 
# We use Hess = T in order to return the Hessian matrix, which allows us to look at the model's summary
summary(pm20)
stargazer::stargazer(pm10, pm15, pm20, type = 'html', out = 'tables/test_gazer_polr_yrly.html')
