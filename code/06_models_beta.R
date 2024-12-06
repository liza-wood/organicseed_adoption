library(dplyr)
library(tidyr)
library(betareg)
library(ggplot2)
library(cowplot)
library(jtools)
library(flextable)
source('code/functions.R')

df.full <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled.rds") %>% 
  select(OGseed_percent_maincrop_prop,
         OG_value_s,
         OG_value_s2,
         #barrier_allowable_rev_s,
         barrier_all_rev_s,
         barrier_all_rev_s2,
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

# MODEL ----
set.seed(19)
m1 <- betareg(OGseed_percent_maincrop_prop ~ 
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
summary(m1)

m2 <- betareg(OGseed_percent_maincrop_prop ~ 
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
summary(m2)

m3 <- betareg(OGseed_percent_maincrop_prop ~ 
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
summary(m3)

m_ix <- betareg(OGseed_percent_maincrop_prop ~ 
                 #barrier_allowable_rev_s +
                 barrier_all_rev_s +
                 OG_value_s + 
                 #barrier_allowable_rev_s*OG_value_s +
                 barrier_all_rev_s*OG_value_s +
                 CertifierRequestn + 
                 acreage_log_s + 
                 cropcat_n_s +
                 noncommercial_seedsource_s +
                 years_certified_s + 
                 maincrop +
                 Region +
                 survey_year,
             data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m_ix)

m1df <- prep_betreg_data(m1)
m1df$model <- 1
m2df <- prep_betreg_data(m2)
m2df$model <- 2
m3df <- prep_betreg_data(m3)
m3df$model <- 3
mdf_ix <- prep_betreg_data(m_ix)
mdf_ix$model <- 4

mdf_combined <- rbind(m1df, m2df, m3df, mdf_ix)
#plot_betareg_models(mdf_combined,4)
#ggsave("figures/figure2.png", width = 8, height = 4.2)
saveRDS(m3, 'tables/append_fullmodel.RDS')
saveRDS(m_ix, 'tables/append_fullmodel_ix.RDS')

dat_list <- list(m1df, m2df, m3df, mdf_ix)
nmodels <- length(dat_list)
mdf_list <- list()
for(i in 1:nmodels){
    mdf <- dat_list[[i]]
    mdf$Estimate <- round(mdf$mean, 2)
    mdf$Estimate <- ifelse(mdf$Pr...z.. < 0.01, paste0(mdf$Estimate, '***'),
                     ifelse(mdf$Pr...z.. < 0.05, paste0(mdf$Estimate, '**'),
                     ifelse(mdf$Pr...z.. < 0.1, paste0(mdf$Estimate, '*'),
                            mdf$Estimate)))
    colnames(mdf)[which(stringr::str_detect(colnames(mdf), 'Estimate'))] <- paste0("Estimate", i)
    mdf_list[[i]] <- mdf
  }

tablea3 <- data.frame('term' = mdf_list[[nmodels]]$term)
tablea3 <- left_join(tablea3, mdf_list[[1]][,c('term', 'Estimate1')])
for(j in 2:length(mdf_list)){
    tablea3 <- left_join(tablea3, mdf_list[[j]][,c('term', paste0('Estimate',j))])
  }


tablea3 <- tablea3 %>% 
  mutate(term = factor(term, rev(coef_order))) %>% 
  arrange(rev(term)) %>% 
  mutate(term = as.character(term)) %>% 
  rbind(c("Observations:", m1$nobs, m2$nobs, m3$nobs, m_ix$nobs)) %>% 
  rbind(c("R2", round(m1$pseudo.r.squared,3), round(m2$pseudo.r.squared,3),
          round(m3$pseudo.r.squared,3), round(m_ix$pseudo.r.squared,3))) %>% 
  mutate(Variables = c(rep('Main effects and interaction', 3), rep('Controls', (nrow(.)-3))))

tablea3 %>% 
  as_grouped_data(groups = c("Variables")) %>% 
  as_flextable() %>% 
  hline(i = (nrow(mdf_ix)+2), part = "body",
        border = fp_border_default(color = "gray40", width = 1.5)) %>% 
  hline(i =c(1,4,5), part = "body",
        border = fp_border_default(color = "gray40", width = 1.5)) %>% 
  width(width =  c(3,rep(0.8, nmodels))) %>%
  height(height = 0) %>% 
  add_footer_lines("Note: *p<0.1; **p<0.05; ***p<0.01") %>% 
  save_as_docx(path = "tables/model_table.docx")
write.csv(tablea3, 'tables/append_fullmodel.csv', row.names = F)

m_summ <- summary(m1)
(exp(0.25)-1)
exp(m_summ$coefficients$mean)-1

m_summ <- summary(m2)
exp(m_summ$coefficients$mean)-1

m_summ <- summary(m3)
exp(m_summ$coefficients$mean)-1

m_summ_ix <- summary(m_ix)
exp(m_summ_ix$coefficients$mean)-1

# So, the idea is to exponentiate the coefficient because it is reported as log odds
# https://stats.stackexchange.com/questions/297659/interpretation-of-betareg-coef
# I have checked in this in appendices, bonus beta check
# As SO says, "It's just not a change in "odds" (= ratio of probabilities) but a change in a ratio of proportions.
## "If the temperature coef is .05 so exp(.05) = 1.05 that would say a one unit increase in temp leads to a 1.05 increase in what?"
## Response: "it represents an increase in the ratio of proportion species observed to proportion of species not observed. The odds is just the ratio between positive and negative classes (p/1-p), so rather than saying "odds" you can just describe the ratio explicitly."
## "the interpretation would be: one unit increase in temp leads to 5% increase in the ratio of proportion species observed to proportion of species not observed. or simply, one unit increase in temp leads to 5% increase in the ratio of proportion species observed"

# Re-running without scaled for the image and ease
m_ix <- betareg(OGseed_percent_maincrop_prop ~ 
                  #barrier_allowable_rev_s +
                  barrier_all_rev_s2 +
                  OG_value_s2 + 
                  #barrier_allowable_rev_s*OG_value_s +
                  barrier_all_rev_s2*OG_value_s2 +
                  CertifierRequestn + 
                  acreage_log_s + 
                  cropcat_n_s +
                  noncommercial_seedsource_s +
                  years_certified_s + 
                  maincrop +
                  Region +
                  survey_year,
                data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m_ix)

summary(df.full_rounded$barrier_all_rev_s2)
mean(df.full_rounded$barrier_all_rev_s2)
sd(df.full_rounded$barrier_all_rev_s2)
table(df.full_rounded$barrier_all_rev_s2)

summary(df.full_rounded$OG_value_s2)
mean(df.full_rounded$OG_value_s2)
sd(df.full_rounded$OG_value_s2)
mylist <- list(barrier_all_rev_s2=c(0:3),
               OG_value_s2=c(2,4))
em_df <- emmeans::emmip(m_ix, OG_value_s2 ~ barrier_all_rev_s2 ,at=mylist, CIs=TRUE)
em_df <- em_df$data

# When barrier is low/ability is high, organic seed value makes a different of 2%
round(em_df$yvar[em_df$barrier_all_rev_s2 == 2 & em_df$OG_value_s2 == 2],2)
round(em_df$yvar[em_df$barrier_all_rev_s2 == 2 & em_df$OG_value_s2 == 4],2)
# When duty is low, barrier levels account for 19% difference
round(em_df$yvar[em_df$barrier_all_rev_s2 == 0 & em_df$OG_value_s2 == 2],2)
round(em_df$yvar[em_df$barrier_all_rev_s2 == 2 & em_df$OG_value_s2 == 2],2)

# Already, main effects suggest that when ability is held constant, duty still matter, suggesting there is an overall element of effort
# The interaction effects suggest more than that -- more dutiful farmers try harder when barriers are high. 

# When barrier is high, organic seed value makes a different of 13% -- the difference between the two more than doubles, suggesting that there is an element
round(em_df$yvar[em_df$barrier_all_rev_s2 == 0 & em_df$OG_value_s2 == 2],2)
round(em_df$yvar[em_df$barrier_all_rev_s2 == 0 & em_df$OG_value_s2 == 4],2)
# When duty is high, barrier levels account for only 8% diference, suggesting the effort to overcome barriers. This may mean switching varieties, trying something new, or looking harder.
round(em_df$yvar[em_df$barrier_all_rev_s2 == 0 & em_df$OG_value_s2 == 4],2)
round(em_df$yvar[em_df$barrier_all_rev_s2 == 2 & em_df$OG_value_s2 == 4],2)

ggplot(em_df, aes(x = factor(barrier_all_rev_s2), y = yvar, 
                  color = factor(OG_value_s2))) +
  geom_point() + 
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.1) +
  geom_line(aes(group = factor(OG_value_s2))) +
  theme_minimal() +
  scale_x_discrete(labels=c("0" = "None", "1" = "Somewhat",
                              "2" = "Moderate", "3" = "High")) +
  scale_color_grey(start = 0, end = 0.7, labels = c("Low",
                                                    "High")) +
  labs(x = "Ability", 
       color = "Duty", y = "Linear prediction") +
  theme(legend.position = "bottom",
        text = element_text(family = "Times"),
        axis.title.x = element_text(vjust = -2))
  
ggsave('figures/figure_ix.png', width = 4.9, height = 3.7)

# How does this compare to the actual data?
df.full %>% 
  mutate(OG_value_s = ifelse(OG_value_s <= 2, "Neutral", "High")) %>% 
  ggplot(aes(x = barrier_allowable_rev_s, 
                    y = OGseed_percent_maincrop_prop, 
                  color = OG_value_s)) +
  geom_point(position = "jitter", alpha = .5) + 
  #geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.1) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  scale_x_discrete(labels=c("0" = "None", "1" = "Somewhat",
                            "2" = "Moderate", "3" = "High")) +
  #scale_color_grey(start = 0, end = 0.7, labels = c("None",
  #                                                  "High")) +
  labs(x = "Capability", 
       color = "Norms", y = "Linear prediction") +
  theme(legend.position = "bottom",
        text = element_text(family = "Times"),
        axis.title.x = element_text(vjust = -2))


# PREDICTED PROBABILITY NOT USED ----
## Vegetables ----
focal_crop = "Vegetables"
dfw11 <- make_pp_df(filter(df.full, Region == "West" &
                              maincrop == focal_crop & 
                              survey_year == 2011))
dfs11 <- make_pp_df(filter(df.full, Region == "South" &
                   maincrop == focal_crop & 
                   survey_year == 2011))
dfnc11 <- make_pp_df(filter(df.full, Region == "North Central" &
                    maincrop == focal_crop & 
                    survey_year == 2011))
dfne11 <- make_pp_df(filter(df.full, Region == "Northeast" &
                    maincrop == focal_crop & 
                    survey_year == 2011))
dfw16 <- make_pp_df(filter(df.full, Region == "West" &
                          maincrop == focal_crop & 
                          survey_year == 2016))
dfs16 <- make_pp_df(filter(df.full, Region == "South" &
                   maincrop == focal_crop & 
                   survey_year == 2016))
dfnc16 <- make_pp_df(filter(df.full, Region == "North Central" &
                   maincrop == focal_crop & 
                   survey_year == 2016))
dfne16 <- make_pp_df(filter(df.full, Region == "Northeast" &
                    maincrop == focal_crop & 
                    survey_year == 2016))
dfw21 <- make_pp_df(filter(df.full, Region == "West" &
                   maincrop == focal_crop & 
                   survey_year == 2021))
dfs21 <- make_pp_df(filter(df.full, Region == "South" &
                   maincrop == focal_crop & 
                   survey_year == 2021))
dfnc21 <- make_pp_df(filter(df.full, Region == "North Central" &
                    maincrop == focal_crop & 
                    survey_year == 2021))
dfne21 <- make_pp_df(filter(df.full, Region == "Northeast" &
                    maincrop == focal_crop & 
                    survey_year == 2021))


df_og <- rbind(dfw11, dfw16, dfw21, dfs11, dfs16, dfs21,
      dfnc11, dfnc16, dfnc21, dfne11, dfne16, dfne21)
df_og$OGseed_percent_maincrop_prop <- rep(NA, nrow(df_og))
df <- select(df_og, colnames(df.full_rounded))

df$mean <- predict(m, newdata = df)
df$model <- ifelse(df$OG_value_s < 0, "Practical", "Principled")
plot_predicted_probs(df, .35, .8)

ggsave('figures/figure3.png', width = 7, height = 4)

# Text
wide11 <- df %>%
  select(survey_year, Region, model, mean) %>% 
  filter(survey_year == 2011) %>% 
  pivot_wider(names_from = model,
              values_from = mean) 

sort(wide11$Principled-wide11$Practical)

wide16 <- df %>%
  select(survey_year, Region, model, mean) %>% 
  filter(survey_year == 2016) %>% 
  pivot_wider(names_from = model,
              values_from = mean) 
sort(wide16$Principled-wide16$Practical)

wide21 <- df %>%
  select(survey_year, Region, model, mean) %>% 
  filter(survey_year == 2021) %>% 
  pivot_wider(names_from = model,
              values_from = mean) 
sort(wide21$Principled-wide21$Practical)

## Field ----
focal_crop = "Field"
dfw11 <- make_pp_df(filter(df.full, Region == "West" &
                             maincrop == focal_crop & 
                             survey_year == 2011))
dfs11 <- make_pp_df(filter(df.full, Region == "South" &
                             maincrop == focal_crop & 
                             survey_year == 2011))
dfnc11 <- make_pp_df(filter(df.full, Region == "North Central" &
                              maincrop == focal_crop & 
                              survey_year == 2011))
dfne11 <- make_pp_df(filter(df.full, Region == "Northeast" &
                              maincrop == focal_crop & 
                              survey_year == 2011))
dfw16 <- make_pp_df(filter(df.full, Region == "West" &
                             maincrop == focal_crop & 
                             survey_year == 2016))
dfs16 <- make_pp_df(filter(df.full, Region == "South" &
                             maincrop == focal_crop & 
                             survey_year == 2016))
dfnc16 <- make_pp_df(filter(df.full, Region == "North Central" &
                              maincrop == focal_crop & 
                              survey_year == 2016))
dfne16 <- make_pp_df(filter(df.full, Region == "Northeast" &
                              maincrop == focal_crop & 
                              survey_year == 2016))
dfw21 <- make_pp_df(filter(df.full, Region == "West" &
                             maincrop == focal_crop & 
                             survey_year == 2021))
dfs21 <- make_pp_df(filter(df.full, Region == "South" &
                             maincrop == focal_crop & 
                             survey_year == 2021))
dfnc21 <- make_pp_df(filter(df.full, Region == "North Central" &
                              maincrop == focal_crop & 
                              survey_year == 2021))
dfne21 <- make_pp_df(filter(df.full, Region == "Northeast" &
                              maincrop == focal_crop & 
                              survey_year == 2021))


df_og <- rbind(dfw11, dfw16, dfw21, dfs11, dfs16, dfs21,
               dfnc11, dfnc16, dfnc21, dfne11, dfne16, dfne21)
df_og$OGseed_percent_maincrop_prop <- rep(NA, nrow(df_og))
df <- select(df_og, colnames(df.full_rounded))

df$mean <- predict(m, newdata = df)
df$model <- ifelse(df$OG_value_s < 0, "Practical", "Principled")
plot_predicted_probs(df, .2, .8)
ggsave('figures/append_field_pp.png', width = 7, height = 4)

# Text
wide11 <- df %>%
  select(survey_year, Region, model, mean) %>% 
  filter(survey_year == 2011) %>% 
  pivot_wider(names_from = model,
              values_from = mean) 

wide11$Practical-wide11$Principled

wide16 <- df %>%
  select(survey_year, Region, model, mean) %>% 
  filter(survey_year == 2016) %>% 
  pivot_wider(names_from = model,
              values_from = mean) 
wide16$Practical-wide16$Principled

wide21 <- df %>%
  select(survey_year, Region, model, mean) %>% 
  filter(survey_year == 2021) %>% 
  pivot_wider(names_from = model,
              values_from = mean) 
wide21$Practical-wide21$Principled

## Forage ----
focal_crop = "Forage"
dfw11 <- make_pp_df(filter(df.full, Region == "West" &
                             maincrop == focal_crop & 
                             survey_year == 2011))
dfs11 <- make_pp_df(filter(df.full, Region == "South" &
                             maincrop == focal_crop & 
                             survey_year == 2011))
dfnc11 <- make_pp_df(filter(df.full, Region == "North Central" &
                              maincrop == focal_crop & 
                              survey_year == 2011))
dfne11 <- make_pp_df(filter(df.full, Region == "Northeast" &
                              maincrop == focal_crop & 
                              survey_year == 2011))
dfw16 <- make_pp_df(filter(df.full, Region == "West" &
                             maincrop == focal_crop & 
                             survey_year == 2016))
dfs16 <- make_pp_df(filter(df.full, Region == "South" &
                             maincrop == focal_crop & 
                             survey_year == 2016))
dfnc16 <- make_pp_df(filter(df.full, Region == "North Central" &
                              maincrop == focal_crop & 
                              survey_year == 2016))
dfne16 <- make_pp_df(filter(df.full, Region == "Northeast" &
                              maincrop == focal_crop & 
                              survey_year == 2016))
dfw21 <- make_pp_df(filter(df.full, Region == "West" &
                             maincrop == focal_crop & 
                             survey_year == 2021))
dfs21 <- make_pp_df(filter(df.full, Region == "South" &
                             maincrop == focal_crop & 
                             survey_year == 2021))
dfnc21 <- make_pp_df(filter(df.full, Region == "North Central" &
                              maincrop == focal_crop & 
                              survey_year == 2021))
dfne21 <- make_pp_df(filter(df.full, Region == "Northeast" &
                              maincrop == focal_crop & 
                              survey_year == 2021))


df_og <- rbind(dfw11, dfw16, dfw21, dfs11, dfs16, dfs21,
               dfnc11, dfnc16, dfnc21, dfne11, dfne16, dfne21)
df_og$OGseed_percent_maincrop_prop <- rep(NA, nrow(df_og))
df <- select(df_og, colnames(df.full_rounded))

df$mean <- predict(m, newdata = df)
df$model <- ifelse(df$OG_value_s < 0, "Practical", "Principled")
df$model <- ifelse(df$Region == "South" & df$survey_year == 2016 &
                     df$OG_value_s > -0.5, "Principled", 
                   ifelse(df$Region == "South" & df$survey_year == 2021 &
                            df$OG_value_s < 0.3, "Practical", df$model))
plot_predicted_probs(df, .2, .8)

ggsave('figures/append_forage_pp.png', width = 7, height = 4)

# Text
wide11 <- df %>%
  select(survey_year, Region, model, mean) %>% 
  filter(survey_year == 2011) %>% 
  pivot_wider(names_from = model,
              values_from = mean) 

wide11$Practical-wide11$Principled

wide16 <- df %>%
  select(survey_year, Region, model, mean) %>% 
  filter(survey_year == 2016) %>% 
  pivot_wider(names_from = model,
              values_from = mean) 
wide16$Practical-wide16$Principled

wide21 <- df %>%
  select(survey_year, Region, model, mean) %>% 
  filter(survey_year == 2021) %>% 
  pivot_wider(names_from = model,
              values_from = mean) 
wide21$Practical-wide21$Principled

