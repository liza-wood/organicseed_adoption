library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
library(cowplot)
library(brms)
library(RColorBrewer)
library(jtools)
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)
setwd("~/Documents/Davis/R-Projects/organicseed_adoption/")
source('code/functions_model.R')
df.full <- readRDS("~/Box/organicseed_adoption/df_full.rds")

str(df.full)
dfm1 <- readRDS("~/Box/organicseed_adoption/dfm1.rds")

greens <- colorRampPalette(c("lightgrey", "#55752f"))(8) # 2002-2008
reds <- colorRampPalette(c("lightgrey", "#90353b"))(8)  # 2009-2013
blues <- colorRampPalette(c("lightgrey", "#1A476f"))(8) # 2014-2018

# FINAL MODEL ----
acreage_bayes <- fullmodel_fx(data = dfm1, 
                                    model_type = "bayes multi-level")

df2011 <- filter(dfm1, survey_year == 2011)
df2016 <- filter(dfm1, survey_year == 2016)
df2021 <- filter(dfm1, survey_year == 2021)

brm(OGseed_percent_maincrop ~ acreage_log, data = df2011)
# For the equation ----
modelforeq <- lmer(eval(parse(text=multilevel_perc_21)),
                   data = df2021)
equatiomatic::extract_eq(modelforeq)

acreage_2011_bayes <- yearlymodel_fx(data = df2011, 
                                     model_type = "bayes multi-level",
                                     year = 2011)
acreage_2016_bayes <- yearlymodel_fx(data = df2016, 
                                     model_type = "bayes multi-level",
                                     year = 2016)
acreage_2021_bayes <- yearlymodel_fx(data = df2021, 
                                     model_type = "bayes multi-level",
                                     year = 2021)

coef_order <- c("Operational resources: Farm size (log acres)",
                #"Operational complexity: Number of crop categories",
                "Operational practice: Non-commerical seed sources",
                "Attitudes: Value organic seed",
           #"Entrepreneurship: Number of new organic seed supply entrants",
           "Entrepreneurship: Entrant ratio (organic growers:supply chain actors)",
           "Regulatory guidance: Certifiers request organic seed use",
           "Resource mobilization: Organic research funding ($M)",
           "Knowledge development: Number of registered organic varieties",
           "Control: Years certified organic",
           "Intercept")

prep_data_fx <- function(model, year){
  sumdf <- summary(model)
  sumdf <- data.frame(sumdf$fixed)
  sumdf$term = rownames(sumdf)
  sumdf$year = year
  colnames(sumdf)[c(3,4)] <- c('LCI', 'UCI')
  sumdf <- sumdf %>% 
    mutate(term = case_when(
      term == "legitimation_user" ~ "Attitudes: Value organic seed",
     # term == "entrants_supplychain_n_avg_s" ~ "Entrepreneurship: Number of new organic seed supply entrants",
     term == "ratio" ~ "Entrepreneurship: Entrant ratio (organic growers:supply chain actors)",
      term == "CertifierRequestn" ~ "Regulatory guidance: Certifiers request organic seed use",
      term == "resources_funding_100thous" ~ "Resource mobilization: Organic research funding ($M)", 
      term == "knowledge_organicip_10" ~ "Knowledge development: Number of registered organic varieties", 
      term == "noncommercial_seedsource_s" ~ "Seed saved (%)", 
      term == "acreage_log" ~ "Farm size (log acres)", 
     # term == "cropcat_n" ~ "Operational complexity: Number of crop categories", 
      term == "years_certified_s" ~ "Control: Years certified organic", 
      T ~ term)) 
  sumdf <- sumdf %>% 
    mutate(term = factor(term, rev(coef_order))) %>% 
    arrange(year, term)
  return(sumdf)
}

model_df <- rbind(prep_data_fx(acreage_2011_bayes, 2010),
                  prep_data_fx(acreage_2016_bayes, 2015),
                  prep_data_fx(acreage_2021_bayes, 2020))

manual_coef_plot <- function(df){
  df %>% 
    filter(!(term %in% c("Intercept", "Control: Years certified organic"))) %>% 
    ggplot(aes(x = Estimate, 
               y = term)) +
    ggstance::geom_pointrangeh(aes(xmin = LCI, 
                                   xmax = UCI,
                                   color = factor(year, c(2020, 2015, 2010)), 
                                   shape = factor(year, c(2020, 2015, 2010))),
                               position = ggstance::position_dodgev(height = .8), 
                               fill = "white", fatten = 2, size = 1) +
    #geom_errorbar(aes(xmin=`conf.high`,xmax=`conf.low`), width=0.1, position = pd) +
    geom_vline(xintercept = 0, linetype = 2, size = 0.25) + 
    scale_colour_manual(values = rev(c(greens[5], reds[5], blues[5])),
                        guide = guide_legend(reverse = TRUE)) + 
    scale_shape_manual(values = c(21:23),
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
}

# FIGURE ----
manual_coef_plot(model_df)
ggsave(filename = "figures/figure3.jpeg", width = 9.5, height = 4)

# FIXED EFFECTS TABLE ----
tbl11 <- data.frame(sjPlot::plot_model(acreage_2011_bayes)[[1]])[c(1,2,4,5)] %>% 
mutate(`2010 Effect` = round(estimate, 2),
       `2010 L CI` = round(conf.low, 2),
         `2010 U CI` = round(conf.high, 2)) %>% 
  select(-c(estimate, conf.low, conf.high))
tbl16 <- data.frame(sjPlot::plot_model(acreage_2016_bayes)[[1]])[c(1,2,4,5)] %>% 
  mutate(`2015 Effect` = round(estimate, 2),
         `2015 L CI` = round(conf.low, 2),
         `2015 U CI` = round(conf.high, 2)) %>% 
  select(-c(estimate, conf.low, conf.high))
tbl21 <- data.frame(sjPlot::plot_model(acreage_2021_bayes)[[1]])[c(1,2,4,5)] %>% 
  mutate(`2020 Effect` = round(estimate, 2),
         `2020 L CI` = round(conf.low, 2),
         `2020 U CI` = round(conf.high, 2)) %>% 
  select(-c(estimate, conf.low, conf.high))
tbl <- left_join(tbl21, tbl16) %>% left_join(tbl11) %>% 
  mutate(term = names(coefs)) %>% 
  rename("Variable" = term) %>% 
  arrange(factor(Variable, coef_order)) %>% 
  select(c(1,8:10,5:7,2:4))

write.csv(tbl, "tables/tableS6.csv", row.names = F)

# RANDOM EFFECTS TABLE ----
# Nothing will automatically make what I want for random effects, so here it goes:

out <- sjPlot::plot_model(acreage_bayes, type = "re")
ranefdf_full <- data.frame(out[[1]]) %>% 
  mutate(term = c("Field", "Forage", "Vegetables", 
                  "North Central", "West", "Northeast", "South",
                  "2011", "2016", "2021")) %>% 
  rename("conf.low.inner" = "conf.low50",
         "conf.high.inner" = "conf.high50") %>% 
  select(facet, term, estimate, `conf.low`, `conf.low.inner`, `conf.high.inner`, `conf.high`) %>% 
  mutate(model = "All years")

ranef_order <- c("Field", "Forage", "Vegetables", 
                 "North Central", "West", "Northeast", "South")
ranef_table_yrly <- function(lmer_model, year_in_model){
  out <- sjPlot::plot_model(lmer_model, type = "re")
  ranefdf <- data.frame(out[[1]]) %>% 
    mutate(term = c("Field", "Forage", "Vegetables", 
                    "North Central", "West", "Northeast", "South")) %>% 
    rename("LCI" = "conf.low50",
           "UCI" = "conf.high50",
           "Estimate" = "estimate") %>% 
    select(facet, term, Estimate, `conf.low`, LCI, UCI, `conf.high`) %>%
    mutate(year = case_when(
      year_in_model == "All years" ~ "All years",
      year_in_model == "2011" ~ "2010",
      year_in_model == "2016" ~ "2015",
      year_in_model == "2021" ~ "2020",
    )) %>% 
    mutate(term = factor(term, rev(ranef_order))) %>% 
    arrange(year, term)
  return(ranefdf)
}
ranefdf11 <- ranef_table_yrly(acreage_2011_bayes, 2011)
ranefdf16 <- ranef_table_yrly(acreage_2016_bayes, 2016)
ranefdf21 <- ranef_table_yrly(acreage_2021_bayes, 2021)

ranefdf <- rbind(#ranefdf_full, 
                 ranefdf11, ranefdf16, ranefdf21)

manual_coef_plot(ranefdf) +
  labs(x = "Intercept", shape = "", color = "")

ggsave("tables/figure5b.jpeg", width = 8, height = 4)

ranefdf11 <- ranefdf11 %>% 
  mutate(`2010 Int` = round(Estimate, 2),
         `2010 L CI` = round(LCI, 2),
         `2010 U CI` = round(UCI, 2)) %>% 
  select(-c(Estimate, conf.low, conf.high, LCI, UCI, facet, year))
ranefdf16 <- ranefdf16 %>% 
  mutate(`2015 Effect` = round(Estimate, 2),
         `2015 L CI` = round(LCI, 2),
         `2015 U CI` = round(UCI, 2)) %>% 
  select(-c(Estimate, conf.low, conf.high, LCI, UCI, facet, year))
ranefdf21 <- ranefdf21 %>% 
  mutate(`2020 Effect` = round(Estimate, 2),
         `2020 L CI` = round(LCI, 2),
         `2020 U CI` = round(UCI, 2)) %>% 
  select(-c(Estimate, conf.low, conf.high, LCI, UCI, facet, year))
ranefdf_tbl <- left_join(ranefdf11, ranefdf16) %>% left_join(ranefdf21) 

write.csv(ranefdf_tbl, "tables/tableS7.csv", row.names = F)
# PREDICTED PROBABILITY ----
mock_region = "West"
mock_crop = "Vegetables"

# Tara
library(tidybayes)

## Two farm models ----

summary(df.full$OGVarietiesSuccessn)

scenario11_model1 <- acreage_2011_bayes %>%
  epred_draws(newdata = tidyr::expand_grid(acreage_log = quantile(dfm1$acreage_log)[2],
                                           CertifierRequestn = 1, 
                                           years_certified_s = median(df2011$years_certified_s),
                                           legitimation_user = 5,
                                           noncommercial_seedsource_s = quantile(dfm1$noncommercial_seedsource_s)[4],
                                        
                                           resources_funding_100thous = median(unique(df2011$resources_funding_100thous)),
                                           #entrants_supplychain_n_avg_s = median(unique(df2011$entrants_supplychain_n_avg_s)),
                                           ratio = median(unique(df2011$ratio)),
                                           #cropcat_n = quantile(unique(dfm1$cropcat_n))[4],
                                           knowledge_organicip_10 = median(unique(df2011$knowledge_organicip_10))),
              re_formula = NA)%>% #Conditional effects for a single typical hypothetical group
  mutate(model = 'Alternative')

scenario11_model2 <- acreage_2011_bayes %>%
  epred_draws(newdata = tidyr::expand_grid(acreage_log = quantile(dfm1$acreage_log)[4],
                                           CertifierRequestn = 1, 
                                           years_certified_s = median(df2011$years_certified_s),
                                           legitimation_user = 4,
                                           noncommercial_seedsource_s = quantile(dfm1$noncommercial_seedsource_s)[2],
                                      
                                           resources_funding_100thous = median(unique(df2011$resources_funding_100thous)),
                                           #entrants_supplychain_n_avg_s = median(unique(df2011$entrants_supplychain_n_avg_s)),
                                           ratio = median(unique(df2011$ratio)),
                                           #cropcat_n = quantile(unique(dfm1$cropcat_n))[2],
                                           #cropcat_n = median(unique(df2011$cropcat_n)),
                                           knowledge_organicip_10 = median(unique(df2011$knowledge_organicip_10))),
              re_formula = NA)%>% #Conditional effects for a single typical hypothetical group
  mutate(model = 'Traditional')

scenario_models11 <- rbind(scenario11_model1, scenario11_model2)

scenario_models11 %>% 
  ggplot(aes(y = .epred)) +
  stat_halfeye(alpha = .8, aes(fill = model)) +
  coord_flip() +
  scale_color_manual(values = c(greens[4], greens[8])) +
  scale_fill_manual(values = c(greens[4], greens[8])) +
  ylim(0, 100)+
  ylab("Predicted acreage (%) planted to organic seed") +
  xlab("Posterior distribution") +
  labs(title ="2011: Adoption based farm system type") +
  theme_minimal_grid() 

scenario16_model1 <- acreage_2016_bayes %>%
  epred_draws(newdata = tidyr::expand_grid(acreage_log = quantile(dfm1$acreage_log)[2],
                                           CertifierRequestn = 1, 
                                           years_certified_s = median(df2016$years_certified_s),
                                           legitimation_user = 5,
                                           noncommercial_seedsource_s = quantile(dfm1$noncommercial_seedsource_s)[4],
                                           #noncommercial_seedsource_s = median(df2016$noncommercial_seedsource_s),
                                           resources_funding_100thous = median(unique(df2016$resources_funding_100thous)),
                                           #entrants_supplychain_n_avg_s = median(unique(df2016$entrants_supplychain_n_avg_s)),
                                           ratio = median(unique(df2016$ratio)),
                                           #cropcat_n = quantile(unique(dfm1$cropcat_n))[4],
                                           #cropcat_n = median(unique(df2016$cropcat_n)),
                                           knowledge_organicip_10 = median(unique(df2016$knowledge_organicip_10))),
              re_formula = NA)%>% #Conditional effects for a single typical hypothetical group
  mutate(model = 'Alternative')

scenario16_model2 <- acreage_2016_bayes %>%
  epred_draws(newdata = tidyr::expand_grid(acreage_log = quantile(dfm1$acreage_log)[4],
                                           CertifierRequestn = 1, 
                                           years_certified_s = median(df2016$years_certified_s),
                                           legitimation_user = 4,
                                           noncommercial_seedsource_s = quantile(dfm1$noncommercial_seedsource_s)[2],
                                           #noncommercial_seedsource_s = median(df2016$noncommercial_seedsource_s),
                                           resources_funding_100thous = median(unique(df2016$resources_funding_100thous)),
                                           #entrants_supplychain_n_avg_s = median(unique(df2016$entrants_supplychain_n_avg_s)),
                                           ratio = median(unique(df2016$ratio)),
                                           #cropcat_n = quantile(unique(dfm1$cropcat_n))[2],
                                           #cropcat_n = median(unique(df2016$cropcat_n)),
                                           knowledge_organicip_10 = median(unique(df2016$knowledge_organicip_10))),
              re_formula = NA)%>% #Conditional effects for a single typical hypothetical group
  mutate(model = 'Traditional')

scenario_models16 <- rbind(scenario16_model1, scenario16_model2)

scenario_models16 %>% 
  ggplot(aes(y = .epred)) +
  stat_halfeye(alpha = .8, aes(fill = model)) +
  coord_flip() +
  scale_color_manual(values = c(reds[4], reds[8])) +
  scale_fill_manual(values = c(reds[4], reds[8])) +
  ylim(0, 100)+
  ylab("Predicted acreage (%) planted to organic seed") +
  xlab("Posterior distribution") +
  labs(title ="2016: Adoption based farm system type") +
  theme_minimal_grid() 

scenario21_model1 <- acreage_2021_bayes %>%
  epred_draws(newdata = tidyr::expand_grid(acreage_log = quantile(dfm1$acreage_log)[2],
                                           CertifierRequestn = 1, 
                                           years_certified_s = median(df2021$years_certified_s),
                                           legitimation_user = 5,
                                           noncommercial_seedsource_s = quantile(dfm1$noncommercial_seedsource_s)[4],
                                           #noncommercial_seedsource_s = median(df2021$noncommercial_seedsource_s),
                                           resources_funding_100thous = median(unique(df2021$resources_funding_100thous)),
                                           #entrants_supplychain_n_avg_s = median(unique(df2021$entrants_supplychain_n_avg_s)),
                                           ratio = median(unique(df2021$ratio)),
                                           #cropcat_n = quantile(unique(dfm1$cropcat_n))[4],
                                           #cropcat_n = median(unique(df2021$cropcat_n)),
                                           knowledge_organicip_10 = median(unique(df2021$knowledge_organicip_10))),
              re_formula = NA)%>% #Conditional effects for a single typical hypothetical group
  mutate(model = 'Alternative')

scenario21_model2 <- acreage_2021_bayes %>%
  epred_draws(newdata = tidyr::expand_grid(acreage_log = quantile(dfm1$acreage_log)[4],
                                           CertifierRequestn = 1, 
                                           years_certified_s = median(df2021$years_certified_s),
                                           legitimation_user = 4,
                                           noncommercial_seedsource_s = quantile(dfm1$noncommercial_seedsource_s)[2],
                                           #noncommercial_seedsource_s = median(df2021$noncommercial_seedsource_s),
                                           resources_funding_100thous = median(unique(df2021$resources_funding_100thous)),
                                           #entrants_supplychain_n_avg_s = median(unique(df2021$entrants_supplychain_n_avg_s)),
                                           ratio = median(unique(df2021$ratio)),
                                           #cropcat_n = quantile(unique(dfm1$cropcat_n))[2],
                                           #cropcat_n = median(unique(df2021$cropcat_n)),
                                           knowledge_organicip_10 = median(unique(df2021$knowledge_organicip_10))),
              re_formula = NA)%>% #Conditional effects for a single typical hypothetical group
  mutate(model = 'Traditional')

scenario_models21 <- rbind(scenario21_model1, scenario21_model2)

scenario_models21 %>% 
  ggplot(aes(y = .epred)) +
  stat_halfeye(alpha = .8, aes(fill = model)) +
  coord_flip() +
  scale_color_manual(values = c(blues[4], blues[8])) +
  scale_fill_manual(values = c(blues[4], blues[8])) +
  ylim(0, 100)+
  ylab("Predicted acreage (%) planted to organic seed") +
  xlab("Posterior distribution") +
  labs(title ="2021: Adoption based farm system type", fill = "", color = "") +
  theme_minimal_grid() +
  geom_text(x = .4, y = 30, label = "n = 189",
            family = "Times", size = 4) +
  geom_text(x = .7, y = 90, label = "n = 514",
            family = "Times", size = 4) +
  theme(text = element_text(family = "Times"))

scenario_models11 <- scenario_models11 %>% 
  mutate(survey_year = 2010)
scenario_models16 <- scenario_models16 %>% 
  mutate(survey_year = 2015)
scenario_models21 <- scenario_models21 %>% 
  mutate(survey_year = 2020)

scenario_models_allyrs <- rbind(scenario_models11, scenario_models16) %>% 
  rbind(scenario_models21)

mean(scenario_models11$.epred[scenario_models11$model == "Traditional"])
mean(scenario_models11$.epred[scenario_models11$model == "Alternative"])
# 59 v 83 = 24
mean(scenario_models11$.epred[scenario_models11$model == "Alternative"]) - mean(scenario_models11$.epred[scenario_models11$model == "Traditional"])

mean(scenario_models16$.epred[scenario_models16$model == "Traditional"])
mean(scenario_models16$.epred[scenario_models16$model == "Alternative"])
# 68 v 93 = 25
mean(scenario_models16$.epred[scenario_models16$model == "Alternative"]) - mean(scenario_models16$.epred[scenario_models16$model == "Traditional"])

mean(scenario_models21$.epred[scenario_models21$model == "Traditional"])
mean(scenario_models21$.epred[scenario_models21$model == "Alternative"])
# 80 v 100 = 20
mean(scenario_models21$.epred[scenario_models21$model == "Alternative"]) - mean(scenario_models21$.epred[scenario_models21$model == "Traditional"])

mako9 <- viridis::mako(n = 9)
scenario_models_allyrs %>% 
  mutate(model = factor(model, c("Traditional", "Alternative"))) %>% 
  ggplot(aes(y = .epred)) +
  stat_halfeye(alpha = .8, aes(fill = model, color = model)) +
  coord_flip() +
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = c(mako9[3], mako9[7])) +
  ylim(0, 100)+
  ylab("Predicted acreage (%) planted to organic seed") +
  xlab("Posterior distribution") +
  labs(title ="", fill = "Farm system", color = "Farm system") +
  theme_minimal_grid() +
  facet_wrap(~survey_year) +
  theme(text = element_text(family = 'Times')) 

ggsave(filename = "figures/figure4.jpeg", width = 9.5, height = 4)

# Describe the data
quantile(df.full$TotalAcreage, na.rm = T)[2]
quantile(df.full$TotalAcreage, na.rm = T)[4]
quantile(df.full$noncommercial_seedsource, na.rm = T)[2]
quantile(df.full$noncommercial_seedsource, na.rm = T)[4]

# 2011:
# Alt model
nrow(df.full[df.full$survey_year == 2011,])
nrow(df.full[df.full$survey_year == 2011 & df.full$TotalAcreage <= 7 &
          df.full$noncommercial_seedsource >= 30,])
quantile(df.full$OGVarietiesSuccessn[df.full$survey_year == 2011 & df.full$TotalAcreage <= 7 &
               df.full$noncommercial_seedsource >= 30], na.rm = T)

# Traditional
nrow(df.full[df.full$survey_year == 2011 & df.full$TotalAcreage >= 225 &
               df.full$noncommercial_seedsource == 0,])
quantile(df.full$OGVarietiesSuccessn[df.full$survey_year == 2011 & df.full$TotalAcreage >= 225 &
               df.full$noncommercial_seedsource == 0], na.rm = T)

round(100*148/(899))
round(100*153/(899))

# 2016:
# Alt model
nrow(df.full[df.full$survey_year == 2016,])
nrow(df.full[df.full$survey_year == 2016 & df.full$TotalAcreage <= 7 &
               df.full$noncommercial_seedsource >= 30,])
quantile(df.full$OGVarietiesSuccessn[df.full$survey_year == 2016 & df.full$TotalAcreage <= 7 &
               df.full$noncommercial_seedsource >= 30], na.rm = T)
# Traditional
nrow(df.full[df.full$survey_year == 2016 & df.full$TotalAcreage >= 225 &
               df.full$noncommercial_seedsource == 0,])
quantile(df.full$OGVarietiesSuccessn[df.full$survey_year == 2016 & df.full$TotalAcreage >= 225 &
               df.full$noncommercial_seedsource == 0], na.rm = T)
round(100*189/(1162))
round(100*191/(1162))

# 2021:
nrow(df.full[df.full$survey_year == 2021,])

# Alt model
nrow(df.full[df.full$survey_year == 2021 & df.full$TotalAcreage <= 7 &
               df.full$noncommercial_seedsource >= 30,])
quantile(df.full$OGVarietiesSuccessn[df.full$survey_year == 2021 & df.full$TotalAcreage <= 7 &
               df.full$noncommercial_seedsource >= 30], na.rm = T)
# Traditional
nrow(df.full[df.full$survey_year == 2021 & df.full$TotalAcreage >= 225 &
               df.full$noncommercial_seedsource == 0,])
quantile(df.full$OGVarietiesSuccessn[df.full$survey_year == 2021 & df.full$TotalAcreage >= 225 &
               df.full$noncommercial_seedsource == 0], na.rm = T)
round(100*76/(760))
round(100*119/(760))

# What about all years?
scenario_model1 <- acreage_bayes %>%
  epred_draws(newdata = tidyr::expand_grid(acreage_log = quantile(dfm1$acreage_log, probs = seq(0,1,.1))[3],
                                           CertifierRequestn = 1, 
                                           years_certified_s = median(dfm1$years_certified_s),
                                           legitimation_user = median(dfm1$legitimation_user),
                                           noncommercial_seedsource_s = quantile(dfm1$noncommercial_seedsource_s, probs = seq(0,1,.1))[9],
                                           #noncommercial_seedsource_s = median(dfm1$noncommercial_seedsource_s),
                                           resources_funding_100thous = median(unique(dfm1$resources_funding_100thous)),
                                           #entrants_supplychain_n_avg_s = median(unique(dfm1$entrants_supplychain_n_avg_s)),
                                           ratio = median(unique(dfm1$ratio)),
                                           #cropcat_n = quantile(unique(dfm1$cropcat_n))[2],
                                           #cropcat_n = median(unique(dfm1$cropcat_n)),
                                           knowledge_organicip_10 = median(unique(dfm1$knowledge_organicip_10))),
              re_formula = NA)%>% #Conditional effects for a single typical hypothetical group
  mutate(model = 'Alternative')

scenario_model2 <- acreage_bayes %>%
  epred_draws(newdata = tidyr::expand_grid(acreage_log = quantile(dfm1$acreage_log, probs = seq(0,1,.1))[9],
                                           CertifierRequestn = 1, 
                                           years_certified_s = median(dfm1$years_certified_s),
                                           legitimation_user = median(dfm1$legitimation_user),
                                           noncommercial_seedsource_s = quantile(dfm1$noncommercial_seedsource_s, probs = seq(0,1,.1))[3],
                                           #noncommercial_seedsource_s = median(dfm1$noncommercial_seedsource_s),
                                           resources_funding_100thous = median(unique(dfm1$resources_funding_100thous)),
                                           #entrants_supplychain_n_avg_s = median(unique(dfm1$entrants_supplychain_n_avg_s)),
                                           ratio = median(unique(dfm1$ratio)),
                                           #cropcat_n = quantile(unique(dfm1$cropcat_n))[2],
                                           #cropcat_n = median(unique(dfm1$cropcat_n)),
                                           knowledge_organicip_10 = median(unique(dfm1$knowledge_organicip_10))),
              re_formula = NA)%>% #Conditional effects for a single typical hypothetical group
  mutate(model = 'Traditional')

scenario_models <- rbind(scenario_model1, scenario_model2)

scenario_models %>% 
  ggplot(aes(y = .epred)) +
  stat_halfeye(alpha = .8, aes(fill = model)) +
  coord_flip() +
  scale_color_manual(values = c("black", "black")) +
  scale_fill_manual(values = c("gray70", "gray40")) +
  ylim(0, 100)+
  ylab("Predicted acreage (%) planted to organic seed") +
  xlab("Posterior distribution") +
  labs(title ="", fill = "Farm system", color = "Farm system") +
  theme_minimal_grid() +
  theme(text = element_text(family = 'Times'))

#ggsave(filename = "figures/figure4.jpeg", width = 5, height = 4)

ggplot(df.full, aes(x = TotalAcreage, y = OGseed_percent_maincrop,color = Region)) +
  geom_jitter() +
  geom_smooth(method = 'lm') +
  scale_x_log10() +
  facet_wrap(~survey_year)

## Regional x crop random effects ----
scenario11_subsys2 <- acreage_2011_bayes %>%
  epred_draws(newdata = tidyr::expand_grid(acreage_log = median(df2011$acreage_log),
                                           CertifierRequestn = 0, 
                                           years_certified_s = median(df2011$years_certified_s),
                                           legitimation_user = median(df2011$legitimation_user),
                                           noncommercial_seedsource_s = median(df2011$noncommercial_seedsource_s),
                                           resources_funding_100thous = median(unique(df2011$resources_funding_100thous)),
                                           #entrants_supplychain_n_avg_s = median(unique(df2011$entrants_supplychain_n_avg_s)),
                                           ratio = median(unique(df2011$ratio)),
                                           cropcat_n = median(unique(df2011$cropcat_n)),
                                           knowledge_organicip_10 = median(unique(df2011$knowledge_organicip_10)),
                                           Region = c("West", "North Central", "Northeast", "South"),
                                           maincrop = c(rep("Vegetables", 4),
                                                        rep("Field", 4),
                                                        rep("Forage", 4)))) %>% #Conditional effects for a single typical hypothetical group
  mutate(survey_year = 2010)

scenario16_subsys2 <- acreage_2016_bayes %>%
  epred_draws(newdata = tidyr::expand_grid(acreage_log = median(df2016$acreage_log),
                                           CertifierRequestn = 0, 
                                           years_certified_s = median(df2016$years_certified_s),
                                           legitimation_user = median(df2016$legitimation_user),
                                           noncommercial_seedsource_s = median(df2016$noncommercial_seedsource_s),
                                           resources_funding_100thous = median(unique(df2016$resources_funding_100thous)),
                                           #entrants_supplychain_n_avg_s = median(unique(df2016$entrants_supplychain_n_avg_s)),
                                           ratio = median(unique(df2016$ratio)),
                                           cropcat_n = median(unique(df2016$cropcat_n)),
                                           knowledge_organicip_10 = median(unique(df2016$knowledge_organicip_10)),
                                           Region = c("West", "North Central", "Northeast", "South"),
                                           maincrop = c(rep("Vegetables", 4),
                                                        rep("Field", 4),
                                                        rep("Forage", 4)))) %>% #Conditional effects for a single typical hypothetical group
  mutate(survey_year = 2015)

scenario21_subsys2 <- acreage_2021_bayes %>%
  epred_draws(newdata = tidyr::expand_grid(acreage_log = median(df2021$acreage_log),
                                           CertifierRequestn = 0, 
                                           years_certified_s = median(df2021$years_certified_s),
                                           legitimation_user = median(df2021$legitimation_user),
                                           noncommercial_seedsource_s = median(df2021$noncommercial_seedsource_s),
                                           resources_funding_100thous = median(unique(df2021$resources_funding_100thous)),
                                           #entrants_supplychain_n_avg_s = median(unique(df2021$entrants_supplychain_n_avg_s)),
                                           ratio = median(unique(df2021$ratio)),
                                           cropcat_n = median(unique(df2021$cropcat_n)),
                                           knowledge_organicip_10 = median(unique(df2021$knowledge_organicip_10)),
                                           Region = c("West", "North Central", "Northeast", "South"),
                                           maincrop = c(rep("Vegetables", 4),
                                                        rep("Field", 4),
                                                        rep("Forage", 4)))) %>% #Conditional effects for a single typical hypothetical group
  mutate(survey_year = 2020)

scenario_subsys2 <- rbind(scenario11_subsys2, scenario16_subsys2) %>% 
  rbind(scenario21_subsys2)

scenario_subsys2 %>% 
  ggplot(aes(x = factor(Region, levels = rev(levels(factor(Region)))), y = .epred)) +
  stat_halfeye(aes(color = factor(survey_year),
                   fill = factor(survey_year)),
               alpha = .8) +
  coord_flip() +
  scale_fill_manual(values = c(greens[5], reds[5], blues[5]))+
  scale_color_manual(values = c(greens[7], reds[7], blues[7]))+
  ylim(0, 100)+
  ylab("Predicted acreage (%) planted to organic seed") +
  xlab("") +
  labs(title ="", fill = "", color = "") +
  theme_minimal_grid() +
  facet_wrap(~maincrop) +
  theme(text = element_text(family = "Times"))

ggsave(filename = "figures/figure4.jpeg", width = 8.5, height = 4.5)

df.full %>% 
  filter(maincrop == 'Vegetables') %>% 
  group_by(Region, survey_year) %>% 
  mutate(n = n()) %>% 
  summarize(min = min(TotalAcreage, na.rm = T),
            Q1 = quantile(TotalAcreage, na.rm = T)[2],
            mean = mean(TotalAcreage, na.rm = T),
            median = median(TotalAcreage, na.rm = T),
            Q3 = quantile(TotalAcreage, na.rm = T)[4],
            max = max(TotalAcreage, na.rm = T),
            n = n) %>% 
  unique()

df.full %>% 
  filter(maincrop == 'Vegetables') %>% 
  group_by(Region, survey_year) %>% 
  mutate(n = n()) %>% 
  summarize(min = min(OGVarietiesSuccessn, na.rm = T),
            Q1 = quantile(OGVarietiesSuccessn, na.rm = T)[2],
            mean = mean(OGVarietiesSuccessn, na.rm = T),
            median = median(OGVarietiesSuccessn, na.rm = T),
            Q3 = quantile(OGVarietiesSuccessn, na.rm = T)[4],
            max = max(OGVarietiesSuccessn, na.rm = T),
            n = n) %>% 
  unique()


## Operational attributes: legitimation and size ----
scenario11_indvl <- acreage_2011_bayes %>%
  epred_draws(newdata = tidyr::expand_grid(acreage_log = c(quantile(df2011$acreage_log)[2],
                                                           quantile(df2011$acreage_log)[2],
                                                           quantile(df2011$acreage_log)[4],
                                                           quantile(df2011$acreage_log)[4]),
                                           CertifierRequestn = 0, 
                                           years_certified_s = median(df2011$years_certified_s),
                                           legitimation_user = c(min(df2011$legitimation_user),
                                                                 min(df2011$legitimation_user),
                                                                 max(df2011$legitimation_user),
                                                                 max(df2011$legitimation_user)),
                                           noncommercial_seedsource_s = median(df2011$noncommercial_seedsource_s),
                                           resources_funding_100thous = median(unique(df2011$resources_funding_100thous)),
                                           entrants_supplychain_n_avg_s = median(unique(df2011$entrants_supplychain_n_avg_s)),
                                           knowledge_organicip_10 = median(unique(df2011$knowledge_organicip_10)),
                                           Region = mock_region,
                                           maincrop = mock_crop), #baseline
              re_formula = NA) #Conditional effects for a single typical hypothetical group

scenario21_indvl <- acreage_2021_bayes %>%
  epred_draws(newdata = tidyr::expand_grid(acreage_log = c(quantile(df2021$acreage_log)[2],
                                                           quantile(df2021$acreage_log)[2],
                                                           quantile(df2021$acreage_log)[4],
                                                           quantile(df2021$acreage_log)[4]),
                                           CertifierRequestn = 0, 
                                           years_certified_s = median(df2021$years_certified_s),
                                           legitimation_user = c(min(df2021$legitimation_user),
                                                                 min(df2021$legitimation_user),
                                                                 max(df2021$legitimation_user),
                                                                 max(df2021$legitimation_user)),
                                           noncommercial_seedsource_s = median(df2021$noncommercial_seedsource_s),
                                           resources_funding_100thous = median(unique(df2021$resources_funding_100thous)),
                                           entrants_supplychain_n_avg_s = median(unique(df2021$entrants_supplychain_n_avg_s)),
                                           knowledge_organicip_10 = median(unique(df2021$knowledge_organicip_10)),
                                           Region = mock_region,
                                           maincrop = mock_crop), #baseline
              re_formula = NA) #Conditional effects for a single typical hypothetical group


scenario11_indvl %>% 
  mutate(`Farm size` = case_when(acreage_log == min(.$acreage_log) ~ "Small",
                                 T ~ "Large"),
  ) %>% 
  mutate(Attitude = case_when(legitimation_user == min(.$legitimation_user) ~ "Low",
                              T ~ "High")) %>% 
  ggplot(aes(x = Attitude, y = .epred)) +
  stat_halfeye(aes(color = `Farm size`,
                   fill = `Farm size`),
               alpha = .8) +
  coord_flip() +
  scale_fill_manual(values = greens[c(4,7)])+
  scale_color_manual(values = c('black', 'black'))+
  ylim(0, 100)+
  ylab("Predicted acreage (%) planted to organic seed") +
  xlab("Values organic seed") +
  labs(title ="2010: Operational profiles") +
  theme_minimal_grid() 

scenario21_indvl %>% 
  mutate(`Farm size` = case_when(acreage_log == min(.$acreage_log) ~ "Small",
                                 T ~ "Large"),
         ) %>% 
  mutate(Attitude = case_when(legitimation_user == min(.$legitimation_user) ~ "Low",
                              T ~ "High")) %>% 
ggplot(aes(x = Attitude, y = .epred)) +
  stat_halfeye(aes(color = `Farm size`,
                   fill = `Farm size`),
               alpha = .8) +
  coord_flip() +
  scale_fill_manual(values = blues[c(4,7)])+
  scale_color_manual(values = c('black', 'black'))+
  ylim(0, 100)+
  ylab("Predicted acreage (%) planted to organic seed") +
  xlab("Values organic seed") +
  labs(title ="2020: Operational profiles") +
  theme_minimal_grid() 

## System attributes: certifier request across years ----
scenario11_sys <- acreage_2011_bayes %>%
  epred_draws(newdata = tidyr::expand_grid(acreage_log = median(df2011$acreage_log),
                                           CertifierRequestn = c(0,1), 
                                           years_certified_s = median(df2011$years_certified_s),
                                           legitimation_user = median(df2011$legitimation_user),
                                           noncommercial_seedsource_s = median(df2011$noncommercial_seedsource_s),
                                           resources_funding_100thous = median(unique(df2011$resources_funding_100thous)),
                                           entrants_supplychain_n_avg_s = median(unique(df2011$entrants_supplychain_n_avg_s)),
                                           knowledge_organicip_10 = median(unique(df2011$knowledge_organicip_10)),
                                           Region = mock_region,
                                           maincrop = mock_crop), #baseline
              re_formula = NA) %>% #Conditional effects for a single typical hypothetical group
  mutate(survey_year = 2010)

scenario16_sys <- acreage_2016_bayes %>%
  epred_draws(newdata = tidyr::expand_grid(acreage_log = median(df2016$acreage_log),
                                           CertifierRequestn = c(0,1), 
                                           years_certified_s = median(df2016$years_certified_s),
                                           legitimation_user = median(df2016$legitimation_user),
                                           noncommercial_seedsource_s = median(df2016$noncommercial_seedsource_s),
                                           resources_funding_100thous = median(unique(df2016$resources_funding_100thous)),
                                           entrants_supplychain_n_avg_s = median(unique(df2016$entrants_supplychain_n_avg_s)),
                                           knowledge_organicip_10 = median(unique(df2016$knowledge_organicip_10)),
                                           Region = mock_region,
                                           maincrop = mock_crop), #baseline
              re_formula = NA) %>% #Conditional effects for a single typical hypothetical group
  mutate(survey_year = 2015)

scenario21_sys <- acreage_2021_bayes %>%
  epred_draws(newdata = tidyr::expand_grid(acreage_log = median(df2021$acreage_log),
                                           CertifierRequestn = c(0,1), 
                                           years_certified_s = median(df2021$years_certified_s),
                                           legitimation_user = median(df2021$legitimation_user),
                                           noncommercial_seedsource_s = median(df2021$noncommercial_seedsource_s),
                                           resources_funding_100thous = median(unique(df2021$resources_funding_100thous)),
                                           entrants_supplychain_n_avg_s = median(unique(df2021$entrants_supplychain_n_avg_s)),
                                           knowledge_organicip_10 = median(unique(df2021$knowledge_organicip_10)),
                                           Region = mock_region,
                                           maincrop = mock_crop), #baseline
              re_formula = NA) %>% #Conditional effects for a single typical hypothetical group
  mutate(survey_year = 2020)

scenario_sys <- rbind(scenario11_sys, scenario16_sys) %>% 
  rbind(scenario21_sys)

scenario_sys %>% 
  mutate(`Certifier request` = case_when(CertifierRequestn == 0 ~ "No",
                                         T ~ "Yes"),
  ) %>% 
  ggplot(aes(x = `Certifier request`, y = .epred)) +
  stat_halfeye(aes(color = factor(survey_year),
                   fill = factor(survey_year)),
               alpha = .8) +
  coord_flip() +
  scale_fill_manual(values = c(greens[5], reds[5], blues[5]))+
  scale_color_manual(values = c(greens[7], reds[7], blues[7]))+
  ylim(0, 100)+
  ylab("Predicted acreage (%) planted to organic seed") +
  xlab("Certifier request") +
  labs(title ="Certifier request") +
  labs(fill = "", color = "") +
  theme_minimal_grid() 


## Regional random effects ----
scenario11_subsys <- acreage_2011_bayes %>%
  epred_draws(newdata = tidyr::expand_grid(acreage_log = median(df2011$acreage_log),
                                           CertifierRequestn = 0, 
                                           years_certified_s = median(df2011$years_certified_s),
                                           legitimation_user = median(df2011$legitimation_user),
                                           noncommercial_seedsource_s = median(df2011$noncommercial_seedsource_s),
                                           resources_funding_100thous = median(unique(df2011$resources_funding_100thous)),
                                           entrants_supplychain_n_avg_s = median(unique(df2011$entrants_supplychain_n_avg_s)),
                                           knowledge_organicip_10 = median(unique(df2011$knowledge_organicip_10)),
                                           Region = c("West", "North Central", "Northeast", "South"),
                                           maincrop = "Vegetables")) %>% #Conditional effects for a single typical hypothetical group
  mutate(survey_year = 2010)

scenario16_subsys <- acreage_2016_bayes %>%
  epred_draws(newdata = tidyr::expand_grid(acreage_log = median(df2016$acreage_log),
                                           CertifierRequestn = 0, 
                                           years_certified_s = median(df2016$years_certified_s),
                                           legitimation_user = median(df2016$legitimation_user),
                                           noncommercial_seedsource_s = median(df2016$noncommercial_seedsource_s),
                                           resources_funding_100thous = median(unique(df2016$resources_funding_100thous)),
                                           entrants_supplychain_n_avg_s = median(unique(df2016$entrants_supplychain_n_avg_s)),
                                           knowledge_organicip_10 = median(unique(df2016$knowledge_organicip_10)),
                                           Region = c("West", "North Central", "Northeast", "South"),
                                           maincrop = "Vegetables")) %>% #Conditional effects for a single typical hypothetical group
  mutate(survey_year = 2015)

scenario21_subsys <- acreage_2021_bayes %>%
  epred_draws(newdata = tidyr::expand_grid(acreage_log = median(df2021$acreage_log),
                                           CertifierRequestn = 0, 
                                           years_certified_s = median(df2021$years_certified_s),
                                           legitimation_user = median(df2021$legitimation_user),
                                           noncommercial_seedsource_s = median(df2021$noncommercial_seedsource_s),
                                           resources_funding_100thous = median(unique(df2021$resources_funding_100thous)),
                                           entrants_supplychain_n_avg_s = median(unique(df2021$entrants_supplychain_n_avg_s)),
                                           knowledge_organicip_10 = median(unique(df2021$knowledge_organicip_10)),
                                           Region = c("West", "North Central", "Northeast", "South"),
                                           maincrop = "Vegetables")) %>% #Conditional effects for a single typical hypothetical group
  mutate(survey_year = 2020)

scenario_subsys <- rbind(scenario11_subsys, scenario16_subsys) %>% 
  rbind(scenario21_subsys)

scenario_subsys %>% 
  ggplot(aes(x = Region, y = .epred)) +
  stat_halfeye(aes(color = factor(survey_year),
                   fill = factor(survey_year)),
               alpha = .8) +
  coord_flip() +
  scale_fill_manual(values = c(greens[5], reds[5], blues[5]))+
  scale_color_manual(values = c(greens[7], reds[7], blues[7]))+
  ylim(0, 100)+
  ylab("Predicted acreage (%) planted to organic seed") +
  xlab("Research funding") +
  labs(title ="Adoption in regional subsystems") +
  theme_minimal_grid() 



  