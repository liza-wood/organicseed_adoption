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
dfm1 <- readRDS("~/Box/organicseed_adoption/dfm1.rds")

greens <- colorRampPalette(c("lightgrey", "#55752f"))(8) # 2002-2008
reds <- colorRampPalette(c("lightgrey", "#90353b"))(8)  # 2009-2013
blues <- colorRampPalette(c("lightgrey", "#1A476f"))(8) # 2014-2018

# For the equation ----
modelforeq <- lmer(eval(parse(text=multilevel_perc)),
                 data = dfm1)
equatiomatic::extract_eq(modelforeq)

# 1. FINAL MODEL: FIXED EFFECTS ----
acreage_bayes <- fullmodel_fx(data = dfm1, 
                                    model_type = "bayes multi-level")

df2011 <- filter(dfm1, survey_year == 2011)
df2016 <- filter(dfm1, survey_year == 2016)
df2021 <- filter(dfm1, survey_year == 2021)

acreage_2011_bayes <- yearlymodel_fx(data = df2011, 
                                     model_type = "bayes multi-level",
                                     year = 2011)
acreage_2016_bayes <- yearlymodel_fx(data = df2016, 
                                     model_type = "bayes multi-level",
                                     year = 2016)
acreage_2021_bayes <- yearlymodel_fx(data = df2021, 
                                     model_type = "bayes multi-level",
                                     year = 2021)

coef_order <- c("Legitimation: Perceived importance of organic seed",
           "Entrepreneurship: Number of new organic seed supply entrants",
           "Guidance: Certifiers request organic seed use",
           "Resources: Organic research funding",
           "Knowledge: Number of registered organically bred varieties",
           "Non-commerical seed sources",
           "Farm size (log acres)",
           "Number of crop categories",
           "Years certified organic",
           "Intercept")

prep_data_fx <- function(model, year){
  sumdf <- summary(model)
  sumdf <- data.frame(sumdf$fixed)
  sumdf$term = rownames(sumdf)
  sumdf$year = year
  colnames(sumdf)[c(3,4)] <- c('LCI', 'UCI')
  sumdf <- sumdf %>% 
    mutate(term = case_when(
      term == "legitimation_user" ~ "Legitimation: Perceived importance of organic seed",
      term == "entrants_supplychain_n_avg_s" ~ "Entrepreneurship: Number of new organic seed supply entrants",
      term == "CertifierRequestn" ~ "Guidance: Certifiers request organic seed use",
      term == "resources_funding_100thous" ~ "Resources: Organic research funding", 
      term == "knowledge_organicip_10" ~ "Knowledge: Number of registered organically bred varieties", 
      term == "noncommercial_seedsource_s" ~ "Non-commerical seed sources", 
      term == "acreage_log" ~ "Farm size (log acres)", 
      term == "cropcat_n" ~ "Number of crop categories", 
      term == "years_certified_s" ~ "Years certified organic", 
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
    filter(term != "Intercept") %>% 
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
    scale_colour_manual(values = rev(c(greens[5], reds[5], blues[5]))) + 
    scale_shape_manual(values = c(21:23)) +
    jtools::theme_nice(base_family = "Times") + 
    jtools::drop_y_gridlines() + 
    theme(axis.title.y = element_blank(),
          axis.title.x = element_text(size = 10),
          text = element_text(size = 12),
          legend.text = element_text(size = 10),
          legend.position = "bottom",
          legend.justification = 0,
          panel.grid.major.x = element_line(linetype = "solid")) + 
    labs(x = "Coefficient Estimate", shape = "", color = "") +
    guides(color=guide_legend(nrow=1,byrow=TRUE),
           shape=guide_legend(nrow=1,byrow=TRUE))
}

manual_coef_plot(model_df)
ggsave(filename = "tables/figure5a.jpeg", width = 8, height = 5.5)

# 2. TABLE FIXED EFFECTS ----
tbl11 <- data.frame(sjPlot::plot_model(acreage_2011_bayes)[[1]])[c(1,2,4,5)] %>% 
mutate(`2010 Effect` = round(estimate, 2),
         `2010 U CI` = round(conf.high, 2),
         `2010 L CI` = round(conf.low, 2)) %>% 
  select(-c(estimate, conf.low, conf.high))
tbl16 <- data.frame(sjPlot::plot_model(acreage_2016_bayes)[[1]])[c(1,2,4,5)] %>% 
  mutate(`2015 Effect` = round(estimate, 2),
         `2015 U CI` = round(conf.high, 2),
         `2015 L CI` = round(conf.low, 2)) %>% 
  select(-c(estimate, conf.low, conf.high))
tbl21 <- data.frame(sjPlot::plot_model(acreage_2021_bayes)[[1]])[c(1,2,4,5)] %>% 
  mutate(`2020 Effect` = round(estimate, 2),
         `2020 U CI` = round(conf.high, 2),
         `2020 L CI` = round(conf.low, 2)) %>% 
  select(-c(estimate, conf.low, conf.high))
tbl <- left_join(tbl21, tbl16) %>% left_join(tbl11) %>% 
  mutate(term = names(coefs)) %>% 
  rename("Variable" = term) 

write.csv(tbl, "tables/tableS7.csv", row.names = F)

# 2. FINAL MODEL: RANDOM EFFECTS ----
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

# Predicted probability
colnames(dfm1)
regional_summary <- read.csv('~/Box/quantifying_seed_innovation/outputs/regional_data.csv')

mock_region = "West"
mock_crop = "Vegetables"

pp_is <- filter(regional_summary, year_range == '2014-2018' &
                  Region == mock_region & maincrop == mock_crop)

colnames(dfm1)
pp_df_cert <- with(dfm1,
                data.frame(acreage_log = median(acreage_log),
                           CertifierRequestn = c(0,1), 
                           cropcat_n = median(cropcat_n), 
                           years_certified_s = median(years_certified_s),
                           legitimation_user = median(legitimation_user),
                           noncommercial_seedsource_s = median(noncommercial_seedsource_s),
                           resources_funding_100thous = pp_is$annual_average,
                           entrants_supplychain_n_avg_s = pp_is$supplychain_n_avg,
                           knowledge_organicip_10 = pp_is$organicip_n,
                           Region = mock_region,
                           survey_year = 2020,
                           maincrop = mock_crop
                           ))

# Using predict function to get predicted probabilities, with standard errors
pp_pred <- cbind(pp_df_cert, predict(acreage_bayes, newdata = pp_df_cert, 
                                     type = "link", se=TRUE, allow_new_levels = T))

fit1 <- pp_pred$`fitted.values.logitlink(P[Y>1|Y>=1])`
fit2 <- pp_pred$`fitted.values.logitlink(P[Y>2|Y>=2])`
se.fit1 <- pp_pred$`se.fit.logitlink(P[Y>1|Y>=1])`
se.fit2 <- pp_pred$`se.fit.logitlink(P[Y>2|Y>=2])`

ppdf_predCIs <- within(pp_pred, {
  Conceptual <- plogis(fit1)
  LL1 <- plogis(fit1 - (1.96 * se.fit1))
  UL1 <- plogis(fit1 + (1.96 * se.fit1))
  Applied <- plogis(fit2)
  LL2 <- plogis(fit2 - (1.96 * se.fit2))
  UL2 <- plogis(fit2 + (1.96 * se.fit2))
})

meltdf <- ppdf_predCIs %>% 
  gather(key = "Level", value = "Probability", 
         Conceptual, Applied, -UL1, -LL1, -UL2, -LL2) %>% 
  gather(key = "ULCI", value = "ULvalue", 
         UL1, UL2, -Level, -Probability, -LL1, -LL2) %>% 
  gather(key = "LLCI", value = "LLvalue", 
         LL1, LL2, -Level, -Probability, -ULCI, -ULvalue) 

meltdf1 <- meltdf %>% 
  filter(Level == "Conceptual" & ULCI == "UL1" & LLCI == "LL1")
meltdf2 <- meltdf %>% 
  filter(Level == "Applied" & ULCI == "UL2" & LLCI == "LL2")
meltdf.clean = rbind(meltdf1, meltdf2)


meltdf.clean$Level <- ordered(meltdf.clean$Level, levels=c("Conceptual","Applied"))
pd = position_dodge(.1)

p3 <- meltdf.clean %>% 
  ggplot(aes(x=priority_conservation, y=Probability, 
             ymin=LLvalue, ymax=ULvalue)) + 
  geom_ribbon(alpha=0.2) +
  geom_line(size=0.2) + 
  facet_wrap(~Level) +
  labs(x = "Stewardship attitude", y = "Predicted probabilities", 
       title = "Nitrogen Management Plan") + 
  scale_fill_brewer() + 
  theme_classic() + 
  theme(text= element_text(size=12, family="Times"), 
        plot.title = element_text(hjust = .5, vjust = 0, size = 12), 
        strip.background  = element_blank())
