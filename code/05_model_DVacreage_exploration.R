library(dplyr)
library(tidyr)
library(stargazer)
library(lme4)
library(ggplot2)
library(equatiomatic)
library(cowplot)
library(brms)
library(RColorBrewer)
library(jtools)
setwd("~/Documents/Davis/R-Projects/organicseed_adoption/")
df <- readRDS("~/Box/organicseed_adoption/df_multilevel.rds") 

# Select data ----
# Considering acreage as the dependent variable 
## Use c() when scaling numbers because it "strips" the dimension (i.e. scaling converts them into some kind of matrix and Stack Overflow told me that c would strip this)
df.full <- df %>% 
  filter(Region != "International", maincrop != "Other") %>% 
  select(RespondentID, 
         OGseed_percent_maincrop, #dependent variable
         OGVarietiesSuccessn, # legitimation
         CertifierRequestn, # guidance of the search
         total_awards, # knowledge development/resource mobilization
         ratio, # entrepreneurial activities 
         organicip_n, # knowledge development
         # sales would be market formation?
         maincrop, survey_year, Region, # system level features
         TotalAcreage, cropcat_n # operation controls
  ) %>% 
  mutate(Region = factor(Region, levels = c("North Central", "West", 
                                            "Northeast", "South"))) %>% 
  mutate(acreage_log = c(scale(log(TotalAcreage), scale = F)), 
         cropcat_n = c(scale(cropcat_n, scale = F)),
         funding_million = c(scale(total_awards/1000000, scale = F)),
         legitimation = c(scale(OGVarietiesSuccessn, scale = F)),
         ratio = c(scale(ratio, scale = F)), # entrepreneurial activities 
         organicip = c(scale(organicip_n, scale = F)), # knowledge development
         survey_year = factor(survey_year))

dfm1 <- select(df.full, 
               OGseed_percent_maincrop, 
               maincrop,
               CertifierRequestn,
               acreage_log, 
               cropcat_n,
               funding_million,
               legitimation,
               ratio, 
               organicip,
               Region, survey_year) %>% 
  filter(complete.cases(.))

# Stepwise process ----
## Let's run a stepwise process just to see what the model likes
lmfit_null <- lm(OGseed_percent_maincrop ~ 1, dfm1)
lmfit_full <- lm(OGseed_percent_maincrop ~ 
                   legitimation + 
                   CertifierRequestn + 
                   funding_million + 
                   ratio +
                   organicip +
                   maincrop +
                   survey_year +
                   Region +
                   acreage_log +  
                   cropcat_n, 
                 data = dfm1)

step(object = lmfit_null, 
     scope = list(upper = lmfit_full), direction = "forward", 
     k = log(nrow(dfm1)), trace = TRUE)

# Full models ----
fullmodel_fx <- function(data, model_type = "lm"){
  if(model_type == "bayes"){
    model <- brm(OGseed_percent_maincrop ~ 
                   legitimation + 
                   CertifierRequestn + 
                   funding_million + 
                   ratio +
                   organicip +
                   (1|maincrop) +
                   (1|Region) +
                   (1|survey_year) +
                   acreage_log +  
                   cropcat_n, 
                 data = data)
  } else if(model_type == "lm"){
    model <- lm(OGseed_percent_maincrop ~ 
                  legitimation + 
                  CertifierRequestn + 
                  funding_million + 
                  ratio +
                  organicip +
                  maincrop +
                  Region +
                  survey_year +
                  acreage_log +  
                  cropcat_n, 
                data = data)
  } else if (model_type == "tobit"){
    model <- censReg::censReg(OGseed_percent_maincrop ~ 
                                legitimation + 
                                CertifierRequestn + 
                                funding_million + 
                                ratio +
                                organicip +
                                maincrop +
                                Region +
                                survey_year +
                                acreage_log +  
                                cropcat_n,
                              right = 100,
                              left = 0,
                              data = dfm1)
  } else if (model_type == "lmer"){
    model <- lmer(OGseed_percent_maincrop ~ 
                    legitimation + 
                    CertifierRequestn + 
                    funding_million + 
                    ratio +
                    organicip +
                    (1|maincrop) +
                    (1|Region) +
                    (1|survey_year) +
                    acreage_log +  
                    cropcat_n, 
                  data = data)
  }
  return(model)
}
table(dfm1$OGseed_percent_maincrop)
acreage_lm <- fullmodel_fx(data = dfm1, model_type = "lm")
acreage_tobit <- fullmodel_fx(data = dfm1, model_type = "tobit")
acreage_lmer <- fullmodel_fx(data = dfm1, model_type = "lmer")
acreage_bayes <- fullmodel_fx(data = dfm1, model_type = "bayes")

summary(acreage_lm)
summary(acreage_tobit)
summary(acreage_lmer)
summary(acreage_bayes)

# Yearly models ----
yearlymodel_fx <- function(data, model_type = "lm"){
  if(model_type == "bayes"){
    model <- brm(OGseed_percent_maincrop ~ 
                   legitimation + 
                   CertifierRequestn + 
                   funding_million + 
                   ratio +
                   #organicip +
                   (1|maincrop) +
                   (1|Region) +
                   acreage_log +  
                   cropcat_n, 
                 data = data)
  } else if(model_type == "lm"){
    model <- lm(OGseed_percent_maincrop ~ 
                  legitimation + 
                  CertifierRequestn + 
                  funding_million + 
                  ratio +
                  organicip +
                  maincrop +
                  Region +
                  acreage_log +  
                  cropcat_n, 
                data = data)
  } else if (model_type == "tobit"){
    model <- censReg::censReg(OGseed_percent_maincrop ~ 
                                legitimation + 
                                CertifierRequestn + 
                                funding_million + 
                                ratio +
                                organicip +
                                maincrop +
                                Region +
                                acreage_log +  
                                cropcat_n,
                              right = 100,
                              left = 0,
                              data = dfm1)
  } else if (model_type == "lmer"){
    model <- lmer(OGseed_percent_maincrop ~ 
                    legitimation + 
                    CertifierRequestn + 
                    funding_million + 
                    ratio +
                    organicip +
                    (1|maincrop) +
                    (1|Region) +
                    acreage_log +  
                    cropcat_n, 
                  data = data)
  }
  return(model)
}
df2011 <- filter(dfm1, survey_year == 2011)
df2016 <- filter(dfm1, survey_year == 2016)
df2021 <- filter(dfm1, survey_year == 2021)
table(df2011$Region)

acreage_2011_lmer <- yearlymodel_fx(data = df2011, model_type = "lmer")
acreage_2016_lmer <- yearlymodel_fx(data = df2016, model_type = "lmer")
acreage_2021_lmer <- yearlymodel_fx(data = df2021, model_type = "lmer")
acreage_2011_lm <- yearlymodel_fx(data = df2011, model_type = "lm")
acreage_2016_lm <- yearlymodel_fx(data = df2016, model_type = "lm")
acreage_2021_lm <- yearlymodel_fx(data = df2021, model_type = "lm")
acreage_2011_tobit <- yearlymodel_fx(data = df2011, model_type = "tobit")
acreage_2016_tobit <- yearlymodel_fx(data = df2016, model_type = "tobit")
acreage_2021_tobit <- yearlymodel_fx(data = df2021, model_type = "tobit")
acreage_2011_bayes <- yearlymodel_fx(data = df2011, model_type = "bayes")
acreage_2016_bayes <- yearlymodel_fx(data = df2016, model_type = "bayes")
acreage_2021_bayes <- yearlymodel_fx(data = df2021, model_type = "bayes")

summary(acreage_2011_lmer)
summary(acreage_2016_lmer)
summary(acreage_2021_lmer)
summary(acreage_2011_lm)
summary(acreage_2016_lm)
summary(acreage_2021_lm)
summary(acreage_2011_tobit)
summary(acreage_2016_tobit)
summary(acreage_2021_tobit)
summary(acreage_2011_bayes)
summary(acreage_2016_bayes)
summary(acreage_2021_bayes)

# Regional models ----
regionalmodel_fx <- function(data, model_type = "lm"){
  if(model_type == "bayes"){
    model <- brm(OGseed_percent_maincrop ~ 
                   legitimation + 
                   CertifierRequestn + 
                   funding_million + 
                   ratio +
                   organicip +
                   (1|maincrop) +
                   (1|survey_year) +
                   acreage_log +  
                   cropcat_n, 
                 data = data)
  } else if(model_type == "lm"){
    model <- lm(OGseed_percent_maincrop ~ 
                  legitimation + 
                  CertifierRequestn + 
                  funding_million + 
                  ratio +
                  organicip +
                  maincrop +
                  survey_year +
                  acreage_log +  
                  cropcat_n, 
                data = data)
  } else if (model_type == "lmer"){
    model <- lmer(OGseed_percent_maincrop ~ 
                    legitimation + 
                    CertifierRequestn + 
                    funding_million + 
                    ratio +
                    organicip +
                    (1|maincrop) +
                    (1|survey_year) +
                    acreage_log +  
                    cropcat_n, 
                  data = data)
  }
  return(model)
}
dfwest <- filter(dfm1, Region == "West")
dfne <- filter(dfm1, Region == "Northeast")
dfnc <- filter(dfm1, Region == "North Central")
dfsouth <- filter(dfm1, Region == "South")

acreage_west_lmer <- regionalmodel_fx(data = dfwest, model_type = "lmer")
acreage_ne_lmer <- regionalmodel_fx(data = dfne, model_type = "lmer")
acreage_nc_lmer <- regionalmodel_fx(data = dfnc, model_type = "lmer")
acreage_south_lmer <- regionalmodel_fx(data = dfsouth, model_type = "lmer")
acreage_west_lm <- regionalmodel_fx(data = dfwest, model_type = "lm")
acreage_ne_lm <- regionalmodel_fx(data = dfne, model_type = "lm")
acreage_nc_lm <- regionalmodel_fx(data = dfnc, model_type = "lm")
acreage_south_lm <- regionalmodel_fx(data = dfsouth, model_type = "lm")

summary(acreage_west_lmer)
summary(acreage_ne_lmer)
summary(acreage_nc_lmer)
summary(acreage_south_lmer)
summary(acreage_west_lm)
summary(acreage_ne_lm)
summary(acreage_nc_lm)
summary(acreage_south_lm)

# Crop models ----
cropmodel_fx <- function(data, model_type = "lm"){
  if(model_type == "bayes"){
    model <- brm(OGseed_percent_maincrop ~ 
                   legitimation + 
                   CertifierRequestn + 
                   funding_million + 
                   ratio +
                   organicip +
                   (1|Region) +
                   (1|survey_year) +
                   acreage_log +  
                   cropcat_n, 
                 data = data)
  } else if(model_type == "lm"){
    model <- lm(OGseed_percent_maincrop ~ 
                  legitimation + 
                  CertifierRequestn + 
                  funding_million + 
                  ratio +
                  organicip +
                  Region +
                  survey_year +
                  acreage_log +  
                  cropcat_n, 
                data = data)
  } else if (model_type == "lmer"){
    model <- lmer(OGseed_percent_maincrop ~ 
                    legitimation + 
                    CertifierRequestn + 
                    funding_million + 
                    ratio +
                    organicip +
                    (1|Region) +
                    (1|survey_year) +
                    acreage_log +  
                    cropcat_n, 
                  data = data)
  }
  return(model)
}
table(dfm1$maincrop)
dfveg <- filter(dfm1, maincrop == "Vegetables")
dffield <- filter(dfm1, maincrop == "Field")
dfforage <- filter(dfm1, maincrop == "Forage")

acreage_veg_lmer <- cropmodel_fx(data = dfveg, model_type = "lmer")
acreage_field_lmer <- cropmodel_fx(data = dffield, model_type = "lmer")
acreage_forage_lmer <- cropmodel_fx(data = dfforage, model_type = "lmer")
acreage_veg_lm <- cropmodel_fx(data = dfveg, model_type = "lm")
acreage_field_lm <- cropmodel_fx(data = dffield, model_type = "lm")
acreage_forage_lm <- cropmodel_fx(data = dfforage, model_type = "lm")

summary(acreage_veg_lm)
summary(acreage_field_lm)
summary(acreage_forage_lm)
summary(acreage_veg_lmer)
summary(acreage_field_lmer)
summary(acreage_forage_lmer)

# Can I do year by region? ----
dfveg11 <- filter(dfm1, maincrop == "Vegetables" & survey_year == 2011)
dfveg16 <- filter(dfm1, maincrop == "Vegetables" & survey_year == 2016)
dfveg21 <- filter(dfm1, maincrop == "Vegetables" & survey_year == 2021)
dffield11 <- filter(dfm1, maincrop == "Field" & survey_year == 2011)
dffield16 <- filter(dfm1, maincrop == "Field" & survey_year == 2016)
dffield21 <- filter(dfm1, maincrop == "Field" & survey_year == 2021)
dfforage11 <- filter(dfm1, maincrop == "Forage" & survey_year == 2011)
dfforage16 <- filter(dfm1, maincrop == "Forage" & survey_year == 2016)
dfforage21 <- filter(dfm1, maincrop == "Forage" & survey_year == 2021)


regionyearmodel_fx <- function(data, model_type = "lm"){
  if(model_type == "bayes"){
    model <- brm(OGseed_percent_maincrop ~ 
                   legitimation + 
                   CertifierRequestn + 
                   funding_million + 
                   ratio +
                   #organicip +
                   (1|maincrop) +
                   acreage_log +  
                   cropcat_n, 
                 data = data)
  }
}

vg11_bayes = regionyearmodel_fx(dfveg11, model_type = "bayes")
field11_bayes = regionyearmodel_fx(dffield11, model_type = "bayes")
forage11_bayes = regionyearmodel_fx(dfforage11, model_type = "bayes")
summary(vg11_bayes)
# Selecting models ----
# Overall, nothing is horribly different from one another. When I run the 2016 on its own, I get a singularity problem, so I might prefer the Bayesian approach. 
# I would like to make coefficient plots of all four models: All years, then 2011, 2016, and 2018. 
# It is hard to make a stargazer table from a Bayesian output.
# It is also nearly impossible to make a coefficient plants with multiple models together. jtools::plot_coefs can do it for just the fixed effcts, but so I decided to duplicate it with random effects, by extracting the random effetcs from the sjr::plot_summs. It is messy...


# Stargazer tables ----
table_stargazer <- function(model, model_levels, output_filepath){
  if(model_levels == 2){
    stargazer(model, 
              type = "html", 
              title = "",
              dep.var.labels = "% Acreage",
              covariate.labels = c("Legimitation: Organic seed impacts success", 
                                   "Guidance: Certifiers request organic seed use", 
                                   "Resource mobilization: Organic research funding (million USD)", 
                                   "Entrepreneurship: Ratio of organic supply chain actors to growers",
                                   "Knowledge development: Organically bred varieties",
                                   "Control: Farm size (log acres)",
                                   "Control: Number of crop categories",
                                   "Intercept: Percent of land planted to organic seed (under 'average' conditions)"
              ),
              model.numbers = FALSE,
              no.space = TRUE, 
              header = FALSE, 
              out = output_filepath)
  } else if(model_levels == 1){
    stargazer(model, 
              type = "html", 
              title = "",
              dep.var.labels = "% Acreage",
              covariate.labels = c("Legimitation: Organic seed impacts success", 
                                   "Guidance: Certifiers request organic seed use", 
                                   "Resource mobilization: Organic research funding (million USD)", 
                                   "Entrepreneurship: Ratio of organic supply chain actors to growers",
                                   "Knowledge development: Organically bred varieties",
                                   "Crop type: Forage (vs. Field)",
                                   "Crop type: Vegetable (vs. Field)",
                                   "Region: West (vs. North Central)",
                                   "Region: Northeast (vs. North Central)",
                                   "Region: South (vs. North Central)",
                                   "Year: 2016 (vs. 2011)",
                                   "Year: 2021 (vs. 2011)",
                                   "Control: Farm size (log acres)",
                                   "Control: Number of crop categories",
                                   "Intercept: Percent of land planted to organic seed (under 'average' conditions)"
              ),
              model.numbers = FALSE,
              no.space = TRUE, 
              header = FALSE, 
              out = output_filepath)
  } 
}

# Full tables
table_stargazer(acreage_lm, model_levels = 1, 'tables/models/full_lm.html')
table_stargazer(acreage_lmer, model_levels = 2, 'tables/models/full_lmer.html')

# Yearly tables
stargazer(acreage_2011_lm,  acreage_2016_lm, acreage_2021_lm, 
          type = "html", out = 'tables/models/yearly_lm.html')
#table_stargazer(c(acreage_2011_lm,  acreage_2016_lm, acreage_2021_lm), model_levels = 1, 'tables/models/yearly_lm.html')
table_stargazer(acreage_2011_tobit, model_levels = 1, 'tables/models/yearly_tobit.html')
table_stargazer(c(acreage_2011_lmer, acreage_2016_lmer, acreage_2021_lmer), model_levels = 2, 'tables/models/yearly_lmer.html')
stargazer(acreage_west_lm,  acreage_nc_lm, acreage_ne_lm, acreage_south_lm, 
          type = "html", out = 'tables/models/regional_lm.html')
table_stargazer(c(acreage_west_lmer, acreage_nc_lmer, acreage_ne_lmer, acreage_south_lmer), model_levels = 2, 'tables/models/regional_lmer.html')
stargazer(acreage_veg_lm,  acreage_field_lm, acreage_forage_lm, 
          type = "html", out = 'tables/models/crop_lm.html')
table_stargazer(c(acreage_veg_lmer, acreage_field_lmer, acreage_forage_lmer), model_levels = 2, 'tables/models/crop_lmer.html')


# Coefficient plots ----
full_coefplot_fixedeff <- function(model, model_levels){
  if(model_levels == 2){
    p <- sjPlot::plot_model(model, 
                            show.values = T,
                            type = "est",
                            value.size = 2,
                            bpe.style = "dot",
                            bpe.color = "black",
                            vline.color = "black",
                            line.size = .5,
                            #group.terms = c(1,1,1,1,1,1,1),
                            colors = c("black"),
                            axis.labels = 
                              rev(c("Legitimation: Organic seed impacts success", 
                                    "Guidance: Certifiers request organic seed use", 
                                    "Resource mobilization: Organic research funding (million USD)", 
                                    "Entrepreneurship: Ratio of organic supply chain actors to growers",
                                    "Knowledge development: Organically bred varieties",
                                    "Control: Farm size (log acres)",
                                    "Control: Number of crop categories"))
    ) +
      theme_minimal() +
      labs(title = "")
    return(p)
  } else if (model_levels == 1){
    p <- sjPlot::plot_model(model, 
                            show.values = T,
                            type = "est",
                            value.size = 2,
                            bpe.style = "dot",
                            bpe.color = "black",
                            vline.color = "black",
                            line.size = .5,
                            group.terms = c(rep(1,5),rep(2,7), 1,1),
                            colors = c("black", "grey"),
                            axis.labels = 
                              rev(c("Legimitation: Organic seed impacts success", 
                                    "Guidance: Certifiers request organic seed use", 
                                    "Resource mobilization: Organic research funding (million USD)", 
                                    "Entrepreneurship: Ratio of organic supply chain actors to growers",
                                    "Knowledge development: Organically bred varieties",
                                    "Crop type: Forage (vs. Field)",
                                    "Crop type: Vegetable (vs. Field)",
                                    "Region: West (vs. North Central)",
                                    "Region: Northeast (vs. North Central)",
                                    "Region: South (vs. North Central)",
                                    "Year: 2016 (vs. 2011)",
                                    "Year: 2021 (vs. 2011)",
                                    "Control: Farm size (log acres)",
                                    "Control: Number of crop categories"))
    ) 
    theme_minimal() +
      labs(title = "")
  }
  return(p)
}

full_coefplot_fixedeff(acreage_lm, model_levels = 1)
full_coefplot_fixedeff(acreage_lmer, model_levels = 2)
full_coefplot_fixedeff(acreage_bayes, model_levels = 2)

yearly_coefplot_fixedeff <- function(model, year){
  if(year %in% c(2011, 2016)){
    p <- sjPlot::plot_model(model, 
                            show.values = T,
                            type = "est",
                            value.size = 2,
                            bpe.style = "dot",
                            bpe.color = "black",
                            vline.color = "black",
                            line.size = .5,
                            #group.terms = c(1,1,1,1,1,1,1),
                            colors = c("black"),
                            axis.labels = 
                              rev(c("Legitimation: Organic seed impacts success", 
                                    "Guidance: Certifiers request organic seed use", 
                                    "Resource mobilization: Organic research funding (million USD)", 
                                    "Entrepreneurship: Ratio of organic supply chain actors to growers",
                                    "Control: Farm size (log acres)",
                                    "Control: Number of crop categories"))
    ) +
      theme_minimal() +
      labs(title = "")
  } else if(year == 2021){
    p <- sjPlot::plot_model(model, 
                            show.values = T,
                            type = "est",
                            value.size = 2,
                            bpe.style = "dot",
                            bpe.color = "black",
                            vline.color = "black",
                            line.size = .5,
                            #group.terms = c(1,1,1,1,1,1,1),
                            colors = c("black"),
                            axis.labels = 
                              rev(c("Legitimation: Organic seed impacts success", 
                                    "Guidance: Certifiers request organic seed use", 
                                    "Resource mobilization: Organic research funding (million USD)", 
                                    "Entrepreneurship: Ratio of organic supply chain actors to growers",
                                    #"Knowledge development: Organically bred varieties",
                                    "Control: Farm size (log acres)",
                                    "Control: Number of crop categories"))
    ) +
      theme_minimal() +
      labs(title = "")
  }
  return(p)
}
jtools::plot_coefs(acreage_2011_lm, acreage_2016_lm, acreage_2021_lm)
jtools::plot_coefs(acreage_west_lmer, acreage_nc_lmer, acreage_ne_lmer, acreage_south_lmer)
jtools::plot_coefs(acreage_veg_lmer, acreage_field_lmer, acreage_forage_lmer)



yearly_coefplot_fixedeff(acreage_2011_lmer, year = 2011)
yearly_coefplot_fixedeff(acreage_2016_lmer, year = 2016)
yearly_coefplot_fixedeff(acreage_2021_lmer, year = 2021)
yearly_coefplot_fixedeff(acreage_2011_bayes, year = 2011)
yearly_coefplot_fixedeff(acreage_2016_bayes, year = 2016)
yearly_coefplot_fixedeff(acreage_2021_bayes, year = 2021)


# This is the one I want for fixed effects:
jtools::plot_coefs(acreage_lmer, acreage_2011_lmer, acreage_2016_lmer, acreage_2021_lmer,
                   legend.title = "", 
                   model.names = c("All years", "2004-2010", "2011-2015", "2016-2020"), 
                   colors = c("black", greens[3:5]),  
                   coefs = c("Legitimation:\nOrganic seed impacts success" = "legitimation", 
                             "Guidance:\nCertifiers request organic seed use" = "CertifierRequestn", 
                             "Resource mobilization:\nOrganic research funding (million USD)" = "funding_million", 
                             "Entrepreneurship:\nRatio of organic growers to supply chain actors" = "ratio",
                             "Knowledge development:\nOrganically bred varieties" = "organicip",
                             "Control: Farm size (log acres)" = "acreage_log",
                             "Control: Number of crop categories" = "cropcat_n"))
# Nothing will automatically make what I want for random effects, so here it goes:
out <- sjPlot::plot_model(acreage_2011_lmer, type = "re")
ranefdf_full <- rbind(data.frame(out[[1]]$data), 
                      data.frame(out[[2]]$data),
                      data.frame(out[[3]]$data)) %>% 
  rename("conf.low.inner" = xmin,
         "conf.high.inner" = xmax,
         "est" = estimate) %>% 
  select(facet, term, est, `conf.low`, `conf.low.inner`, `conf.high.inner`, `conf.high`) %>% 
  mutate(model = "All years")
ranef_table_yrly <- function(lmer_model, year_in_model){
  out <- sjPlot::plot_model(lmer_model, type = "re")
  ranefdf <- rbind(data.frame(out[[1]]$data), 
                   data.frame(out[[2]]$data)) %>% 
    rename("conf.low.inner" = xmin,
           "conf.high.inner" = xmax,
           "est" = estimate) %>% 
    select(facet, term, est, `conf.low`, `conf.low.inner`, `conf.high.inner`, `conf.high`) %>% 
    mutate(model = year_in_model)
  return(ranefdf)
}
ranefdf11 <- ranef_table_yrly(acreage_2011_lmer, 2011)
ranefdf16 <- ranef_table_yrly(acreage_2016_lmer, 2016)
ranefdf21 <- ranef_table_yrly(acreage_2021_lmer, 2021)

ranefdf <- rbind(ranefdf_full,ranefdf11, ranefdf16, ranefdf21)
pd <- position_dodge(width=0.75)
ggplot(ranefdf, aes(x = est, y = term, color = model, shape = model), 
       shapes = 1:4, fill = "white") +
  geom_point(size=3,  position = pd, fill = "white") +
  geom_errorbar(aes(xmin=`conf.high`,xmax=`conf.low`), width=0.1, position = pd) +
  geom_vline(xintercept = 0, linetype = 2, size = 0.25) + 
  scale_colour_manual(values = c("black", greens[3:5])) + 
  theme_nice(legend.pos = "right") + 
  drop_y_gridlines() + 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 10), 
        panel.grid.major.x = element_line(linetype = "solid")) + 
  labs(x = "Estimate", shape = "", color = "")


out[3]
ranefsd <- data.frame(objs$varcor)
obj <- ranef(acreage_lmer)$Region

obj <- obj %>% 
  rename("int" = `(Intercept)`) %>% 
  mutate(variable = rownames(.),
         sd = )

jtools::plot_summs(obj)
?jtools::plot_summs

full_coefplot_randomeff <- function(model){
  p <- sjPlot::plot_model(model, 
                          type = "re",
                          value.size = 2,
                          bpe.style = "dot",
                          bpe.color = "black",
                          vline.color = "black",
                          line.size = .5,
                          group.terms = c(1,1,1,2,2,2,2,3,3,3),
                          colors = c("black", "darkgreen", "darkblue"),
                          axis.labels = 
                            rev(c("Crop type: Field",
                                  "Crop type: Forage",
                                  "Crop type: Vegetable",
                                  "Region: North Central",
                                  "Region: West",
                                  "Region: Northeast",
                                  "Region: South",
                                  "Year: 2011",
                                  "Year: 2016",
                                  "Year: 2021"))
  ) +
    theme_minimal() +
    labs(title = "")
  return(p)
}
yearly_coefplot_randomeff <- function(model){
  p <- sjPlot::plot_model(model, 
                          type = "re",
                          value.size = 2,
                          bpe.style = "dot",
                          bpe.color = "black",
                          vline.color = "black",
                          line.size = .5,
                          group.terms = c(1,1,1,2,2,2,2),
                          colors = c("black", "darkgreen"),
                          axis.labels = 
                            rev(c("Crop type: Field",
                                  "Crop type: Forage",
                                  "Crop type: Vegetable",
                                  "Region: North Central",
                                  "Region: West",
                                  "Region: Northeast",
                                  "Region: South"))
  ) +
    theme_minimal() +
    labs(title = "")
  return(p)
}

full_coefplot_randomeff(acreage_bayes)
out <- sjPlot::plot_model(acreage_lmer, type = "re")
out[3]
yearly_coefplot_randomeff(acreage_2011_bayes)
yearly_coefplot_randomeff(acreage_2016_bayes)
yearly_coefplot_randomeff(acreage_2021_bayes)


sjPlot::plot_model(acreage_bayes, type = "re")


ggdraw() + 
  draw_plot(increase_plot_r, x = 0.4, y = 0, width = .55, height = .53) +
  draw_label("D", x = .58, y = .45) +
  draw_plot(increase_plot_f, x = 0.4, y = .48, width = .55, height = .53) +
  draw_label("C", x = .58, y = .92) +
  draw_plot(acreage_plot_r, x = 0, y = 0, width = .55, height = .53) 
ggsave("tables/figure2.png", height = 6, width = 9, dpi = 800)

# This will tell us how far the random effect is away from the overall mean
ranef(acreage)$Region
ranef(acreage_b)$Region
ranef(acreage)$survey_year
ranef(acreage_b)$survey_year
ranef(acreage)$maincrop
ranef(acreage_b)$maincrop

a1 <- acreage_b$coef

emmeans::emmeans(increase, specs = ~ CertifierRequestn)

# For a field crop grower in the North Central region in 2016, what is the effect of a certifier asking to increase
pp_df1 <- data.frame(maincrop = "Field",
                     TotalAcreage_ls = 
                       median(df.full$TotalAcreage_ls[df.full$Region ==
                                                        "North Central" &
                                                        df.full$survey_year ==
                                                        2016 &
                                                        df.full$maincrop == "Field"], 
                              na.rm = T), 
                     OGSeedOGIntegrityn_s = 
                       median(df.full$OGSeedOGIntegrityn_s, na.rm = T), 
                     CertifierRequestn = c(0,1),
                     funding_avg_annual_amt_ls = 
                       unique(df.full$funding_avg_annual_amt_ls[df.full$Region ==
                                                                  "North Central" &
                                                                  df.full$survey_year ==
                                                                  2016 &
                                                                  df.full$maincrop == 
                                                                  "Field"]),
                     ratio_s = unique(df.full$ratio_s[df.full$Region ==
                                                        "North Central" &
                                                        df.full$survey_year ==
                                                        2016 &
                                                        df.full$maincrop == 
                                                        "Field"]),
                     median_rev_s = unique(df.full$median_rev_s[df.full$Region ==
                                                                  "North Central" &
                                                                  df.full$survey_year ==
                                                                  2016 &
                                                                  df.full$maincrop == 
                                                                  "Field"]),
                     Region = "North Central",
                     survey_year_f = 2016
)

# For a vegetable grower in the Western region in 2016, what is the effect of a certifier asking to increase
pp_df2 <- data.frame(maincrop = "Vegetables",
                     TotalAcreage_ls = 
                       median(df.full$TotalAcreage_ls[df.full$Region ==
                                                        "West" &
                                                        df.full$survey_year ==
                                                        2016 &
                                                        df.full$maincrop == "Vegetables"], 
                              na.rm = T), 
                     OGSeedOGIntegrityn_s = 
                       median(df.full$OGSeedOGIntegrityn_s, na.rm = T), 
                     CertifierRequestn = c(0,1),
                     funding_avg_annual_amt_ls = 
                       unique(df.full$funding_avg_annual_amt_ls[df.full$Region ==
                                                                  "West" &
                                                                  df.full$survey_year ==
                                                                  2016 &
                                                                  df.full$maincrop == 
                                                                  "Vegetables"]),
                     ratio_s = unique(df.full$ratio_s[df.full$Region ==
                                                        "West" &
                                                        df.full$survey_year ==
                                                        2016 &
                                                        df.full$maincrop == 
                                                        "Vegetables"]),
                     median_rev_s = unique(df.full$median_rev_s[df.full$Region ==
                                                                  "West" &
                                                                  df.full$survey_year ==
                                                                  2016 &
                                                                  df.full$maincrop == 
                                                                  "Vegetables"]),
                     Region = "West",
                     survey_year_f = 2016
)


# Using predict function to get predicted probabilities, with standard errors

# I am asking it to predict on the scale of the response variable
# https://stats4nr.com/linear-mixed-models.html
pred_acreage_cert_ncentral16 <- cbind(pp_df1, predict(acreage_b, 
                                                      newdata = pp_df1, 
                                                      type = "response",
                                                      se = T,
                                                      re.form = NULL)) #All ranef
#re.form = NA)) #No ranef
pred_acreage_cert_west16 <- cbind(pp_df2, predict(acreage, 
                                                  newdata = pp_df2, 
                                                  type = "response",
                                                  se = T,
                                                  re.form = NULL)) #All ranef
#re.form = NA)) #No ranef

pred_increase_cert_ncentral16 <- cbind(pp_df1, predict(increase, 
                                                       newdata = pp_df1,
                                                       se = T, 
                                                       type = "link",
                                                       re.form = NULL))
pred_increase_cert_west16 <- cbind(pp_df2, predict(increase, 
                                                   newdata = pp_df2, 
                                                   se = T, 
                                                   type = "response",
                                                   re.form = NULL))

plot_change <- function(profile, ylab){
  profile$CIL <- profile$fit - (1.96 * profile$se.fit)
  profile$CIU <- profile$fit + (1.96 * profile$se.fit)
  
  profile %>% 
    mutate(request_yn = factor(CertifierRequestn)) %>% 
    mutate(request_yn = case_when(
      request_yn == 1 ~ "Yes",
      request_yn == 0 ~ "No",
      T ~ NA_character_
    )) %>% 
    ggplot(aes(x = request_yn, y = fit, 
               ymin = CIL, ymax = CIU)) +
    geom_point() + 
    geom_errorbar(width=.2,
                  position=position_dodge(.9)) +
    labs(x = "Certifier Request", y = ylab, 
         title = "") +
    scale_fill_brewer() + 
    theme_classic() + 
    theme(text= element_text(size=12, family="Times"), 
          plot.title = element_text(hjust = .5, vjust = 0, size = 12), 
          strip.background  = element_blank()) 
}

plot_change(pred_acreage_cert_ncentral16, "Predicted % organic acreage")
plot_change(pred_acreage_cert_west16, "Predicted % organic acreage")
pnc <- plot_change(pred_increase_cert_ncentral16, "") +
  ylim(.2,.8) ; pnc
pw <- plot_change(pred_increase_cert_west16, "Likelihood of increasing organic seed use") +
  ylim(.2,.8) ; pw

ggdraw() + 
  draw_plot(pnc, x = 0.5, y = 0, width = .5, height = 1) +
  draw_label("B", x = 0.6, y = .95) +
  draw_plot(pw, x = 0, y = 0, width = .5, height = 1) +
  draw_label("A", x = 0.1, y = .95)
ggsave("tables/figure3.png", height = 3, width = 6, dpi = 800)

acreage_barriers <- lmer(OGseed_percent_maincrop ~ 
                           OGSeedOGIntegrityn_s + 
                           barrier_availability_s +
                           barrier_price_s +
                           barrier_buyerReq_s +
                           TotalAcreage_ls + 
                           CertifierRequestn + 
                           funding_avg_annual_amt_ls + 
                           ratio_s +
                           median_rev_s +
                           (1|maincrop) +
                           (1|survey_year_f) +
                           (1|Region), data = df2)

acreage_barriers_b <- brm(OGseed_percent_maincrop ~ 
                            OGSeedOGIntegrityn_s + 
                            barrier_availability_s +
                            barrier_price_s +
                            barrier_buyerReq_s +
                            TotalAcreage_ls + 
                            CertifierRequestn + 
                            funding_avg_annual_amt_ls + 
                            ratio_s +
                            median_rev_s +
                            (1|maincrop) +
                            (1|survey_year_f) +
                            (1|Region), data = df2)

increase_barriers <- glmer(IncreasedOGSeed_binom ~ 
                             OGSeedOGIntegrityn_s + 
                             barrier_availability_s +
                             barrier_price_s +
                             barrier_buyerReq_s +
                             TotalAcreage_ls + 
                             CertifierRequestn + 
                             funding_avg_annual_amt_ls + 
                             ratio_s +
                             median_rev_s +
                             (1|maincrop) +
                             (1|survey_year_f) +
                             (1|Region),
                           family = "binomial",
                           data = df4)

increase_barriers_b <- brm(IncreasedOGSeed_binom ~ 
                             OGSeedOGIntegrityn_s + 
                             barrier_availability_s +
                             barrier_price_s +
                             barrier_buyerReq_s +
                             TotalAcreage_ls + 
                             CertifierRequestn + 
                             funding_avg_annual_amt_ls + 
                             ratio_s +
                             median_rev_s +
                             (1|maincrop) +
                             (1|survey_year_f) +
                             (1|Region),
                           family = bernoulli(link = "logit"),
                           data = df4)

stargazer(acreage, #acreage_barriers, 
          increase, #increase_barriers, 
          type = "html", 
          title = "Linear model comparison",
          dep.var.labels = c("% Acreage", "Increased OG seed use"),
          covariate.labels = c("Values organic seed", 
                               "Barrier: Variety availability", 
                               "Barrier: Price",
                               "Barrier: Buyer restriction",
                               #"Crop type: Forage (vs. Field)", 
                               #"Crop type: Vegetable (vs. Field)", 
                               "Total Acres (log)", 
                               "Certifier request", 
                               "Research funding (annual avg.)", 
                               "Ratio of supply chain actors to growers",
                               "Median revenue of seed supply chain"),
          #"Regional funding (annual average)", 
          #"Region: West (vs. North Central)",
          #"Region: Northeast (vs. North Central)",
          #"Region: South (vs. North Central)",
          #"Year: 2016",
          #"Year: 2021"),
          model.numbers = FALSE,
          no.space = TRUE, 
          header = FALSE, 
          out = c("tables/models/lm_barriers.html"))