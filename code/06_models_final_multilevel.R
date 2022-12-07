library(dplyr)
library(tidyr)
library(stargazer)
library(lme4)
library(ggplot2)
library(equatiomatic)
library(cowplot)
library(brms)
library(RColorBrewer)
setwd("~/Documents/Davis/R-Projects/organicseed_adoption/")
df <- readRDS("~/Box/organicseed_adoption/df_multilevel.rds") 

# Considering acreage as the dependent variable ----
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

acreage_lm <- fullmodel_fx(data = dfm1, model_type = "lm")
acreage_lmer <- fullmodel_fx(data = dfm1, model_type = "lmer")
acreage_bayes <- fullmodel_fx(data = dfm1, model_type = "bayes")

summary(acreage_lm)
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
                   organicip +
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

acreage_2011_lmer <- yearlymodel_fx(data = df2011, model_type = "lmer")
acreage_2016_lmer <- yearlymodel_fx(data = df2016, model_type = "lmer")
acreage_2021_lmer <- yearlymodel_fx(data = df2021, model_type = "lmer")
acreage_2011_lm <- yearlymodel_fx(data = df2011, model_type = "lm")
acreage_2016_lm <- yearlymodel_fx(data = df2016, model_type = "lm")
acreage_2021_lm <- yearlymodel_fx(data = df2021, model_type = "lm")

summary(acreage_2011_lmer)
summary(acreage_2016_lmer)
summary(acreage_2021_lmer)

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

acreage_west <- regionalmodel_fx(data = dfwest, model_type = "lmer")
acreage_ne <- regionalmodel_fx(data = dfne, model_type = "lmer")
acreage_nc <- regionalmodel_fx(data = dfnc, model_type = "lmer")
acreage_south <- regionalmodel_fx(data = dfsouth, model_type = "lmer")

summary(acreage_west)
summary(acreage_ne)
summary(acreage_nc)
summary(acreage_south)

# This will tell us how far the random effect is away from the overall mean
ranef(acreage)$Region
ranef(acreage_b)$Region
ranef(acreage)$survey_year
ranef(acreage_b)$survey_year
ranef(acreage)$maincrop
ranef(acreage_b)$maincrop

a1 <- acreage_b$coef

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

table_stargazer(acreage_lm, model_levels = 1, 'tables/models/full_lm.html')
table_stargazer(acreage_lmer, model_levels = 2, 'tables/models/full_lmer.html')
table_stargazer(c(acreage_2011, acreage_2016, acreage_2021), model_levels = 2, 'tables/models/yearly_lmer.html')
table_stargazer(c(acreage_2011_lm, acreage_2016_lm, acreage_2021_lm), model_levels = 1, 'tables/models/yearly_lm.html')

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
                                    "Knowledge development: Organically bred varieties",
                                    "Control: Farm size (log acres)",
                                    "Control: Number of crop categories"))
    ) +
      theme_minimal() +
      labs(title = "")
  }
  return(p)
}
yearly_coefplot_fixedeff(acreage_2011_lmer, year = 2011)
yearly_coefplot_fixedeff(acreage_2016_lmer, year = 2011)
yearly_coefplot_fixedeff(acreage_2021_lmer, year = 2021)

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

full_coefplot_randomeff(acreage_bayes)
sjPlot::plot_model(acreage_lmer, type = "re")
sjPlot::plot_model(acreage_bayes, type = "re")


ggdraw() + 
  draw_plot(increase_plot_r, x = 0.4, y = 0, width = .55, height = .53) +
  draw_label("D", x = .58, y = .45) +
  draw_plot(increase_plot_f, x = 0.4, y = .48, width = .55, height = .53) +
  draw_label("C", x = .58, y = .92) +
  draw_plot(acreage_plot_r, x = 0, y = 0, width = .55, height = .53) +
  draw_label("B", x = 0.18, y = .45) +
  draw_plot(acreage_plot_f, x = 0, y = .48, width = .55, height = .53) +
  draw_label("A", x = 0.18, y = .92)
ggsave("tables/figure2.png", height = 6, width = 9, dpi = 800)



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