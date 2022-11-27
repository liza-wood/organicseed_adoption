library(dplyr)
library(tidyr)
library(stargazer)
library(lme4)
library(ggplot2)
library(equatiomatic)
library(cowplot)
library(brms)
setwd("~/Documents/Davis/R-Projects/organicseed/")
df <- readRDS("~/Box/organicseed/df_onelevel.rds") 

df$IncreasedOGSeed_f <- factor(df$IncreasedOGSeed , 
                               levels = c("No", "Yes", "Already at 100%"))
df$IncreasedOGSeed_binom <- ifelse(df$IncreasedOGSeed_f == "No", 0,
                                   ifelse(df$IncreasedOGSeed_f == "Yes", 1, NA))
# Considering acreage as the dependent variable ----
## Use c() when scaling numbers because it "strips" the dimension (i.e. scaling converts them into some kind of matrix and Stack Overflow told me that c would strip this)
df.full <- df %>% 
  filter(Region != "International", maincrop != "Other") %>% 
  select(RespondentID, OGseed_percent_maincrop, IncreasedOGSeed_binom, maincrop, 
         TotalAcreage, OGSeedOGIntegrityn,CertifierRequestn,
         #funding_avg_annual_amt, 
         funding_amt,
         #n, prop_n, 
         ngrowers, nsupplychainactors,
         sum_rev_dnb, survey_year, Region, cropcat_n,
         contains("barrier")) %>% 
  mutate(prop_n = ngrowers/nsupplychainactors) %>%
  mutate(Region = factor(Region, levels = c("North Central", "West", 
                                            "Northeast", "South"))) %>% 
  mutate(TotalAcreage_ls = c(scale(log(TotalAcreage))), 
         funding_avg_annual_amt_ls = c(scale(log(funding_amt))),
         OGSeedOGIntegrityn_s = c(scale(OGSeedOGIntegrityn)),
         prop_n_s = as.numeric(scale(prop_n)),
         sum_rev_s = as.numeric(scale(sum_rev_dnb)),
         barrier_availability_s = c(scale(barrier_availability)),
         barrier_price_s = c(scale(barrier_price)),
         barrier_distrustQuality_s = c(scale(barrier_distrustQuality)),
         barrier_buyerReq_s = c(scale(barrier_buyerReq))
         ) %>% 
  mutate(survey_year_f = factor(survey_year))

df1 <- select(df.full, OGseed_percent_maincrop, maincrop,
              CertifierRequestn,
              TotalAcreage_ls, 
              funding_avg_annual_amt_ls,
              OGSeedOGIntegrityn_s,
              prop_n_s, sum_rev_s,
              Region, survey_year_f) %>% 
  filter(complete.cases(.))

df2 <- select(df.full, colnames(df1), 
              barrier_availability_s,
              barrier_price_s,
              barrier_buyerReq_s) %>% 
  filter(complete.cases(.))

acreage <- lmer(OGseed_percent_maincrop ~ 
                  OGSeedOGIntegrityn_s + 
                  TotalAcreage_ls + 
                  CertifierRequestn + 
                  funding_avg_annual_amt_ls + 
                  prop_n_s +
                  sum_rev_s +
                  maincrop +
                  (1|survey_year_f) +
                  (1|Region),
                data = df1)

acreage_b <- brm(OGseed_percent_maincrop ~ 
                  OGSeedOGIntegrityn_s + 
                  TotalAcreage_ls + 
                  CertifierRequestn + 
                  funding_avg_annual_amt_ls + 
                  prop_n_s +
                  sum_rev_s +
                  maincrop +
                  (1|survey_year_f) +
                  (1|Region),
                data = df1)


summary(acreage)
summary(acreage_b)
equatiomatic::extract_eq(acreage)

# This will tell us how far the random effect is away from the overall mean
ranef(acreage)$Region
ranef(acreage_b)$Region
ranef(acreage)$survey_year_f
ranef(acreage_b)$survey_year_f
ranef(acreage)$maincrop
ranef(acreage_b)$maincrop

# Self-reported change ----

df3 <- select(df.full, IncreasedOGSeed_binom, maincrop,
              CertifierRequestn,
              TotalAcreage_ls, 
              funding_avg_annual_amt_ls,
              OGSeedOGIntegrityn_s,
              prop_n_s, sum_rev_s,
              Region, survey_year_f) %>% 
  filter(complete.cases(.))

df4 <- select(df.full, colnames(df3), 
              barrier_availability_s,
              barrier_price_s,
              barrier_buyerReq_s) %>% 
  filter(complete.cases(.))

increase <- glmer(IncreasedOGSeed_binom ~ 
                  OGSeedOGIntegrityn_s + 
                  TotalAcreage_ls + 
                  CertifierRequestn + 
                  funding_avg_annual_amt_ls + 
                  prop_n_s +
                  sum_rev_s +
                  maincrop +
                  (1|survey_year_f) +
                  (1|Region),
                  family = "binomial",
                  data = df3)

increase_b <- brm(IncreasedOGSeed_binom ~ 
                    OGSeedOGIntegrityn_s + 
                    TotalAcreage_ls + 
                    CertifierRequestn + 
                    funding_avg_annual_amt_ls + 
                    prop_n_s +
                    sum_rev_s +
                    maincrop +
                    (1|survey_year_f) +
                    (1|Region),
                  family = bernoulli(link = "logit"),
                  data = df3)



summary(increase)
summary(increase_b)
equatiomatic::extract_eq(increase)

# This will tell us how far the Region is away from the overall mean
ranef(increase)$Region
ranef(increase_b)$Region
ranef(increase)$survey_year_f
ranef(increase_b)$survey_year_f
ranef(increase)$maincrop
ranef(increase_b)$maincrop

a1 <- acreage_b$coef
i1 <- exp(increase_b$coef)

stargazer(acreage, #acreage_barriers, 
          increase, #increase_barriers, 
          type = "html", 
          title = "Linear model comparison",
          dep.var.labels = c("% Acreage", "Increased OG seed use"),
          covariate.labels = c("Values organic seed", 
                             #"Barrier: Variety availability", 
                             #"Barrier: Price",
                             #"Barrier: Buyer restriction",
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
          out = c("tables/models/lm.html"))


acreage_plot_f <- sjPlot::plot_model(acreage_b, show.values = T, 
                                     value.size = 2,
                                     bpe.style = "dot",
                                     bpe.color = "black",
                                     vline.color = "black",
                                     line.size = .5,
                                     group.terms = c(1,1,1,2,2,2),
                                     colors = c("black", "darkgray"),
                                     axis.labels = 
                                     rev(c("Values organic seed",  
                                     "Total Acres (log)",
                                     "Certifier request", 
                                     "Research funding\n(annual-avg.)", 
                                     "Ratio of supply chain actors to growers",
                                     "Median revenue of\nseed-supply chain")))  + 
  theme_linedraw() +
  labs(title = "", y = "") ; acreage_plot_f
                                     
acreage_plot_r <- sjPlot::plot_model(acreage_b, show.values = T, 
                                     value.size = 2,
                                     type = "re",
                                     bpe.style = "dot",
                                     bpe.color = "black",
                                     vline.color = "black",
                                     line.size = .5,
                                     #group.terms = c(1,1,1,2,2,2),
                                     colors = "darkgray",
                                     axis.labels = 
                                       rev(c("Crop type: Field",
                                         "Crop type: Forage",
                                         "Crop type: Vegetable",
                                         "Region: North Central",
                                         "Region: West",
                                         "Region: Northeast",
                                         "Region: South",
                                         "Year: 2010",
                                         "Year: 2015",
                                         "Year: 2020"))) + 
  theme_linedraw() +
  labs(title = "", y = "Coefficient estimates") ; acreage_plot_r

# Transformation is automatic for logistic
increase_plot_f <- sjPlot::plot_model(increase_b, show.values = T, 
                                      value.size = 2,
                                     bpe.style = "dot",
                                     bpe.color = "black",
                                     vline.color = "black",
                                     line.size = .5,
                                     group.terms = c(1,1,1,2,2,2),
                                     colors = c("black", "darkgray"),
                                     axis.labels = 
                                       rev(c("Values organic seed",  
                                             "Total Acres (log)",
                                             "Certifier request", 
                                             "Research funding\n(annual-avg.)", 
                                             "Ratio of supply chain actors to growers",
                                             "Median revenue of\nseed-supply chain")),
                                     axis.lim = c(.6, 2))  + 
  theme_linedraw() +
  labs(title = "", y = ""); increase_plot_f

increase_plot_r <- sjPlot::plot_model(increase_b, show.values = T, 
                                      value.size = 2,
                                      type = "re",
                                     bpe.style = "dot",
                                     bpe.color = "black",
                                     vline.color = "black",
                                     line.size = .5,
                                     #group.terms = c(1,1,1,2,2,2),
                                     colors = "darkgray",
                                     axis.labels = 
                                       rev(c("Crop type: Field",
                                             "Crop type: Forage",
                                             "Crop type: Vegetable",
                                             "Region: North Central",
                                             "Region: West",
                                             "Region: Northeast",
                                             "Region: South",
                                             "Year: 2010",
                                             "Year: 2015",
                                             "Year: 2020")),
                                     axis.lim = c(.1, 10)) + 
  theme_linedraw() +
  labs(title = "", y = "Log of coefficient estimates") ; increase_plot_r


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