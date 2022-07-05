library(dplyr)
library(tidyr)
library(stargazer)
library(lme4)
library(ggplot2)

df <- readRDS("~/Box/organicseed/df.rds")

df$IncreasedOGSeed_f <- factor(df$IncreasedOGSeed , 
                               levels = c("No", "Yes", "Already at 100%"))
df$IncreasedOGSeed_binom <- ifelse(df$IncreasedOGSeed_f == "No", 0,
                                   ifelse(df$IncreasedOGSeed_f == "Yes", 1, NA))
# Considering acreage as the dependent variable ----
## Use c() when scaling numbers because it "strips" the dimension (i.e. scaling converts them into some kind of matrix and Stack Overflow told me that c would strip this)
df.full <- df %>% 
  filter(Region != "International") %>% 
  select(RespondentID, OGseed_percent_maincrop, IncreasedOGSeed_binom, maincrop, 
         TotalAcreage, OGSeedOGIntegrityn,CertifierRequestn,
         funding_avg_annual_amt, survey_year, Region, contains("barrier")) %>% 
  mutate(Region = factor(Region, levels = c("West", "North Central", 
                                            "Northeast", "South"))) %>% 
  mutate(TotalAcreage_ls = c(scale(log(TotalAcreage))), 
         funding_avg_annual_amt_ls = c(scale(log(funding_avg_annual_amt))),
         OGSeedOGIntegrityn_s = c(scale(OGSeedOGIntegrityn)),
         barrier_availability_s = c(scale(barrier_availability)),
         barrier_price_s = c(scale(barrier_price)),
         barrier_distrustQuality_s = c(scale(barrier_distrustQuality)),
         barrier_buyerReq_s = c(scale(barrier_buyerReq))
         ) 

df1 <- select(df.full, OGseed_percent_maincrop, maincrop,
              CertifierRequestn,
              TotalAcreage_ls, 
              funding_avg_annual_amt_ls,
              OGSeedOGIntegrityn_s,
              Region, survey_year) %>% 
  filter(complete.cases(.))

df2 <- select(df.full, colnames(df1), 
              barrier_availability_s,
              barrier_price_s,
              barrier_buyerReq_s) %>% 
  filter(complete.cases(.))

acreage <- lm(OGseed_percent_maincrop ~ 
                        OGSeedOGIntegrityn_s + 
                        maincrop + 
                        TotalAcreage_ls + 
                        CertifierRequestn +  
                        funding_avg_annual_amt_ls + 
                        Region +
                        factor(survey_year), 
              data = df1)


acreage_barriers <- lm(OGseed_percent_maincrop ~ 
                         OGSeedOGIntegrityn_s + 
                         barrier_availability_s +
                         barrier_price_s +
                         barrier_buyerReq_s +
                         maincrop + 
                         TotalAcreage_ls + 
                         CertifierRequestn +  
                         funding_avg_annual_amt_ls + 
                         Region +
                         factor(survey_year), data = df2)
summary(acreage)
summary(acreage_barriers)

# Self-reported change ----

df3 <- select(df.full, IncreasedOGSeed_binom, maincrop,
              CertifierRequestn,
              TotalAcreage_ls, 
              funding_avg_annual_amt_ls,
              OGSeedOGIntegrityn_s,
              Region, survey_year) %>% 
  filter(complete.cases(.))

df4 <- select(df.full, colnames(df3), 
              barrier_availability_s,
              barrier_price_s,
              barrier_buyerReq_s) %>% 
  filter(complete.cases(.))

increase <- glm(IncreasedOGSeed_binom ~ 
                         OGSeedOGIntegrityn_s + 
                         maincrop + 
                         TotalAcreage_ls + 
                         CertifierRequestn +  
                         funding_avg_annual_amt_ls + 
                         Region +
                         factor(survey_year), family = "binomial",
                       data = df3)

increase_barriers <- glm(IncreasedOGSeed_binom ~ 
                            OGSeedOGIntegrityn_s + 
                           barrier_availability_s +
                           barrier_price_s +
                           barrier_buyerReq_s +
                           maincrop + 
                           TotalAcreage_ls + 
                           CertifierRequestn +  
                           funding_avg_annual_amt_ls + 
                           Region +
                           factor(survey_year), family = "binomial",
                               data = df4)

summary(increase)
summary(increase_barriers)

a1 <- acreage$coef
a2 <- acreage_barriers$coef
i1 <- exp(increase$coef)
i2 <- exp(increase_barriers$coef)

stargazer(acreage, acreage_barriers, increase, increase_barriers, type = "html", 
          title = "Linear model comparison",
          dep.var.labels = c("% Acreage", "Increased OG seed use"),
          covariate.labels = c("Values organic seed", 
                               "Barrier: Variety availability", 
                               "Barrier: Price",
                               "Barrier: Buyer restriction",
                               "Crop type: Forage (vs. Field)", 
                               "Crop type: Mix (vs. Field)", 
                               "Crop type: Vegetable (vs. Field)", 
                               "Total Acres (log)", 
                               "Certified request", 
                               "Regional funding (annual average)", 
                               "Region: North Central (vs. West)",
                               "Region: Northeast (vs. West)",
                               "Region: South (vs. West)",
                               "Year: 2016",
                               "Year: 2021"),
          model.numbers = FALSE,
          no.space = TRUE, 
          header = FALSE, 
          out = "~/Documents/Davis/R-Projects/organicseed/tables/models/lm.html")

exp(increase$coefficients)

stargazer(acreage, acreage_barriers, increase,increase_barriers, type = "text", 
          title = "Linear model comparison",
          dep.var.labels = c("% Acreage", "Increased OG seed use"),
          covariate.labels = c( "Values organic seed", 
                               "Barrier: Variety availability", 
                               "Barrier: Price",
                               "Barrier: Buyer restriction",
                               "Crop type: Forage (vs. Field)", 
                               "Crop type: Mix (vs. Field)", 
                               "Crop type: Vegetable (vs. Field)", 
                               "Total Acres (log)",
                               "Certified request", 
                               "Regional funding (annual average)", 
                               "Region: North Central (vs. West)",
                               "Region: Northeast (vs. West)",
                               "Region: South (vs. West)",
                               "Year: 2016",
                               "Year: 2021"),
          model.numbers = FALSE,
          no.space = TRUE, 
          header = FALSE, 
          out = "~/Documents/Davis/R-Projects/organicseed/tables/models/lm.doc")


emmeans::emmeans(increase, specs = ~ CertifierRequestn)

# For a field crop grower in the North Central region in 2016, what is the effect of a certifier asking to increase
pp_df1 <- data.frame(maincrop = "Field",
                     TotalAcreage_ls = median(df.full$TotalAcreage_ls[df.full$Region ==
                                                                        "North Central" &
                                                                        df.full$survey_year ==
                                                                        2016], na.rm = T), 
                     OGSeedOGIntegrityn_s = median(df.full$OGSeedOGIntegrityn_s, na.rm = T), 
                     CertifierRequestn = c(0,1),
                     funding_avg_annual_amt_ls = 
                       unique(df.full$funding_avg_annual_amt_ls[df.full$Region ==
                                                              "North Central" &
                                                              df.full$survey_year ==
                                                              2016]),
                     Region = "North Central",
                     survey_year = 2016
                     )

# For a vegetable grower in the Western region in 2016, what is the effect of a certifier asking to increase
pp_df2 <- data.frame(maincrop = "Vegetables",
                     TotalAcreage_ls = median(df.full$TotalAcreage_ls[df.full$Region ==
                                                                    "West" &
                                                                    df.full$survey_year ==
                                                                    2016], na.rm = T), 
                     OGSeedOGIntegrityn_s = median(df.full$OGSeedOGIntegrityn_s, na.rm = T), 
                     CertifierRequestn = c(0,1),
                     funding_avg_annual_amt_ls = 
                       unique(df.full$funding_avg_annual_amt_ls[df.full$Region ==
                                                              "West" &
                                                              df.full$survey_year ==
                                                              2016]),
                     Region = "West",
                     survey_year = 2016
)

# Using predict function to get predicted probabilities, with standard errors
?predict
# I am asking it to predict on the scale of the response variable
pred_acreage_cert_ncentral16 <- cbind(pp_df1, predict(acreage, newdata = pp_df1, 
                                                      se = T, type = "response"))
pred_acreage_cert_west16 <- cbind(pp_df2, predict(acreage, newdata = pp_df2, 
                                                  se = T, type = "response"))

pred_increase_cert_ncentral16 <- cbind(pp_df1, predict.glm(increase, newdata = pp_df1, 
                                                           se = T, type = "response"))
pred_increase_cert_west16 <- cbind(pp_df2, predict.glm(increase, newdata = pp_df2, 
                                                       se = T, type = "response"))

plot_change <- function(profile, ylab){
  profile$CIL <- profile$fit - (1.96 * profile$se.fit)
  profile$CIU <- profile$fit + (1.96 * profile$se.fit)

  profile %>% 
    ggplot(aes(x = factor(CertifierRequestn), y = fit, 
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
plot_change(pred_increase_cert_ncentral16, "Likelihood of increasing organic seed use")
plot_change(pred_increase_cert_west16, "Likelihood of increasing organic seed use")

