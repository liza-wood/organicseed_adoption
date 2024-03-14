source('code/functions.R')
setwd("~/Documents/Davis/R-Projects/organicseed_adoption/")
df.full <- readRDS("~/Box/organicseed_adoption/df_scaled.rds") %>% 
  select(CVseed_percent_maincrop_prop,
         legitimation_user_s,
         CertifierRequestn_s,
         noncommercial_seedsource_s,
         #acreage_log_s,
         acreage_s,
         cropcat_n_s,
         barrier_buyerReq_s,
         barrier_insuffQuantity_s,
         barrier_lackdesiredTraits_s,
         barrier_availability_s,
         maincrop,
         Region,
         survey_year) %>% 
  na.omit()

df.full_rounded <- df.full
df.full_rounded$CVseed_percent_maincrop_prop <- ifelse(df.full_rounded$CVseed_percent_maincrop_prop == 1, 0.999, ifelse(df.full_rounded$CVseed_percent_maincrop_prop == 0, 0.001, df.full_rounded$CVseed_percent_maincrop_prop))

m <- betareg(CVseed_percent_maincrop_prop ~ 
               legitimation_user_s + 
               acreage_s + 
               cropcat_n_s +
               barrier_buyerReq_s +
               barrier_insuffQuantity_s +
               barrier_lackdesiredTraits_s +
               barrier_availability_s +
               maincrop +
               Region +
               survey_year +
               CertifierRequestn_s + 
               noncommercial_seedsource_s, 
             data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")

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
df_og$CVseed_percent_maincrop_prop <- rep(NA, nrow(df_og))
df <- select(df_og, colnames(df.full_rounded))

df$mean <- predict(b, newdata = df, se.fit = T)
df$model <- ifelse(df$legitimation_user_s < 0, "Practical", "Principled")
plot_predicted_probs(df, .1, .8)
ggsave('figures/figurea1.png', width = 7, height = 4)

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
df_og$CVseed_percent_maincrop_prop <- rep(NA, nrow(df_og))
df <- select(df_og, colnames(df.full_rounded))

df$mean <- predict(b, newdata = df, se.fit = T)
df$model <- ifelse(df$legitimation_user_s < 0.3, "Practical", "Principled")
plot_predicted_probs(df, .1, .8)

ggsave('figures/figurea2.png', width = 7, height = 4)

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
