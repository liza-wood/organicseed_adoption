dvs_core <- "legitimation_user + 
                 #barrier_distrustQuality_s +
                 #ratio +
                 entrants_supplychain_n_avg_s +
                 #barrier_buyerReq_s +
                 #barrier_insuffQuantity_s +
                 CertifierRequestn + 
                 resources_funding_100thous + 
                 #n_nosb_recs_s +
                 #barrier_lackdesiredTraits_s +
                 knowledge_organicip_10 +
                 #barrier_availability_s +
                 #sales_mill + 
                 #barrier_saveOwn_s +
                 #SeedSourceOwn_s +
                 noncommercial_seedsource_s +
                 acreage_log +  
                 cropcat_n + 
                 years_certified_s + "
subsyst_onelevel <- "maincrop +
                     Region +
                     survey_year"
subsyst_multilevel <- "(1|maincrop) +
                     (1|Region) +
                     (1|survey_year)"

controls <-   ", control = list(adapt_delta = 0.8),
chains = 4, iter = 2000, warmup = 2000,
cores = 4, seed = 12345)"
subsyst_multilevel_inla <- "f(maincrop, model = 'iid') +
                            f(Region, model = 'iid') +
                            f(survey_year, model = 'iid')"
iv_perc <- "OGseed_percent_maincrop ~ "
iv_prop <- "OGseed_percent_maincrop_prop ~ "
iv_binary <- "OGseed_percent_maincrop_binary ~ "

onelevel_prop <- paste0(iv_prop, dvs_core, subsyst_onelevel)
onelevel_perc <- paste0(iv_perc, dvs_core, subsyst_onelevel)
multilevel_prop <- paste0(iv_prop, dvs_core, subsyst_multilevel)
multilevel_perc <- paste0(iv_perc, dvs_core, subsyst_multilevel)
multilevel_prop_inla <- paste0(iv_prop, dvs_core, subsyst_multilevel_inla)
multilevel_bin_inla <- paste0(iv_binary, dvs_core, subsyst_multilevel_inla)


fullmodel_fx <- function(data, model_type){
  if(model_type == "bayes multi-level"){
    model <- brm(eval(parse(text=paste0(multilevel_perc, controls))), 
                 data = data)
  } else if(model_type == "bayes one-level"){
    model <- brm(eval(parse(text=onelevel_perc)), 
                 data = data)
  } else if(model_type == "beta inla one-level"){
    cens <- 0.05
    model <- inla(eval(parse(text=onelevel_prop)), 
                  data = data,
                  family = "beta",
                  control.compute = list(cpo=TRUE, waic=TRUE),
                  control.family = list(beta.censor.value = cens)
    )
  } else if(model_type == "beta inla multi-level"){
    cens <- 0.01
    model <- inla(eval(parse(text=multilevel_prop_inla)), 
                  data = data,
                  family = "beta",
                  control.compute = list(cpo=TRUE, waic=TRUE),
                  control.family = list(beta.censor.value = cens)
    )
  } else if(model_type == "normal inla multi-level"){
    cens <- 0.05
    model <- inla(eval(parse(text=multilevel_prop_inla)), 
                  data = data,
                  family = "gaussian",
                  control.compute = list(dic=TRUE, cpo=TRUE, waic=TRUE)
    )
  } else if(model_type == "binomial inla multi-level"){
    cens <- 0.05
    model <- inla(eval(parse(text=multilevel_bin_inla)), 
                  data = data,
                  family = "binomial",
                  control.compute = list(dic=TRUE, cpo=TRUE, waic=TRUE)
    )
  } else if(model_type == "binomial one-level"){
    model <- glm(eval(parse(text=onelevel_prop)), 
                 family = quasibinomial(link = 'logit'),
                 data = data)
  } else if(model_type == "binomial multi-level"){
    model <- glmer(eval(parse(text=multilevel_prop)), 
                   family = binomial(link = 'logit'),
                   data = data)
  } else if(model_type == "lm"){
    model <- lm(eval(parse(text=onelevel_perc)), 
                data = data)
  } else if (model_type == "tobit"){
    model <- censReg::censReg(eval(parse(text=onelevel_perc)),
                              right = 100,
                              left = 0,
                              data = data)
  } else if (model_type == "lmer"){
    model <- lmer(eval(parse(text=multilevel_perc)), 
                  data = data)
  }
  return(model)
}

dvs_core_noIP <- "legitimation_user + 
                 #barrier_distrustQuality_s +
                 #ratio +
                 entrants_supplychain_n_avg_s +
                 #barrier_buyerReq_s +
                 #barrier_insuffQuantity_s +
                 CertifierRequestn + 
                 resources_funding_100thous + 
                 #n_nosb_recs_s +
                 #barrier_lackdesiredTraits_s +
                 #knowledge_organicip_10 +
                 #barrier_availability_s +
                 #sales_mill + 
                 #barrier_saveOwn_s +
                 #SeedSourceOwn_s +
                 noncommercial_seedsource_s +
                 acreage_log +  
                 cropcat_n + 
                 years_certified_s + "

subsyst_onelevel_noyear <- "maincrop +
                     Region"

subsyst_multilevel_noyear <- "(1|maincrop) +
                     (1|Region)"

subsyst_multilevel_inla_noyear <- "f(maincrop, model = 'iid') +
                            f(Region, model = 'iid')"

onelevel_prop_21 <- paste0(iv_prop, dvs_core, subsyst_onelevel_noyear)
onelevel_perc_21 <- paste0(iv_perc, dvs_core, subsyst_onelevel_noyear)
multilevel_prop_21 <- paste0(iv_prop, dvs_core, subsyst_multilevel_noyear)
multilevel_perc_21 <- paste0(iv_perc, dvs_core, subsyst_multilevel_noyear)
multilevel_prop_inla_21 <- paste0(iv_prop, dvs_core, subsyst_multilevel_inla_noyear)
multilevel_bin_inla_21 <- paste0(iv_binary, dvs_core, subsyst_multilevel_inla_noyear)

onelevel_prop_11_16 <- paste0(iv_prop, dvs_core_noIP, subsyst_onelevel_noyear)
onelevel_perc_11_16 <- paste0(iv_perc, dvs_core_noIP, subsyst_onelevel_noyear)
multilevel_prop_11_16 <- paste0(iv_prop, dvs_core_noIP, subsyst_multilevel_noyear)
multilevel_perc_11_16 <- paste0(iv_perc, dvs_core_noIP, subsyst_multilevel_noyear)
multilevel_prop_inla_11_16 <- paste0(iv_prop, dvs_core_noIP, subsyst_multilevel_inla_noyear)
multilevel_bin_inla_11_16 <- paste0(iv_binary, dvs_core_noIP, subsyst_multilevel_inla_noyear)


yearlymodel_fx <- function(data, model_type, year){
  if(year %in% c(2011, 2016)){
    if(model_type == "bayes multi-level"){
      model <- brm(eval(parse(text = multilevel_perc_11_16)), 
                   data = data)
    } else if(model_type == "bayes one-level"){
      model <- brm(eval(parse(text = onelevel_perc_11_16)), 
                   data = data)
    } else if(model_type == "beta inla one-level"){
      cens <- 0.05
      model <- inla(eval(parse(text = onelevel_prop_11_16)), 
                    data = data,
                    family = "beta",
                    control.compute = list(cpo=TRUE, waic=TRUE),
                    control.family = list(beta.censor.value = cens)
      )
    } else if(model_type == "beta inla multi-level"){
      cens <- 0.05
      model <- inla(eval(parse(text = multilevel_prop_inla_11_16)), 
                    data = data,
                    family = "beta",
                    control.compute = list(cpo=TRUE, waic=TRUE),
                    control.family = list(beta.censor.value = cens)
      )
    } else if(model_type == "binomial one-level"){
      model <- glm(eval(parse(text = onelevel_prop_11_16)), 
                   family = quasibinomial(link = 'logit'),
                   data = data)
    } else if(model_type == "binomial multi-level"){
      model <- glmer(eval(parse(text = multilevel_prop_11_16)), 
                     family = binomial(link = 'logit'),
                     data = data)
    } else if(model_type == "lm"){
      model <- lm(eval(parse(text = onelevel_perc_11_16)), 
                  data = data)
    } else if (model_type == "lmer"){
      model <- lmer(eval(parse(text = multilevel_perc_11_16)), 
                    data = data)
    } else if (model_type == "tobit"){
      model <- censReg::censReg(eval(parse(text = onelevel_perc_11_16)),
                                right = 100,
                                left = 0,
                                data = data)
    } 
  } else if(year == 2021){
    if(model_type == "bayes multi-level"){
      model <- brm(eval(parse(text = multilevel_perc_21)), 
                   data = data)
    } else if(model_type == "bayes one-level"){
      model <- brm(eval(parse(text = onelevel_perc_21)), 
                   data = data)
    } else if(model_type == "beta inla one-level"){
      cens <- 0.05
      model <- inla(eval(parse(text = onelevel_prop_21)), 
                    data = data,
                    family = "beta",
                    control.compute = list(cpo=TRUE, waic=TRUE),
                    control.family = list(beta.censor.value = cens)
      )
    } else if(model_type == "beta inla multi-level"){
      cens <- 0.05
      model <- inla(eval(parse(text = multilevel_prop_inla_21)), 
                    data = data,
                    family = "beta",
                    control.compute = list(cpo=TRUE, waic=TRUE),
                    control.family = list(beta.censor.value = cens)
      )
    } else if(model_type == "binomial one-level"){
      model <- glm(eval(parse(text = onelevel_prop_21)), 
                   family = quasibinomial(link = 'logit'),
                   data = data)
    } else if(model_type == "binomial multi-level"){
      model <- glmer(eval(parse(text = multilevel_prop_21)), 
                     family = binomial(link = 'logit'),
                     data = data)
    } else if(model_type == "lm"){
      model <- lm(eval(parse(text = onelevel_perc_21)), 
                  data = data)
    } else if (model_type == "lmer"){
      model <- lmer(eval(parse(text = multilevel_perc_21)), 
                    data = data)
    } else if (model_type == "tobit"){
      model <- censReg::censReg(eval(parse(text = onelevel_perc_21)),
                                right = 100,
                                left = 0,
                                data = data)
    } 
  }
  return(model)
}

coefs <- c("Perceived importance of organic seed" = 
             "legitimation_user", 
           #"Seed quality distrust barrier" =
           #  "barrier_distrustQuality_s",
           #"Ratio of organic growers to supply chain actors" =
           #  "ratio",
           "Number of new organic seed supply entrants" =
             "entrants_supplychain_n_avg_s",
           #"Buyer requirement barrier" = 
           #  "barrier_buyerReq_s",
           "Certifiers request organic seed use" =
             "CertifierRequestn", 
           "Organic research funding" = 
             "resources_funding_100thous", 
           #"Limited seed traits barrier" =
           #  "barrier_lackdesiredTraits_s",
           "Number of registered organically bred varieties" =
             "knowledge_organicip_10",
           #"Seed availability barrier" = 
           #  "barrier_availability_s",
           #"Organic seed company sales" = 
           #  "sales_mill",
           "Non-commerical seed sources" = "noncommercial_seedsource_s", #"SeedSourceOwn_s",
           "Farm size (log acres)" = "acreage_log",
           "Number of crop categories" = "cropcat_n",
           "Years certified organic" = "years_certified_s")
