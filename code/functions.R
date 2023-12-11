coef_order <- c("Barrier: Organic availability",
                "Barrier: Buyer requirements",
                "Barrier: Undesirable traits",
                "Barrier: Insufficient quantity",
                "Year: 2015 (vs. 2010)",
                "Year: 2020 (vs. 2010)",
                "Values organic seed",
                "Farm size (log acres)",
                "Crop diversity", 
                "Years certified organic",
                "Seed saved or traded (%)",
                "Certifiers request organic seed use",
                "Forage crops (vs. Field)",
                "Vegetable crops (vs. Field)",
                "Western region (vs. North Central)",
                "Northeastern region (vs. North Central)",
                "Southern region (vs. North Central)",
                "(Intercept)")

shapes <- 21:25
prep_inla_data <- function(m){
  mdf <- m$summary.fixed
  mdf$term = rownames(mdf)
  mdf$se <- mdf$sd/sqrt(nrow(m$summary.linear.predictor))
  colnames(mdf)[c(3,5)] <- c('LCI', 'UCI')
  mdf <- mdf %>% 
    mutate(term = case_when(
      term == "legitimation_user_s" ~ "Values organic seed",
      term == "CertifierRequestn_s" ~ "Certifiers request organic seed use",
      term == "noncommercial_seedsource_s" ~ "Seed saved or traded (%)", 
      term == "acreage_s" ~ "Farm size (log acres)", 
      term == "cropcat_n_s" ~ "Crop diversity", 
      term == "years_certified_s" ~ "Years certified organic", 
      term == "barrier_buyerReq_s" ~ "Barrier: Buyer requirements",
      term == "barrier_insuffQuantity_s" ~ "Barrier: Insufficient quantity", 
      term == "barrier_lackdesiredTraits_s" ~ "Barrier: Undesirable traits", 
      term == "barrier_availability_s" ~ "Barrier: Organic availability", 
      term == "maincropForage" ~ "Forage crops (vs. Field)", 
      term == "maincropVegetables" ~ "Vegetable crops (vs. Field)", 
      term == "RegionWest" ~ "Western region (vs. North Central)",
      term == "RegionNortheast" ~ "Northeastern region (vs. North Central)", 
      term == "RegionSouth" ~ "Southern region (vs. North Central)", 
      term == "survey_year2016" ~ "Year: 2015 (vs. 2010)",
      term == "survey_year2021" ~ "Year: 2020 (vs. 2010)",
      T ~ term)) %>% 
    mutate(term = factor(term, rev(coef_order))) %>% 
    arrange(term)
  return(mdf)
}
prep_betreg_data <- function(m){
  ms <- summary(m)
  mdf <- data.frame(ms$coefficients$mean)
  mdf$term = rownames(mdf)
  mdf$LCI <- mdf$Estimate - 1.96 * mdf$Std..Error
  mdf$UCI <- mdf$Estimate + 1.96 * mdf$Std..Error
  colnames(mdf)[c(1,2,6,7)] <- c('mean', 'se', 'LCI', 'UCI')
  mdf <- mdf %>% 
    mutate(term = case_when(
      term == "legitimation_user_s" ~ "Values organic seed",
      term == "CertifierRequestn_s" ~ "Certifiers request organic seed use",
      term == "noncommercial_seedsource_s" ~ "Seed saved or traded (%)", 
      term == "acreage_s" ~ "Farm size (log acres)", 
      term == "cropcat_n_s" ~ "Crop diversity", 
      term == "years_certified_s" ~ "Years certified organic", 
      term == "barrier_buyerReq_s" ~ "Barrier: Buyer requirements",
      term == "barrier_insuffQuantity_s" ~ "Barrier: Insufficient quantity", 
      term == "barrier_lackdesiredTraits_s" ~ "Barrier: Undesirable traits", 
      term == "barrier_availability_s" ~ "Barrier: Organic availability", 
      term == "maincropForage" ~ "Forage crops (vs. Field)", 
      term == "maincropVegetables" ~ "Vegetable crops (vs. Field)", 
      term == "RegionWest" ~ "Western region (vs. North Central)",
      term == "RegionNortheast" ~ "Northeastern region (vs. North Central)", 
      term == "RegionSouth" ~ "Southern region (vs. North Central)", 
      term == "survey_year2016" ~ "Year: 2015 (vs. 2010)",
      term == "survey_year2021" ~ "Year: 2020 (vs. 2010)",
      T ~ term)) %>% 
    mutate(term = factor(term, rev(coef_order))) %>% 
    arrange(term)
  return(mdf)
}

plot_inla_models <- function(mdf, nmodels){
  if(nmodels == 1){
    p <- mdf  %>% 
      ggplot(aes(x = mean, 
                 y = term)) +
      ggstance::geom_pointrangeh(aes(xmin = LCI, 
                                     xmax = UCI),
                                 position = ggstance::position_dodgev(height = .8), 
                                 fill = "white", fatten = 2, size = 1) +
      geom_vline(xintercept = 0, linetype = 2, size = 0.25) + 
      jtools::theme_nice(base_family = "Times") + 
      jtools::drop_y_gridlines() + 
      theme(axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 12),
            text = element_text(size = 14),
            panel.grid.major.x = element_line(linetype = "solid")) + 
      labs(x = "Coefficient Estimate", shape = "", color = "", y = "")
    return(p)
  } else {
    p <- mdf  %>% 
      ggplot(aes(x = mean, 
                 y = term,
                 color = factor(model), 
                 shape = factor(model))) +
      ggstance::geom_pointrangeh(aes(xmin = LCI, 
                                     xmax = UCI),
                                 position = ggstance::position_dodgev(height = .8), 
                                 fill = "white", fatten = 2, size = 1) +
      geom_vline(xintercept = 0, linetype = 2, size = 0.25) + 
      scale_colour_manual(values = viridis::mako(n = nmodels, begin = .1, end = .9),
                          guide = guide_legend(reverse = TRUE)) + 
      scale_shape_manual(values = shapes[1:nmodels],
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
    return(p)
  }
}

make_pp_df <- function(subset){
  row.names(subset) <- NULL
  df <- suppressWarnings(
    data.frame(acreage_s = quantile(subset$acreage_s, na.rm = T)[c(2,5)],
                   legitimation_user_s = quantile(subset$legitimation_user_s, 
                                                  na.rm = T)[c(5,2)],
                   cropcat_n_s = quantile(subset$cropcat_n_s, na.rm = T)[c(5,2)],
                   CertifierRequestn_s = rep(0, 2),
                   noncommercial_seedsource_s = 
                     rep(median(subset$noncommercial_seedsource_s, 
                                na.rm = T), 2),
                   barrier_buyerReq_s = rep(median(subset$barrier_buyerReq_s,
                                                   na.rm = T), 2),
                   barrier_insuffQuantity_s = 
                     rep(median(subset$barrier_insuffQuantity_s, na.rm = T), 2),
                   barrier_lackdesiredTraits_s = 
                     rep(median(subset$barrier_lackdesiredTraits_s, na.rm = T), 2),
                   barrier_availability_s = 
                     rep(median(subset$barrier_availability_s, na.rm = T), 2),
                   maincrop = rep(subset$maincrop, 2),
                   Region = rep(subset$Region, 2),
                   survey_year = rep(subset$survey_year, 2))) %>% 
    unique() %>% 
    mutate(model = c('Principled', 'Practical'))
  return(df)
}

plot_predicted_probs <- function(pred_df, ymin = 0.2, ymax = 0.8){
  p <- pred_df  %>% 
    mutate(survey_year = case_when(survey_year == 2011 ~ 2010, 
                                   survey_year == 2016 ~ 2015, T ~2020)) %>% 
    ggplot(aes(x = model, y = mean, color = factor(Region, 
                                                   c("North Central", "Northeast",
                                                     "South", "West")))) +
    geom_point(size = 4, alpha = .8,) +
    geom_line(aes(group = factor(Region, 
                                 c("North Central", "Northeast",
                                   "South", "West")))) +
    #geom_linerange(aes(ymin = `0.025quant`, ymax = `0.975quant`)) +
    facet_wrap(~survey_year) +
    theme_minimal() +
    scale_color_viridis_d(option = "D", begin = .1, end = .9) +
    theme(legend.position = "bottom",
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          text = element_text(size = 14, family = "Times"),
          panel.grid.major.x = element_line(linetype = "solid")) + 
    labs(x = "", shape = "", color = "", y = "Predicted acreage planted\nto conventional seed") +
    ylim(c(ymin,ymax))
  p
} 
