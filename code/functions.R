coef_order <- c("Barrier: Organic availability",
                "Barrier: Undesirable traits",
                "Barrier: Buyer requirements",
                "Barrier: Insufficient quantity",
                "Year: 2016 (vs. 2011)",
                "Year: 2021 (vs. 2011)",
                "Values organic seed",
                "Farm size (acres)",
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
      term == "acreage_s" ~ "Farm size (acres)", 
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
      term == "survey_year2016" ~ "Year: 2016 (vs. 2011)",
      term == "survey_year2021" ~ "Year: 2021 (vs. 2011)",
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
      scale_shape_manual(values = 21:25[1:nmodels],
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
  df <- data.frame(acreage_s = quantile(subset$acreage_s, na.rm = T)[c(2,5)],
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
                   survey_year = rep(subset$survey_year, 2)) %>% 
    unique() %>% 
    mutate(model = c('Principled', 'Conventionalized'))
  return(df)
}
