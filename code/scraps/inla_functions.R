prep_inla_data <- function(m){
  mdf <- m$summary.fixed
  mdf$term = rownames(mdf)
  mdf$se <- mdf$sd/sqrt(nrow(m$summary.linear.predictor))
  colnames(mdf)[c(3,5)] <- c('LCI', 'UCI')
  mdf <- mdf %>% 
    mutate(term = case_when(
      term == "OG_value_s" ~ "Values organic seed",
      term == "CertifierRequestn_s" ~ "Certifiers request organic seed use",
      term == "CertifierRequestn" ~ "Certifiers request organic seed use",
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