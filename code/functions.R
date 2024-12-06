region_text <- function(df){
  n.by.state <- df %>% 
    group_by(Region, State) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(StateN = sum(n), 
           Stateprop = n/StateN,
           StatePercent = paste(round(100*(n/StateN), 1), "%"),
           StateN = sum(n), ) %>% 
    rename("Staten" = n) 
  n.by.region <- df %>% 
    group_by(Region) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(RegionN = sum(n), 
           Regionprop = n/RegionN,
           RegionPercent = paste(round(100*(n/RegionN), 1), "%")) %>% 
    rename("Regionn" = n) %>% 
    ungroup()
  
  counts <- left_join(n.by.state, n.by.region)
  
  return(counts)
}

make_map <- function(df, mapdf){
  # This one is finnicky but works
  us_states <- left_join(us_states_og, mapdf, by = c("abbr" = "State")) %>% 
    rename("State" = "full") %>% 
    mutate(State = trimws(State)) 
  
  # For the states that weren't in the dataset
  us_states <- us_states %>% 
    mutate(Region = case_when(
      State %in% western ~ "West",
      State %in% northcentral ~ "North Central",
      State %in% southern ~ "South",
      State %in% northeast ~ "Northeast",
      T ~ NA_character_)) %>% 
    mutate(Regionn = case_when(
      is.na(Regionn) & Region == "Northeast" ~ 
        unique(mapdf$Regionn[mapdf$Region == "Northeast"]),
      is.na(Regionn) & Region == "West" ~ 
        unique(mapdf$Regionn[mapdf$Region == "West"]),
      is.na(Regionn) & Region == "South" ~ 
        unique(mapdf$Regionn[mapdf$Region == "South"]),
      is.na(Regionn) & Region == "North Central" ~ 
        unique(mapdf$Regionn[mapdf$Region == "North Central"]),
      T ~ Regionn),
      RegionPercent = case_when(
        is.na(RegionPercent) & Region == "Northeast" ~ unique(mapdf$RegionPercent[mapdf$Region == "Northeast"]),
        is.na(RegionPercent) & Region == "West" ~ unique(mapdf$RegionPercent[mapdf$Region == "West"]),
        is.na(RegionPercent) & Region == "North Central" ~ unique(mapdf$RegionPercent[mapdf$Region == "North Central"]),
        is.na(RegionPercent) & Region == "South" ~ unique(mapdf$RegionPercent[mapdf$Region == "South"]),
        T ~ RegionPercent))
  
  # To plot points on the map
  loc <- data.frame(lon = df$long, lat = df$lat, year = df$year)
  transformed <- usmap_transform(loc)
  coords <- unlist(sf::st_geometry(transformed$geometry)) %>% 
    matrix(ncol=2,byrow=TRUE) %>% 
    as_tibble() %>% 
    setNames(c("lon","lat"))
  transformed <- cbind(transformed, coords) %>% 
    #filter(lat < 51 & lat > 21) %>% 
    rename(x = lon, y = lat) 
  transformed <- transformed %>% 
    mutate(year = case_when(year == 2011 ~ 2010, year == 2016 ~ 2015,
                            year == 2021 ~ 2020, T ~ NA)) 

  #plot_usmap() +
  #  geom_point(data = transformed, aes(x = x, y = y), colour = "#2d677f", shape=21, size = 2)
  
  # To plot regional blocks
  counts <- data.frame(us_states) %>% select(Region, Regionn) %>% 
    unique() %>% arrange(Regionn)
  
  us_states$n <- factor(us_states$Regionn)
  levels(us_states$n) <- c(paste(counts[1,1], counts[1,2], sep = ": "),
                           paste(counts[2,1], counts[2,2], sep = ": "),
                           paste(counts[3,1], counts[3,2], sep = ": "),
                           paste(counts[4,1], counts[4,2], sep = ": "))
  
  p <- ggplot() +
    geom_polygon(data = us_states, fill = "white", size = 0.2,
                 color = "black",
                 aes(x = x, y = y, 
                     #color = factor(Regionn), 
                     group = group)) + 
    geom_point(data = transformed, aes(x = x, y = y, colour = factor(year)) , 
               alpha = .4,shape=20, size = 2,
               position=position_jitter(width=10000, height=10000)) +
    scale_color_manual(values = mako[c(2,5,8)]) + 
    labs(title = "",
         color = "",
         x = "", y = "") + 
    theme_light(base_size = 10)  +
    theme(
      #legend.position="bottom", # was bottom
      legend.position = c(0.5, 0.05),
      legend.direction = "horizontal",
      panel.grid = element_blank(),
      line = element_blank(),
      rect = element_blank(),
      axis.text = element_blank(),
      text = element_text(family = "Times", size = 14)
      #axis.title = element_text(family = FONTTYPE, size = TITLESIZE)
    ) +
    guides(color = guide_legend(override.aes = list(size = 10)))
  return(p)
  
}

coef_order_core <- c(#"Barrier: Availability",
                     #"Barrier: Buyer requirements",
                     #"Barrier: Quality",
                     #"Barrier: Quantity",
                     #"Barrier: Form",
                     "Ability",
                     "Duty",
                     "Ability x Duty",
                     "Certifier request",
                     "Farm size (log acres)",
                     "Crop diversity", 
                     "Seed saved or traded (\\%)",
                     "Years certified organic",
                     "Forage crops (vs. Field)",
                     "Vegetable crops (vs. Field)",
                     "Western region (vs. North Central)",
                     "Northeastern region (vs. North Central)",
                     "Southern region (vs. North Central)",
                     "Year: 2010 (vs. 2015)",
                     "Year: 2020 (vs. 2015)")

coef_order_stargazer <- c(#"Barrier\\: Availability",
                #"Barrier\\: Buyer requirements",
                #"Barrier\\: Quality",
                #"Barrier\\: Quantity",
                #"Barrier\\: Form",
                "Ability",
                "Duty",
                "Ability x Duty",
                "Certifier request",
                "Farm size \\(log acres\\)",
                "Crop diversity", 
                "Seed saved or traded \\(\\%\\)",
                "Years certified organic",
                "Forage crops \\(vs\\. Field\\)",
                "Vegetable crops \\(vs\\. Field\\)",
                "Western region \\(vs\\. North Central\\)",
                "Northeastern region \\(vs\\. North Central\\)",
                "Southern region \\(vs\\. North Central\\)",
                "Year\\: 2010 \\(vs\\. 2015\\)",
                "Year\\: 2020 \\(vs\\. 2015\\)",
                "Certifier 7",
                "Certifier 42",
                "Certifier 34",
                "Certifier 27",
                "Certifier 24",
                "Certifier 2",
                "Random sample \\(vs\\. convenience\\)",
                "Distributor: OSA (vs. Other)",
                "Constant")

coef_order <- c( "Ability",
                 "Duty",
                 "Ability x Duty",
                "Certifier request",
                "Farm size (log acres)",
                "Crop diversity", 
                "Seed saved or traded (\\%)",
                "Years certified organic",
                "Forage crops (vs. Field)",
                "Vegetable crops (vs. Field)",
                "Western region (vs. North Central)",
                "Northeastern region (vs. North Central)",
                "Southern region (vs. North Central)",
                "Year: 2010 (vs. 2015)",
                "Year: 2020 (vs. 2015)",
                "(Intercept)",
                "Certifier Other",
                "Certifier 24",
                "Certifier 2",
                "Certifier 27",
                "Certifier 42",
                "Certifier 7",
                "Random sample (vs. convenience)",
                "Distributor: OSA (vs. Other)")

shapes <- 21:25

prep_betreg_data <- function(m){
  ms <- summary(m)
  mdf <- data.frame(ms$coefficients$mean)
  mdf$term = rownames(mdf)
  mdf$LCI <- mdf$Estimate - 1.96 * mdf$Std..Error
  mdf$UCI <- mdf$Estimate + 1.96 * mdf$Std..Error
  colnames(mdf)[c(1,2,6,7)] <- c('mean', 'se', 'LCI', 'UCI')
  mdf <- mdf %>% 
    mutate(term = case_when(
      term == "OG_value_s" ~ "Duty",
      #term == "CertifierRequestn_s" ~ "Certifiers request organic seed use",
      term == "CertifierRequestn" ~ "Certifier request",
      term == "noncommercial_seedsource_s" ~ "Seed saved or traded (\\%)", 
      term == "acreage_log_s" ~ "Farm size (log acres)", 
      term == "cropcat_n_s" ~ "Crop diversity", 
      term == "years_certified_s" ~ "Years certified organic", 
      term == "barrier_allowable_s" ~ "Inability: Seed sourcing challenges",
      term == "barrier_allowable_rev_s" ~ "Ability: Allowable",
      term == "barrier_all_rev_s" ~ "Ability",
      term == "barrier_buyerReq_s" ~ "Barrier: Buyer requirements",
      term == "barrier_seedTreatlack_s" ~ "Barrier: Form",
      term == "barrier_insuffQuantity_s" ~ "Barrier: Quantity", 
      term == "barrier_lackdesiredTraits_s" ~ "Barrier: Quality", 
      term == "barrier_availability_s" ~ "Barrier: Availability", 
      term == "maincropForage" ~ "Forage crops (vs. Field)", 
      term == "maincropVegetables" ~ "Vegetable crops (vs. Field)", 
      term == "RegionWest" ~ "Western region (vs. North Central)",
      term == "RegionNortheast" ~ "Northeastern region (vs. North Central)", 
      term == "RegionSouth" ~ "Southern region (vs. North Central)", 
      #term == "survey_year2016" ~ "Year: 2015 (vs. 2010)",
      term == "survey_year2011" ~ "Year: 2010 (vs. 2015)",
      term == "survey_year2021" ~ "Year: 2020 (vs. 2015)",
      term == "barrier_allowable_s:OG_value_s" ~ "Inability x Duty",
      term == "barrier_allowable_rev_s:OG_value_s" ~ "Capability x Duty",
      term == "barrier_all_rev_s:OG_value_s" ~ "Ability x Duty",
      # For extra tests in the appendix
      term == "certifier_codenameother_certifier" ~ "Certifier Other",
      term == "certifier_codenamecertifier7" ~ "Certifier 7",
      term == "certifier_codenamecertifier42" ~ "Certifier 42",
      term == "certifier_codenamecertifier34" ~ "Certifier 34",
      term == "certifier_codenamecertifier27" ~ "Certifier 27",
      term == "certifier_codenamecertifier24" ~ "Certifier 24",
      term == "certifier_codenamecertifier2" ~ "Certifier 2",
      term == "data_coll_methodRandom" ~ "Random sample (vs. convenience)",
      term == "distributorOSA" ~  "Distributor: OSA (vs. Other)",
      T ~ term)) %>% 
    mutate(term = factor(term, rev(coef_order))) %>% 
    arrange(term)
  return(mdf)
}

plot_betareg_models <- function(mdf, nmodels){
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
    data.frame(acreage_log_s = quantile(subset$acreage_log_s, na.rm = T)[c(2,4)],
                   OG_value_s = quantile(subset$OG_value_s, 
                                                  na.rm = T)[c(4,2)],
                   cropcat_n_s = quantile(subset$cropcat_n_s, na.rm = T)[c(4,2)],
                   CertifierRequestn = rep(0, 2),
                   noncommercial_seedsource_s = 
                     rep(median(subset$noncommercial_seedsource_s, 
                                na.rm = T), 2),
                   #barrier_buyerReq_s = rep(median(subset$barrier_buyerReq_s,
                   #                                na.rm = T), 2),
               barrier_seedTreatlack_s = 
                 rep(median(subset$barrier_seedTreatlack_s, na.rm = T), 2),
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
    labs(x = "", shape = "", color = "", y = "Predicted acreage planted\nto organic seed") +
    ylim(c(ymin,ymax))
  p
} 
