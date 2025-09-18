library(tidyverse)
library(lubridate)
library(ggplot2)
library(kableExtra)
library(flextable)
# This is the dataframe where I have identified main crops of growers, so each observation is only one grower
df <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds")

## Get everything of interest
df <- df %>% 
  filter(Region != "International", maincrop != "Unknown") %>% 
  select(maincrop, OGseed_percent_maincrop, IncreasedOGSeed,
         VegiAcreage, FieldAcreage, ForageAcreage, 
         OGVegiSeed, OGFieldSeed, OGForageSeed, 
         TotalAcreage, 
         OGSeedOGIntegrity, 
         OGSeedOGIntegrityf, 
         OGSeedOGIntegrityn, 
         OGVarietiesSuccess, 
         OGVarietiesSuccessf, 
         OGVarietiesSuccessn, 
         OG_value,
         CertifierRequest,
         CertifierRequestn, 
         survey_year, 
         cropcat_n, 
         Region, 
         YearsCertified,
         contains("barrier"), 
         noncommercial_seedsource,
         contains("AddSteps"),
         RespondentID) 

model_ids <- readRDS('data/model_ids.RDS')
df.full <- df %>% 
  filter(RespondentID %in% model_ids) %>% 
  select(OGseed_percent_maincrop,
                  barrier_buyerReq,
                  barrier_price,
                  barrier_insuffQuantity,
                  barrier_lackdesiredTraits,
                  barrier_availability, 
                  barrier_seedTreatlack,
                  OG_value,
                  OGVarietiesSuccessn,
                  OGSeedOGIntegrityn,
                  CertifierRequestn,
                  maincrop, survey_year, Region, 
                  TotalAcreage, cropcat_n, 
                  YearsCertified,
                  VegiAcreage, FieldAcreage, ForageAcreage,
                  noncommercial_seedsource) %>% 
  #na.omit() %>% Don't need to do this because I read in model IDs from
  # the completely cleaned and transformed data to filter
  mutate(barrier_all = purrr::pmap_dbl(list(barrier_availability,
                                            barrier_insuffQuantity,
                                            barrier_lackdesiredTraits,
                                            barrier_seedTreatlack,
                                            barrier_price,
                                            barrier_buyerReq), max), 
         barrier_all_rev = ifelse(barrier_all == 1, 4,
                           ifelse(barrier_all == 2, 3,
                           ifelse(barrier_all == 3, 2,
                           ifelse(barrier_all == 4, 1, NA))))) %>% 
  mutate(barrier_availability_rev = ifelse(barrier_availability == 1, 4,
                                    ifelse(barrier_availability == 2, 3,
                                    ifelse(barrier_availability == 3, 2,
                                    ifelse(barrier_availability == 4, 1, NA))))) %>% 
  mutate(barrier_insuffQuantity_rev = ifelse(barrier_insuffQuantity == 1, 4,
                                      ifelse(barrier_insuffQuantity == 2, 3,
                                      ifelse(barrier_insuffQuantity == 3, 2,
                                      ifelse(barrier_insuffQuantity == 4, 1, NA))))) %>% 
  mutate(barrier_lackdesiredTraits_rev = ifelse(barrier_lackdesiredTraits == 1, 4,
                                         ifelse(barrier_lackdesiredTraits == 2, 3,
                                         ifelse(barrier_lackdesiredTraits == 3, 2,
                                         ifelse(barrier_lackdesiredTraits == 4, 1, NA))))) %>% 
  mutate(barrier_seedTreatlack_rev = ifelse(barrier_seedTreatlack == 1, 4,
                                     ifelse(barrier_seedTreatlack == 2, 3,
                                     ifelse(barrier_seedTreatlack == 3, 2,
                                     ifelse(barrier_seedTreatlack == 4, 1, NA))))) %>% 
  mutate(barrier_price_rev = ifelse(barrier_price == 1, 4,
                             ifelse(barrier_price == 2, 3,
                             ifelse(barrier_price == 3, 2,
                             ifelse(barrier_price == 4, 1, NA))))) %>% 
  mutate(barrier_buyerReq_rev = ifelse(barrier_buyerReq == 1, 4,
                                ifelse(barrier_buyerReq == 2, 3,
                                ifelse(barrier_buyerReq == 3, 2,
                                ifelse(barrier_buyerReq == 4, 1, NA))))) %>% 
  mutate(OGseed_percent_maincrop_prop = OGseed_percent_maincrop/100)

df11 <- filter(df.full, survey_year == 2011)
df16 <- filter(df.full, survey_year == 2016)
df21 <- filter(df.full, survey_year == 2021) 


# Table 1 Construction ----
#table1.1 <- read.csv('~/Documents/Davis/R-Projects/organicseed_adoption/tables/table1_manual#.csv') 
#
#kbl(table1.1, booktabs = T, caption = 'Organic seed grower survey questions, key variables, and #hypotheses', align = c('l', 'l', rep('c', 3))) %>% kable_styling(latex_options = 'scale_down') %#>% row_spec(2, hline_after = T) %>% row_spec(5, hline_after = T) %>% row_spec(6, hline_after = T#) %>% row_spec(26, hline_after = T) %>% row_spec(27, hline_after = T)

ltm::cronbach.alpha(df.full[,c('OGVarietiesSuccessn', 'OGSeedOGIntegrityn')],
                    na.rm = T)
ltm::cronbach.alpha(df.full[,c('barrier_availability', 'barrier_insuffQuantity',
                               'barrier_lackdesiredTraits', 'barrier_seedTreatlack',
                               'barrier_price', 'barrier_buyerReq')],
                    na.rm = T)


formulas <- c("", "", "", "\\min_i(a_i)", "", "", "",
 "", "\\frac{1}{2}(d_1 + d_2)", "")

tbl <- data.frame("Variables" = c("", "", "", "Ability", "", "","", "", "Duty", ""),
                  "Questions" = c("\\text{Over the last three years, how much were each of} \\ \\text{the following a factor in your decision NOT to purchase organic seed?}",
                                  "    \\text{Availability: Variety is not available} \\ \\text{as organic}\\:(a_1)",
                                  "    \\text{Quality: Lack of desirable genetic traits}\\:(a_2)",
                                  "    \\text{Quantity: Insufficient quantity of seed}\\:(a_3)",
                                  "    \\text{Form: Lack of seed treatments such as}\\
                                  \\text{pelleting or priming}\\:(a_4)",
                                  "    \\text{Price}\\:(a_5)",
                                  "    \\text{Buyer requirement: Processor (buyer) requires or}\\
                                  \\text{supplies varieties that are not available organically}\\:(a_6)",
                                  
                                  "\\text{Please indicate the extent to which you disagree\\ or agree with the following statements:}",
                                  "    \\text{Organic seed is important in maintaining\\ the integrity of organic food production}\\:(d_1)",
                                  "    \\text{Varieties bred for organic system management\\  are important to the overall success of organic\\ agriculture}\\:(d_2)"),
                  "Cronbach's alpha" = c("", "", "", "0.63",  "", "", "", 
                                         "","0.81",  ""), 
                  "Construction" = c(formulas))


tbl <- data.frame("Variables" = c("", "", "", "Ability", "", "","", "", "Duty", ""),
                  "Questions" = c("Over the last three years, how much were each of the following a factor in your decision NOT to purchase organic seed?",
                                  "    Availability: Variety is not available as organic (a_1)",
                                  "    Quality: Lack of desirable genetic traits (a_2)",
                                  "    Quantity: Insufficient quantity of seed (a_3)",
                                  "    Form: Lack of seed treatments such as pelleting or priming (a_4)",
                                  "    Price (a_5)",
                                  "    Buyer requirement: Processor/buyer requires or supplies varieties that are not available organically (a_6)",
                                  "Please indicate the extent to which you disagree or agree with the following statements:",
                                  "    Organic seed is important in maintaining the integrity of organic food production:(d_1)",
                                  "    Varieties bred for organic system management are important to the overall success of organic agriculture (d_2)"),
                  "Cronach's alpha" = c("", "", "", "0.63",  "", "", "", 
                                     "","0.81",  ""), 
                  "Construction" = c(formulas))

fontname = "Times"
ft <- tbl %>% 
  #as_grouped_data(groups = c("Variables")) %>% 
  flextable() %>% 
  #delete_rows(i = 2, part = "header") %>% 
  hline(i = (nrow(tbl)), part = "body",
        border = fp_border_default(color = "gray40", width = 1.5)) %>% 
  hline(i =7, part = "body",
        border = fp_border_default(color = "gray40", width = 1.5)) %>% 
  width(width =  c(1, 3, 1, 1)) %>%
  height(height = 0) #%>% 
  #add_footer_lines("Note: *p<0.1; **p<0.05; ***p<0.01") 

ft <- flextable::compose(
  x = ft, j = "Construction",
  value = as_paragraph(as_equation(Construction, width = 2, height = .5))
)
#ft <- flextable::compose(
#  x = ft, j = "Questions",
#  value = as_paragraph(as_equation(Questions, width = 2, height = .5))
#)
ft
ft %>% 
  flextable::footnote(i = 1, j = 2, value = as_paragraph("This scale was reversed to represent ability, so that lower scores suggest lower ability (e.g. 1 = significant factor (low ability) and 4 = not a factor (high ability))"), ref_symbols = ("a")) %>% 
  font(fontname = fontname, part = "body") %>% 
  save_as_docx(path = "tables/table1_variable_construction.docx")

# Appendix correlation table ----

cor(df.full[,c('OGVarietiesSuccessn')], df.full[,c('OGSeedOGIntegrityn')])


av_quant <- ltm::cronbach.alpha(df.full[,c('barrier_availability', 'barrier_insuffQuantity')],
                                na.rm = T)
av_quant <- round(av_quant$alpha, 2)
av_qual <- ltm::cronbach.alpha(df.full[,c('barrier_availability', 'barrier_lackdesiredTraits')],
                               na.rm = T)
av_qual <- round(av_qual$alpha, 2)
av_form <- ltm::cronbach.alpha(df.full[,c('barrier_availability',  'barrier_seedTreatlack')],
                               na.rm = T)
av_form <- round(av_form$alpha, 2)
av_price <- ltm::cronbach.alpha(df.full[,c('barrier_availability',  'barrier_price')],
                                na.rm = T)
av_price <- round(av_price$alpha, 2)
av_req <- ltm::cronbach.alpha(df.full[,c('barrier_availability',  'barrier_buyerReq')],
                              na.rm = T)
av_req <- round(av_req$alpha, 2)
quant_qual <- ltm::cronbach.alpha(df.full[,c('barrier_insuffQuantity', 'barrier_lackdesiredTraits')],
                                  na.rm = T)
quant_qual <- round(quant_qual$alpha, 2)
quant_form <- ltm::cronbach.alpha(df.full[,c('barrier_insuffQuantity', 'barrier_seedTreatlack')],
                                  na.rm = T)
quant_form <- round(quant_form$alpha, 2)
qual_form <- ltm::cronbach.alpha(df.full[,c('barrier_lackdesiredTraits', 'barrier_seedTreatlack')],
                                 na.rm = T)
qual_form <- round(qual_form$alpha, 2)
qual_price <- ltm::cronbach.alpha(df.full[,c('barrier_lackdesiredTraits', 'barrier_price')],
                                  na.rm = T)
qual_price <- round(qual_price$alpha, 2)
qual_req <- ltm::cronbach.alpha(df.full[,c('barrier_lackdesiredTraits', 'barrier_buyerReq')],
                                na.rm = T)
qual_req <- round(qual_req$alpha, 2)
quant_price <- ltm::cronbach.alpha(df.full[,c('barrier_insuffQuantity', 'barrier_price')],
                                   na.rm = T)
quant_price <- round(quant_price$alpha, 2)
quant_req <- ltm::cronbach.alpha(df.full[,c('barrier_insuffQuantity', 'barrier_buyerReq')],
                                 na.rm = T)
quant_req <- round(quant_req$alpha, 2)
form_price <- ltm::cronbach.alpha(df.full[,c('barrier_seedTreatlack', 'barrier_price')],
                                  na.rm = T)
form_price <- round(form_price$alpha, 2)
form_req <- ltm::cronbach.alpha(df.full[,c('barrier_seedTreatlack', 'barrier_buyerReq')],
                                na.rm = T)
form_req <- round(form_req$alpha, 2)
price_req <- ltm::cronbach.alpha(df.full[,c('barrier_price', 'barrier_buyerReq')],
                                 na.rm = T)
price_req <- round(price_req$alpha, 2)

cor_tbl <- data.frame("Barrier" = c(#"Availability", 
                                    "Quantity",  "Quality", "Form", "Price", "Buyer requirement"),
                      "Availability" = c(#"", 
                                         av_quant, av_qual, av_form, av_price, av_req),
                      "Quantity" = c(#av_quant, 
                                     "", quant_qual, quant_form, quant_price, quant_req),
                      "Quality" = c(#av_qual, 
                                    quant_qual, "", qual_form, qual_price, qual_req),
                      "Form" = c(#av_form, 
                                 quant_form, qual_form, "", form_price, form_req),
                      "Price" = c(#av_price, 
                                  quant_price, qual_price, form_price, "", price_req))
                      #"Buyer Requirement" = c(av_req, quant_req, qual_req, form_req, price_req, "")
cor_tbl[1,4:6] <- ""
cor_tbl[2,5:6] <- ""
cor_tbl[3,6] <- ""

write.csv(cor_tbl, 'tables/cor_table.csv', row.names = F)

# --- Full descriptives appendix ----

summary(df.full$OGSeedOGIntegrityn)
sd(df.full$OGSeedOGIntegrityn)
summary(df.full$OGVarietiesSuccessn)
sd(df.full$OGVarietiesSuccessn, na.rm = T)
df.full$barrier_availability_rev

rbind(round(c(summary(df.full$OG_value)[1:6], sd(df.full$OG_value)), 2),
      round(c(summary(df.full$OGSeedOGIntegrityn)[1:6], sd(df.full$OGSeedOGIntegrityn)), 2),
      round(c(summary(df.full$OGVarietiesSuccessn)[1:6], sd(df.full$OGVarietiesSuccessn, na.rm = T)), 2),
      round(c(summary(df.full$barrier_all_rev)[1:6], sd(df.full$barrier_all_rev)), 2),
      round(c(summary(df.full$barrier_availability_rev)[1:6], sd(df.full$barrier_availability_rev, na.rm = T)), 2))


# ---- Text summaries ----
summary(df.full$OGseed_percent_maincrop)
table(df.full$CertifierRequestn[df.full$IncreasedOGSeed != "Already at 100%" | df.full$OGseed_percent_maincrop != 100])
summary(df.full$CertifierRequestn[df.full$IncreasedOGSeed != "Already at 100%" | df.full$OGseed_percent_maincrop != 100])
summary(df.full$barrier_allowable)
summary(df.full$barrier_allowable_rev)
summary(df.full$barrier_availability_rev)
summary(df.full$barrier_lackdesiredTraits_rev)
summary(df.full$barrier_insuffQuantity_rev)
summary(df.full$barrier_seedTreatlack_rev)
summary(df.full$barrier_buyerReq_rev)
summary(df.full$barrier_distrustQuality_rev)
summary(df.full$noncommercial_seedsource)
summary(df.full$TotalAcreage)
summary(df.full$cropcat_n)
summary(df.full$YearsCertified)

### Additional steps ----
stopcol <- which(colnames(df.full) == "barrier_all")
get_add_steps <- function(add_step){
  tbl <- df %>% 
    select(colnames(df.full)[-c(stopcol:ncol(df))], {{add_step}}) %>% 
    filter(CertifierRequestn == 1) %>% 
    na.omit() %>% 
    group_by(survey_year, {{add_step}}) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(prop = round(100*(n/sum(n))), .by = survey_year) %>% 
    filter({{add_step}} == "Yes")
  return(tbl)
}
catalog <- get_add_steps(AddStepsCat)
seedfinder <- get_add_steps(AddStepsSeedFinder)
trial <- get_add_steps(AddStepsTrial)
#get_add_steps(AddStepsTimely)
contract <- get_add_steps(AddStepsContract)

# Table 2 Summary stats ----

variable_df <- data.frame(
  Questions = c("Proportion of acreage planted to organic seed",
                "Ability: Able to source organic seed", 
                "      Availability",
                "      Quality",
                "      Quantity",
                "      Form",
                "      Price",
                "      Buyer requirement",
                "Duty: Values organic seed",
                "      Integrity",
                "      Success",
                "Farm size (acres)",
                "      Vegetable Crop Acreage",
                "      Field Crop Acreage",
                "      Forage Crop Acreage",
                "Crop diversity",
                "Certifiers request organic seed use (%)",
                "      Research more than three seed catalogs",
                #"      Research the Organic Seed Finder website",
                "      Conduct trials of available organic varieties",
                "      Contract organic seed production",
                "Seed saved or traded (%)",
                "Years certified",
                "Number of respondents (complete data)"
  ),
                
  mean2011 = c(
    round(mean(df11$OGseed_percent_maincrop_prop,na.rm = T), 2),
    round(mean(df11$barrier_all_rev, na.rm = T), 2),
    round(mean(df11$barrier_availability_rev, na.rm = T), 2),
    round(mean(df11$barrier_lackdesiredTraits_rev, na.rm = T), 2),
    round(mean(df11$barrier_insuffQuantity_rev, na.rm = T), 2),
    round(mean(df11$barrier_seedTreatlack_rev, na.rm = T), 2),
    round(mean(df11$barrier_price_rev, na.rm = T), 2),
    round(mean(df11$barrier_buyerReq_rev, na.rm = T), 2),
    round(mean(df11$OG_value, na.rm = T), 2),
    round(mean(df11$OGSeedOGIntegrityn, na.rm = T), 2),
    round(mean(df11$OGVarietiesSuccessn, na.rm = T), 2),
    round(mean(df11$TotalAcreage[df11$TotalAcreage != 0], na.rm = T), 0),
    round(mean(df11$VegiAcreage[df11$VegiAcreage != 0], na.rm = T), 0),
    round(mean(df11$FieldAcreage[df11$FieldAcreage != 0], na.rm = T), 0),
    round(mean(df11$ForageAcreage[df11$ForageAcreage != 0], na.rm = T), 0),
    round(mean(df11$cropcat_n), 2),
    round(100*(table(df$CertifierRequest[df$survey_year == 2011])[2]/sum(table(df$IncreasedOGSeed[df$survey_year == 2011])[c(2,3)])), 0),
    catalog$prop[catalog$survey_year == 2011],
    #seedfinder$prop[seedfinder$survey_year == 2011],
    trial$prop[trial$survey_year == 2011],
    NA,
    round(mean(df11$noncommercial_seedsource, na.rm = T), 0),
    round(mean(df11$YearsCertified, na.rm = T), 0),
    nrow(df11)
  ),
           
  mean2016 = c(
    round(mean(df16$OGseed_percent_maincrop_prop,na.rm = T), 2),
    round(mean(df16$barrier_all_rev, na.rm = T), 2),
    round(mean(df16$barrier_availability_rev, na.rm = T), 2),
    round(mean(df16$barrier_lackdesiredTraits_rev, na.rm = T), 2),
    round(mean(df16$barrier_insuffQuantity_rev, na.rm = T), 2),
    round(mean(df16$barrier_seedTreatlack_rev, na.rm = T), 2),
    round(mean(df16$barrier_price_rev, na.rm = T), 2),
    round(mean(df16$barrier_buyerReq_rev, na.rm = T), 2),
    round(mean(df16$OG_value, na.rm = T), 2),
    round(mean(df16$OGSeedOGIntegrityn, na.rm = T), 2),
    round(mean(df16$OGVarietiesSuccessn, na.rm = T), 2),
    round(mean(df16$TotalAcreage[df16$TotalAcreage != 0], na.rm = T), 0),
    round(mean(df16$VegiAcreage[df16$VegiAcreage != 0], na.rm = T), 0),
    round(mean(df16$FieldAcreage[df16$FieldAcreage != 0], na.rm = T), 0),
    round(mean(df16$ForageAcreage[df16$ForageAcreage != 0], na.rm = T), 0),
    round(mean(df16$cropcat_n, na.rm = T), 2),
    round(100*(table(df$CertifierRequest[df$survey_year == 2016])[2]/sum(table(df$IncreasedOGSeed[df$survey_year == 2016])[c(2,3)])), 0),
    catalog$prop[catalog$survey_year == 2016],
    #seedfinder$prop[seedfinder$survey_year == 2016],
    trial$prop[trial$survey_year == 2016],
    contract$prop[contract$survey_year == 2016],
    round(mean(df16$noncommercial_seedsource, na.rm = T), 0),
    round(mean(df16$YearsCertified, na.rm = T), 0),
    nrow(df16)
  ),
  
  mean2021 = c(
    round(mean(df21$OGseed_percent_maincrop_prop,na.rm = T), 2),
    round(mean(df21$barrier_all_rev, na.rm = T), 2),
    round(mean(df21$barrier_availability_rev, na.rm = T), 2),
    round(mean(df21$barrier_lackdesiredTraits_rev, na.rm = T), 2),
    round(mean(df21$barrier_insuffQuantity_rev, na.rm = T), 2),
    round(mean(df21$barrier_seedTreatlack_rev, na.rm = T), 2),
    round(mean(df21$barrier_price_rev, na.rm = T), 2),
    round(mean(df21$barrier_buyerReq_rev, na.rm = T), 2),
    round(mean(df21$OG_value, na.rm = T), 2),
    round(mean(df21$OGSeedOGIntegrityn, na.rm = T), 2),
    round(mean(df21$OGVarietiesSuccessn, na.rm = T), 2),
    round(mean(df21$TotalAcreage[df21$TotalAcreage != 0], na.rm = T), 0),
    round(mean(df21$VegiAcreage[df21$VegiAcreage != 0], na.rm = T), 0),
    round(mean(df21$FieldAcreage[df21$FieldAcreage != 0], na.rm = T), 0),
    round(mean(df21$ForageAcreage[df21$ForageAcreage != 0], na.rm = T), 0),
    round(mean(df21$cropcat_n, na.rm = T), 2),
    round(100*(table(df$CertifierRequest[df$survey_year == 2021])[2]/sum(table(df$IncreasedOGSeed[df$survey_year == 2021])[c(2,3)])), 0),
    catalog$prop[catalog$survey_year == 2021],
    #seedfinder$prop[seedfinder$survey_year == 2021],
    trial$prop[trial$survey_year == 2021],
    contract$prop[contract$survey_year == 2021],
    round(mean(df21$noncommercial_seedsource, na.rm = T), 0),
    round(mean(df21$YearsCertified, na.rm = T), 0),
    nrow(df21)
  ),
  
    sd2011 = c(
      round(sd(df11$OGseed_percent_maincrop_prop,na.rm = T), 2),
      round(sd(df11$barrier_all_rev, na.rm = T), 2),
      round(sd(df11$barrier_availability_rev, na.rm = T), 2),
      round(sd(df11$barrier_lackdesiredTraits_rev, na.rm = T), 2),
      round(sd(df11$barrier_insuffQuantity_rev, na.rm = T), 2),
      round(sd(df11$barrier_seedTreatlack_rev, na.rm = T), 2),
      round(sd(df11$barrier_price_rev, na.rm = T), 2),
      round(sd(df11$barrier_buyerReq_rev, na.rm = T), 2),
      round(sd(df11$OG_value, na.rm = T), 2),
      round(sd(df11$OGSeedOGIntegrityn, na.rm = T), 2),
      round(sd(df11$OGVarietiesSuccessn, na.rm = T), 2),
      round(sd(df11$TotalAcreage[df11$TotalAcreage != 0], na.rm = T), 0),
      round(sd(df11$VegiAcreage[df11$VegiAcreage != 0], na.rm = T), 0),
      round(sd(df11$FieldAcreage[df11$FieldAcreage != 0], na.rm = T), 0),
      round(sd(df11$ForageAcreage[df11$ForageAcreage != 0], na.rm = T), 0),
      round(sd(df11$cropcat_n), 2),
      "","","","",#"",
      round(sd(df11$noncommercial_seedsource, na.rm = T), 0),
      round(sd(df11$YearsCertified, na.rm = T), 0),
      nrow(df11)
    ),
           
  sd2016 = c(
    round(sd(df16$OGseed_percent_maincrop_prop,na.rm = T), 2),
    round(sd(df16$barrier_all_rev, na.rm = T), 2),
    round(sd(df16$barrier_availability_rev, na.rm = T), 2),
    round(sd(df16$barrier_lackdesiredTraits_rev, na.rm = T), 2),
    round(sd(df16$barrier_insuffQuantity_rev, na.rm = T), 2),
    round(sd(df16$barrier_seedTreatlack_rev, na.rm = T), 2),
    round(sd(df16$barrier_price_rev, na.rm = T), 2),
    round(sd(df16$barrier_buyerReq_rev, na.rm = T), 2),
    round(sd(df16$OG_value, na.rm = T), 2),
    round(sd(df16$OGSeedOGIntegrityn, na.rm = T), 2),
    round(sd(df16$OGVarietiesSuccessn, na.rm = T), 2),
    round(sd(df16$TotalAcreage[df16$TotalAcreage != 0], na.rm = T), 0),
    round(sd(df16$VegiAcreage[df16$VegiAcreage != 0], na.rm = T), 0),
    round(sd(df16$FieldAcreage[df16$FieldAcreage != 0], na.rm = T), 0),
    round(sd(df16$ForageAcreage[df16$ForageAcreage != 0], na.rm = T), 0),
    round(sd(df16$cropcat_n, na.rm = T), 2),
    "","","","",#"",
    round(sd(df16$noncommercial_seedsource, na.rm = T), 0),
    round(sd(df16$YearsCertified, na.rm = T), 0),
    nrow(df16)
  ),
  
  sd2021 = c(
    round(sd(df21$OGseed_percent_maincrop_prop,na.rm = T), 2),
    round(sd(df21$barrier_all_rev, na.rm = T), 2),
    round(sd(df21$barrier_availability_rev, na.rm = T), 2),
    round(sd(df21$barrier_lackdesiredTraits_rev, na.rm = T), 2),
    round(sd(df21$barrier_insuffQuantity_rev, na.rm = T), 2),
    round(sd(df21$barrier_seedTreatlack_rev, na.rm = T), 2),
    round(sd(df21$barrier_price_rev, na.rm = T), 2),
    round(sd(df21$barrier_buyerReq_rev, na.rm = T), 2),
    round(sd(df21$OG_value, na.rm = T), 2),
    round(sd(df21$OGSeedOGIntegrityn, na.rm = T), 2),
    round(sd(df21$OGVarietiesSuccessn, na.rm = T), 2),
    round(sd(df21$TotalAcreage[df21$TotalAcreage != 0], na.rm = T), 0),
    round(sd(df21$VegiAcreage[df21$VegiAcreage != 0], na.rm = T), 0),
    round(sd(df21$FieldAcreage[df21$FieldAcreage != 0], na.rm = T), 0),
    round(sd(df21$ForageAcreage[df21$ForageAcreage != 0], na.rm = T), 0),
    round(sd(df21$cropcat_n, na.rm = T), 2),
    "","","","",#"",
    round(sd(df21$noncommercial_seedsource, na.rm = T), 0),
    round(sd(df21$YearsCertified, na.rm = T), 0),
    nrow(df21)
  )
)

variable_df$stats2011 <- paste0(variable_df$mean2011, " (",
                                variable_df$sd2011, ")")
variable_df$stats2016 <- paste0(variable_df$mean2016, " (",
                                variable_df$sd2016, ")")
variable_df$stats2021 <- paste0(variable_df$mean2021, " (",
                                variable_df$sd2021, ")")

variable_df <- select(variable_df, Questions, stats2011, stats2016, 
                      stats2021)

rowstop <- which(variable_df$Questions == "Certifiers request organic seed use (%)")
variable_df[rowstop,2:4] <- str_remove_all(variable_df[rowstop,2:4], ' \\(\\)')
variable_df[(rowstop+1),2:4] <- str_remove_all(variable_df[(rowstop+1),2:4], ' \\(\\)')
variable_df[(rowstop+2),2:4] <- str_remove_all(variable_df[(rowstop+2),2:4], ' \\(\\)')
variable_df[(rowstop+3),2:4] <- str_remove_all(variable_df[(rowstop+3),2:4], ' \\(\\)')
variable_df[(rowstop+4),2:4] <- str_remove_all(variable_df[(rowstop+4),2:4], ' \\(\\d{3}\\)')
variable_df[nrow(variable_df),2:4] <- str_remove_all(variable_df[nrow(variable_df),2:4], 
                                                     ' \\(.*\\)')

# Already at 100%
df11 <- filter(df, survey_year == 2011)
df16 <- filter(df, survey_year == 2016)
df21 <- filter(df, survey_year == 2021)
round(table(df11$IncreasedOGSeed)[1]/sum(table(df11$IncreasedOGSeed)[1:3]), 2)
round(table(df16$IncreasedOGSeed)[1]/sum(table(df16$IncreasedOGSeed)[1:3]), 2)
round(table(df21$IncreasedOGSeed)[1]/sum(table(df21$IncreasedOGSeed)[1:3]), 2)

write.csv(variable_df, "~/Documents/Davis/R-Projects/organicseed_adoption/tables/table2.csv", row.names = F)

# Table Appendix: Allocation ----

table(df.full$OG_value)
prop.table(table(df.full$OG_value))
prop.table(table(round(df.full$OG_value)))
summary(df.full$OG_value)
table(df.full$barrier_all_rev)
summary(df.full$barrier_all_rev)
table(df.full$OGseed_percent_maincrop == 100)
prop.table(table(df.full$OGseed_percent_maincrop == 100))

type_table = df.full %>% 
  mutate(type = case_when(
    OG_value <= 3 & barrier_all_rev >= 2 ~ "Low duty, high ability",
    OG_value > 3 & barrier_all_rev >= 2 ~ "High duty, high ability",
    OG_value <= 3 & barrier_all_rev < 2 ~ "Low duty, low ability",
    OG_value > 3 & barrier_all_rev < 2 ~ "High duty, low ability",
    T ~ NA)) %>% 
  group_by(type) %>% 
  summarize(count = n(),
            acreage_percent = round(mean(OGseed_percent_maincrop))) %>% 
  ungroup() %>% 
  mutate(percent_sample = 100*round((count / sum(count)),2))
type_table
# Typology ----
library(grid)
grid.newpage()
# I have to use this method of saving because I am blending ggplot and grob objects
jpeg("figures/table_typology.jpeg", width=5, height=4.2, 
     unit = 'in', res = 1000)
grid.lines(x = c(0.2,.9), y = c(.4))
grid.lines(x = c(.6), y = c(.1,.8))
grid.lines(x = c(0.3,.9), y = c(.7))
grid.lines(x = c(0.3,.9), y = c(.1))
grid.lines(x = c(.3), y = c(.1,.7))
grid.lines(x = c(.9), y = c(.1,.7))

grid.text("Ability", x = .15, y =.4, rot = 90,
          gp = gpar(fontsize = 10, fontfamily="Times"))
grid.text("Low (None) ", x = .25, y =.25, rot = 90,
          gp = gpar(fontsize = 10, fontfamily="Times"))
grid.text("High (> None)", x = .25, y =.55, rot = 90,
          gp = gpar(fontsize = 10, fontfamily="Times"))

grid.text("Duty", x = .6, y =.85, 
          gp = gpar(fontsize = 10, fontfamily="Times"))
grid.text("Low (<= Neutral)", x = .45, y =.75, 
          gp = gpar(fontsize = 10, fontfamily="Times"))
grid.text("High (> Neutral)", x = .75, y =.75, 
          gp = gpar(fontsize = 10, fontfamily="Times"))

grid.text(paste0("Operations:\n", type_table$percent_sample[3],
                 "% (n = ", type_table$count[3], 
                 ")\nAvg. organic seed use:\n",
                 type_table$acreage_percent[3],
                 "%"), x = .45, y =.55, 
          gp = gpar(fontsize = 10, fontfamily="Times"))
grid.text(paste0("Operations:\n", type_table$percent_sample[1],
                 "% (n = ", type_table$count[1], 
                 ")\nAvg. organic seed use:\n",
                 type_table$acreage_percent[1],
                 "%"), x = .75, y =.55, 
          gp = gpar(fontsize = 10, fontfamily="Times"))
grid.text(paste0("Operations:\n", type_table$percent_sample[4],
                 "% (n = ", type_table$count[4], 
                 ")\nAvg. organic seed use:\n",
                 type_table$acreage_percent[4],
                 "%"), x = .45, y =.25, 
          gp = gpar(fontsize = 10, fontfamily="Times"))
grid.text(paste0("Operations:\n", type_table$percent_sample[2],
                 "% (n = ", type_table$count[2], 
                 ")\nAvg. organic seed use:\n",
                 type_table$acreage_percent[2],
                 "%"), x = .75, y =.25, 
          gp = gpar(fontsize = 10, fontfamily="Times"))

grid.text("Table S8: Descriptive summaries of observed duty-ability typologies", 
          just = "left", x = .15, y =.925, 
          gp = gpar(fontsize = 10, fontfamily="Times"))

dev.off()
