# Variable creation for funding and seed producers
## Start with the merged producer data
library(tidyverse)
df <- read.csv("~/OneDrive - University of Exeter/data/organicseed_adoption/merged_df_3panel_certifiers.csv")

df$VegiAcreage <- ifelse(df$VegiAcreage < 0, NA, df$VegiAcreage)
df$IncreasedOGVegSeedf <- as.numeric(factor(df$IncreasedOGVegSeed, 
                                            levels = c("Decreased the %",
                                                       "About the same %",
                                                       "Increased the %",
                                                       "Already at 100%")))
df$IncreasedOGFieldSeedf <- as.numeric(factor(df$IncreasedOGFieldSeed, 
                                              levels = c("Decreased the %",
                                                         "About the same %",
                                                         "Increased the %",
                                                         "Already at 100%")))
df$IncreasedOGForageSeedf <- as.numeric(factor(df$IncreasedOGForageSeed, 
                                               levels = c("Decreased the %",
                                                          "About the same %",
                                                          "Increased the %",
                                                          "Already at 100%")))
df$IncreasedOGSeed_avg <- round(rowMeans(df[, (ncol(df)-2):ncol(df)], na.rm = T), 0)
df$IncreasedOGSeed_avg <- ifelse(df$IncreasedOGSeed_avg == "NaN", NA, df$IncreasedOGSeed_avg)

# One way to think about main crop is to assign each producer to a single crop, or mix
# Last year how many organic acres did you have in annual vegetables. Please fill in number
df$VegiAcreage <- ifelse(is.na(df$VegiAcreage), 0 , df$VegiAcreage)
df$FieldAcreage <- ifelse(is.na(df$FieldAcreage), 0 , df$FieldAcreage)
df$ForageAcreage <- ifelse(is.na(df$ForageAcreage), 0 , df$ForageAcreage)

df$GrowVegis <- ifelse(df$VegiAcreage == 0 & is.na(df$GrowVegis), 0, df$GrowVegis)
df$GrowField <- ifelse(df$FieldAcreage == 0 & is.na(df$GrowField), 0, df$GrowField)
df$GrowForage <- ifelse(df$ForageAcreage == 0 & is.na(df$GrowForage), 0, df$GrowForage)

df$cropcat_n <- (df$GrowVegis + df$GrowField + df$GrowForage)/100
table(df$cropcat_n)

# Are these crops really unknown?
test <- filter(df, VegiAcreage == 0 & FieldAcreage == 0 & ForageAcreage == 0)
table(test$GrowField)
table(test$GrowForage)
table(test$GrowVegis)

table(is.na(df$GrowField))
table(is.na(df$GrowForage))
table(is.na(df$GrowVegis))

df$GrowVegis <- ifelse(df$VegiAcreage != 0 & is.na(df$GrowVegis), 100, df$GrowVegis)
df$GrowField <- ifelse(df$FieldAcreage != 0 & is.na(df$GrowField), 100, df$GrowField)
df$GrowForage <- ifelse(df$ForageAcreage != 0 & is.na(df$GrowForage), 100, df$GrowForage)

df <- df %>% 
  mutate(maincrop = case_when(
    # Making the distinction below because 2011 did not have cover crops
    survey_year %in% c(2016, 2021) & VegiAcreage/TotalAcreage > .26 ~ "Vegetables",
    survey_year %in% c(2016, 2021) & FieldAcreage/TotalAcreage > .26 ~ "Field",
    survey_year %in% c(2016, 2021) & ForageAcreage/TotalAcreage > .26 ~ "Forage",
    survey_year == 2011 & VegiAcreage/TotalAcreage > .34 ~ "Vegetables",
    survey_year == 2011 & FieldAcreage/TotalAcreage > .34 ~ "Field",
    survey_year == 2011 & ForageAcreage/TotalAcreage > .34 ~ "Forage",
    # There are some that have no acreage listed but say they grow this thing, so I am choosing in a random order since I cannot discern main; I think these get dropped anyways though because I cannot get their acreage?
    GrowForage == 100 ~ "Forage",
    GrowField == 100 ~ "Field",
    GrowVegis == 100 ~ "Vegetables", 
    VegiAcreage == 0 & FieldAcreage == 0 & ForageAcreage == 0 |
      GrowVegis == 0 & GrowField == 0 & GrowForage == 0 ~ "Unknown",
    T ~ "Mix"
  )) %>% 
  mutate(OGseed_percent_maincrop = case_when(
    maincrop == "Vegetables" ~ OGVegiSeed,
    maincrop == "Field" ~ OGFieldSeed,
    maincrop == "Forage" ~ OGForageSeed,
    T ~ (OGVegiSeed + OGFieldSeed + OGForageSeed)/3, 
  )) %>% 
  mutate(IncreasedOGSeed = case_when(
    survey_year != 2011 & maincrop == "Vegetables" ~ IncreasedOGVegSeed,
    survey_year != 2011 & maincrop == "Field" ~ IncreasedOGFieldSeed,
    survey_year != 2011 & maincrop == "Forage" ~ IncreasedOGForageSeed,
    survey_year != 2011 & maincrop == "Mix" ~ as.character(IncreasedOGSeed_avg),
    survey_year == 2011 ~ IncreasedOGSeed,
    T ~ IncreasedOGSeed
  )) %>% 
  mutate(IncreasedOGSeed = case_when(
    IncreasedOGSeed %in% c("4", "Already at 100%", 
                           "Have been using 100% certified organic seed for at least three years") ~ "Already at 100%",
    IncreasedOGSeed %in% c("3", "Increased the %", "Yes") ~ "Yes: Increased the %",
    IncreasedOGSeed %in% c("2", "About the same %", "No", "1", "Decreased the %") ~ "No: About the same % or decreased",
    IncreasedOGSeed == "" ~ NA_character_,
    T ~ IncreasedOGSeed
  ))
table(df$IncreasedOGSeed)


df$IncreasedOGSeed <- ifelse(df$OGseed_percent_maincrop == 100 & 
                               is.na(df$IncreasedOGSeed), "Already at 100%", 
                             df$IncreasedOGSeed)

table(df$IncreasedOGSeed)
table(df$maincrop)
table(df$maincrop, df$survey_year)

filter(df, survey_year == 2021) %>% 
  select(ForageAcreage, FieldAcreage, VegiAcreage, TotalAcreage, maincrop) %>% 
  filter(ForageAcreage != 0)


df <- df %>% 
  mutate(year_range = case_when(
    survey_year == 2011 ~ "2002-2008",
    survey_year == 2016 ~ "2009-2013",
    survey_year == 2021 ~ "2014-2018",
    T ~ NA_character_
  ))

# Removed the matched funds thing to regional_data script

## Creating a few new variables
df$OGSeedOGIntegrityf <- factor(df$OGSeedOGIntegrity, levels = c("Strongly disagree",
                                                                 "Disagree",
                                                                 "Neutral",
                                                                 "Agree",
                                                                 "Strongly agree"))
df$OGSeedOGIntegrityn <- as.numeric(df$OGSeedOGIntegrityf)

df$OGVarietiesSuccessf <- factor(df$OGVarietiesSuccess, levels = c("Strongly disagree",
                                                                 "Disagree",
                                                                 "Neutral",
                                                                 "Agree",
                                                                 "Strongly agree"))
df$OGVarietiesSuccessn <- as.numeric(df$OGVarietiesSuccessf)

df$CertifierRequestn <- ifelse(df$CertifierRequest == "No", 0,
                               ifelse(df$CertifierRequest == "Yes", 1, NA))

# Seed sources
df$noncommercial_seedsource <- df$SeedSourceFarm + df$SeedSourceOwn

## Lack of seed treatments, such as pelleting or priming
## Processor (buyer) requires or supplies varieties that are not available organically
## Insufficient quantity of seed
## Save my own seed
## Distrust of organic seed quality
## Price
## Specific variety not available as organic seed
## Lack of desirable genetic traits

# There are different levels
table(df$NoOGSeedTreat[df$survey_year == 2011])
table(df$NoOGSeedTreat[df$survey_year == 2016])
table(df$NoOGSeedTreat[df$survey_year == 2021])

df <- df %>% 
  mutate(NoOGSeedTreat = ifelse(NoOGSeedTreat == "More than moderate factor",
                                "Significant factor", NoOGSeedTreat),
         NoOGSeedProcessor = ifelse(NoOGSeedProcessor == "More than moderate factor",
                                "Significant factor", NoOGSeedProcessor),
         NoOGSeedQuant = ifelse(NoOGSeedQuant == "More than moderate factor",
                                "Significant factor", NoOGSeedQuant),
         NoOGSeedSave = ifelse(NoOGSeedSave == "More than moderate factor",
                                "Significant factor", NoOGSeedSave),
         NoOGSeedQual = ifelse(NoOGSeedQual == "More than moderate factor",
                                "Significant factor", NoOGSeedQual),
         NoOGSeedPrice = ifelse(NoOGSeedPrice == "More than moderate factor",
                                "Significant factor", NoOGSeedPrice),
         NoOGSeedVar = ifelse(NoOGSeedVar == "More than moderate factor",
                                "Significant factor", NoOGSeedVar),
         NoOGSeedTrait = ifelse(NoOGSeedTrait == "More than moderate factor",
                                "Significant factor", NoOGSeedTrait))

rmv <- c("No answer", "partial complete", "skipped due to branching", "")
levels_b <- c("Not a factor", "Slight factor", "Moderate factor", "Significant factor")

df <- df %>% 
  mutate(barrier_seedTreatlack = ifelse(NoOGSeedTreat %in% rmv, NA, 
                                        as.numeric(factor(NoOGSeedTreat, levels = levels_b))),
         barrier_buyerReq = ifelse(NoOGSeedProcessor %in% rmv, NA, 
                                   as.numeric(factor(NoOGSeedProcessor, levels = levels_b))),
         barrier_insuffQuantity = ifelse(NoOGSeedQuant %in% rmv, NA, 
                                         as.numeric(factor(NoOGSeedQuant, levels = levels_b))),
         barrier_saveOwn = ifelse(NoOGSeedSave %in% rmv, NA, 
                                  as.numeric(factor(NoOGSeedSave, levels = levels_b))),
         barrier_distrustQuality = ifelse(NoOGSeedQual %in% rmv, NA, 
                                          as.numeric(factor(NoOGSeedQual, levels = levels_b))),
         barrier_price = ifelse(NoOGSeedPrice %in% rmv, NA, 
                                as.numeric(factor(NoOGSeedPrice, levels = levels_b))),
         barrier_availability = ifelse(NoOGSeedVar %in% rmv, NA, 
                                       as.numeric(factor(NoOGSeedVar, levels = levels_b))),
         barrier_lackdesiredTraits = ifelse(NoOGSeedTrait %in% rmv, NA, 
                                            as.numeric(factor(NoOGSeedTrait, levels = levels_b))),
         barrier_text = NoOGSeedText)
df$barrier_seedTreatlackf = factor(df$barrier_seedTreatlack)
levels(df$barrier_seedTreatlackf) = levels_b
df$barrier_buyerReqf = factor(df$barrier_buyerReq)
levels(df$barrier_buyerReqf) = levels_b
df$barrier_insuffQuantityf = factor(df$barrier_insuffQuantity)
levels(df$barrier_insuffQuantityf) = levels_b
df$barrier_saveOwnf = factor(df$barrier_saveOwn)
levels(df$barrier_saveOwnf) = levels_b
df$barrier_distrustQualityf = factor(df$barrier_distrustQuality)
levels(df$barrier_distrustQualityf) = levels_b
df$barrier_pricef = factor(df$barrier_price)
levels(df$barrier_pricef) = levels_b
df$barrier_availabilityf = factor(df$barrier_availability)
levels(df$barrier_availabilityf) = levels_b
df$barrier_lackdesiredTraitsf = factor(df$barrier_lackdesiredTraits)
levels(df$barrier_lackdesiredTraitsf) = levels_b
  
table(df$NoOGSeedTreat)
table(df$barrier_seedTreatlack)
table(df$barrier_seedTreatlackf)
df <- select(df, -contains("NoOG"))

# Factor analysis of barriers ----
## So far this is not convincing
mat <- df %>% 
  filter(!is.na(RespondentID)) %>% 
  tibble::column_to_rownames("RespondentID") %>% 
  dplyr::select(barrier_seedTreatlack:barrier_lackdesiredTraits) #%>%
#map_dfr(~ ifelse(. == "Not applicable", NA, .)) #%>%  
#filter(. != "Not applicable") #   Getting rid of only all the NAs

ResponseId <- rownames(mat)

#mat <- mat %>% 
#  map_dfr(~ ifelse(is.na(.), "No response", .)) %>% # Then converting
#  map_dfr(~ factor(., levels =
#                     c("No response",
#                       "Not a factor",
#                       "Slight factor",
#                       "Moderate factor",
#                       "Significant factor"))) %>%
#  map_dfr(~ as.numeric(.))

nrow(mat[complete.cases(mat),])
mat.nona <- mat[complete.cases(mat),]

fa = stats::factanal(mat.nona, 
                     factors = 3, 
                     n.obs = nrow(mat.nona), 
                     rotation="varimax")
psy::scree.plot(fa$correlation) # looks like 3 or 4

fa = factanal(mat.nona, 
              factors = 3, 
              n.obs = length(mat.nona), 
              rotation="varimax")
print(fa, digits=2, cutoff=.2, sort=TRUE)

loadings <- as.data.frame(fa$loadings[1:8, 1:3])
loadings$max <- apply(X=loadings, MARGIN=1, FUN=which.max)

#Factor 1
f <- loadings %>% filter(max == 1) %>% 
  row.names() ; f
#ltm::cronbach.alpha(dplyr::select(mat, f), na.rm = T) 
f1 <- mat %>% select(f) %>% 
  mutate(barrier_f_traits = rowMeans(., na.rm = T)) %>% 
  select(barrier_f_traits)

#Factor 2
f <- loadings %>% filter(max == 2) %>% 
  row.names() ; f
ltm::cronbach.alpha(dplyr::select(mat, f), na.rm = T) # .57
f2 <- mat %>% select(f) %>% 
  mutate(barrier_f_availability = rowMeans(., na.rm = T)) %>% 
  select(barrier_f_availability)

#Factor 3
f <- loadings %>% filter(max == 3) %>% 
  row.names() ; f
ltm::cronbach.alpha(dplyr::select(mat, f), na.rm = T) # .39
f3 <- mat %>% select(f) %>% 
  mutate(barrier_f_misc = rowMeans(., na.rm = T)) %>% 
  select(barrier_f_misc)

# These are no good; not using them

df$growsVeg <- ifelse(is.na(df$VegiAcreage), 0, ifelse(df$VegiAcreage > 0, 1, 0))
growsveg <- df$RespondentID[df$growsVeg == 1]
df$growsField <- ifelse(is.na(df$FieldAcreage), 0, ifelse(df$FieldAcreage > 0, 1, 0))
growsfield <- df$RespondentID[df$growsField == 1]
df$growsForage <- ifelse(is.na(df$ForageAcreage), 0, ifelse(df$ForageAcreage > 0, 1, 0))
growsforage <- df$RespondentID[df$growsForage == 1]


# A second way is to lengthen the data so that each crop gets an observation, making repeats for crop type
df.long <- pivot_longer(df, cols = c(OGVegiSeed, OGFieldSeed, OGForageSeed),
                        names_to = "OGseed_type", values_to = "OGseed_percent")
# This removes the NAs, I think
df.long <- df.long[df.long$RespondentID %in% growsveg & df.long$OGseed_type == "OGVegiSeed" | 
                   df.long$RespondentID %in% growsfield & df.long$OGseed_type == "OGFieldSeed" | 
                   df.long$RespondentID %in% growsforage & df.long$OGseed_type == "OGForageSeed", ]

df$OGVegiSeed <- ifelse(is.na(df$OGVegiSeed), 0 , df$OGVegiSeed)
df$OGFieldSeed <- ifelse(is.na(df$OGFieldSeed), 0 , df$OGFieldSeed)
df$OGForageSeed <- ifelse(is.na(df$OGForageSeed), 0 , df$OGForageSeed)

df.long$OGcrop <- ifelse(stringr::str_detect(df.long$OGseed_type, "Vegi"), "Vegetables",
                  ifelse(stringr::str_detect(df.long$OGseed_type, "Forage"), "Forage",
                  ifelse(stringr::str_detect(df.long$OGseed_type, "Field"), "Field", NA)))

df.long$TotalCropAcreage <- ifelse(df.long$OGcrop == "Vegetables", df.long$VegiAcreage,
                            ifelse(df.long$OGcrop == "Forage", df.long$ForageAcreage,
                            ifelse(df.long$OGcrop == "Field", df.long$FieldAcreage, "missed")))

df.long <- df.long %>% 
  mutate(IncreasedOGSeed = case_when(
  OGcrop == "Vegetables" ~ IncreasedOGVegSeed,
  OGcrop == "Field" ~ IncreasedOGFieldSeed,
  OGcrop == "Forage" ~ IncreasedOGForageSeed,
  T ~ "Error"
)) %>% 
  mutate(IncreasedOGSeed = case_when(
    IncreasedOGSeed %in% c("4", "Already at 100%", 
                           "Have been using 100% certified organic seed for at least three years") ~ "Already at 100%",
    IncreasedOGSeed %in% c("3", "Increased the %", "Yes") ~ "Yes: Increased the %",
    IncreasedOGSeed %in% c("2", "About the same %", "No", "1", "Decreased the %") ~ "No: About the same % or decreased",
    IncreasedOGSeed == "" ~ NA_character_,
    T ~ IncreasedOGSeed
  ))

df.long$IncreasedOGSeed <- ifelse(df.long$OGseed_percent == 100 & 
                               is.na(df.long$IncreasedOGSeed), "Already at 100%", 
                             df.long$IncreasedOGSeed)

saveRDS(df, "~/Box/organicseed_adoption/df.rds")
saveRDS(df, "~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds")
saveRDS(df.long, "~/Box/organicseed_adoption/df_long.rds")
saveRDS(df.long, "~/OneDrive - University of Exeter/data/organicseed_adoption/df_long.rds")

