# Variable creation for seed producers
## Start with the merged producer data
library(tidyverse)
df <- read.csv("~/OneDrive - University of Exeter/data/organicseed_adoption/merged_df_3panel.csv")

# Increased organic seed use ----
# First, make this increased question a factor number
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

# Crop types and main crop ----
#Last year  did you have any organic acres in... 
## JZ coded these as 0 or 100, rather than 0-1
table(df$GrowField)
table(df$GrowForage)
table(df$GrowVegis)

df$cropcat_n <- (df$GrowVegis + df$GrowField + df$GrowForage)/100
table(df$cropcat_n) # 0 = 508, 1 = 1575, 2 = 965, 3 = 142
table(is.na(df$cropcat_n)) # 283 NA

# For those that sum to 0, are these crops really unknown?
test <- filter(df, GrowField == 0 & GrowForage == 0 & GrowVegis == 0)
table(test$VegiAcreage)
table(test$FieldAcreage)
table(test$ForageAcreage)
# ^ No, some report acreage

# Replace NA with 0 if acreages is NA
df$VegiAcreage <- ifelse(is.na(df$VegiAcreage), 0 , df$VegiAcreage)
df$FieldAcreage <- ifelse(is.na(df$FieldAcreage), 0 , df$FieldAcreage)
df$ForageAcreage <- ifelse(is.na(df$ForageAcreage), 0 , df$ForageAcreage)

# And now update the grow variable (binary 0 or 100)
df$GrowVegis <- ifelse(df$VegiAcreage == 0 & 
                         (is.na(df$GrowVegis) | df$GrowVegis == 0), 0, 
                       ifelse(df$VegiAcreage != 0 &
                                (is.na(df$GrowVegis) | df$GrowVegis == 0), 100,
                              df$GrowVegis))
df$GrowField <- ifelse(df$FieldAcreage == 0 & 
                         (is.na(df$GrowField) | df$GrowField == 0), 0, 
                       ifelse(df$FieldAcreage != 0 &
                                (is.na(df$GrowField) | df$GrowField == 0), 100,
                              df$GrowField))
df$GrowForage <- ifelse(df$ForageAcreage == 0 & 
                         (is.na(df$GrowForage) | df$GrowForage == 0), 0, 
                       ifelse(df$ForageAcreage != 0 &
                                (is.na(df$GrowForage) | df$GrowForage == 0), 100,
                              df$GrowForage))

# Check again
df$cropcat_n <- (df$GrowVegis + df$GrowField + df$GrowForage)/100
table(df$cropcat_n) # 0 = 613, 1 = 1603, 2 = 1092, 3 = 165
table(is.na(df$cropcat_n)) # No NAs

# This still isn't quite right, so updating here with new variable names
# Usign only acreage reporting
df$growsVeg <- ifelse(is.na(df$VegiAcreage), 0, ifelse(df$VegiAcreage > 0, 1, 0))
df$growsField <- ifelse(is.na(df$FieldAcreage), 0, ifelse(df$FieldAcreage > 0, 1, 0))
df$growsForage <- ifelse(is.na(df$ForageAcreage), 0, ifelse(df$ForageAcreage > 0, 1, 0))

df$cropcat_n <- (df$growsVeg + df$growsField + df$growsForage)
table(df$cropcat_n) # 0 = 834, 1 = 1459, 2 = 1027, 3 = 153
table(is.na(df$cropcat_n)) # No NAs

# One way to think about main crop is to A) assign each producer to a single crop, or B) have repeated observations for each farmer-crop. Here I create the variable for A
# Last year how many organic acres did you have in [crop]. Please fill in number

# This just does not seem like the best way to do this
#df <- df %>% 
  #mutate(maincrop = case_when(
  #  # Making the distinction below because 2011 did not have cover crops
  #  survey_year %in% c(2016, 2021) & VegiAcreage/TotalAcreage > .26 ~ "Vegetables",
  #  survey_year %in% c(2016, 2021) & FieldAcreage/TotalAcreage > .26 ~ "Field",
  #  survey_year %in% c(2016, 2021) & ForageAcreage/TotalAcreage > .26 ~ "Forage",
  #  survey_year == 2011 & VegiAcreage/TotalAcreage > .34 ~ "Vegetables",
  #  survey_year == 2011 & FieldAcreage/TotalAcreage > .34 ~ "Field",
  #  survey_year == 2011 & ForageAcreage/TotalAcreage > .34 ~ "Forage",
    # There are some that have no acreage listed but say they grow this thing, so I am choosing in a random order since I cannot discern main;
  #  growsForage == 1 ~ "Forage",
  #  growsField == 1 ~ "Field",
  #  growsVeg == 1 ~ "Vegetables", 
  #  VegiAcreage == 0 & FieldAcreage == 0 & ForageAcreage == 0 |
  #    growsVeg == 0 & growsField == 0 & growsForage == 0 ~ "Unknown",
  #  T ~ "Mix"
  #)) %>% 

# Try this instead, looking for the maximum and the minimum
crops <- c("Vegetables", "Field", "Forage")
acre_cols <- which(colnames(df) %in% c('VegiAcreage', 'FieldAcreage', 'ForageAcreage'))
max_acres <- apply(df[,acre_cols], 1, function(x) max(x, na.rm = T))
min_acres <- apply(df[,acre_cols], 1, function(x) min(x[x!=0], na.rm = T))

maincrop <- c()
mincrop <- c()
for(i in 1:nrow(df)){
  if((df$VegiAcreage[i] == 0 & df$FieldAcreage[i] == 0 & df$ForageAcreage[i] == 0) |
     (df$growsVeg[i] == 0 & df$growsField[i] == 0 & df$growsForage[i] == 0)){
    maincrop[i] <- 'Unknown'
    mincrop[i] <- 'Unknown'
  } else if (max_acres[i] > 0) {
    max_col = which(df[i,acre_cols] == max_acres[i])
    if(length(max_col) > 1){
      max_col <- max_col[1]
    }
    maincrop[i] <- crops[max_col]
    
    min_col = which(df[i,acre_cols] == min_acres[i])
    if(length(min_col) > 1){
      min_col <- min_col[1]
    }
    mincrop[i] <- crops[min_col]
  } else if (df$growsForage[i] == 1){
    maincrop[i] <- "Forage"
    mincrop[i] <- "Forage"
  } else if (df$growsField[i] == 1){
    maincrop[i] <- "Field"
    mincrop[i] <- "Field"
  } else if (df$growsVeg[i] == 1){
    maincrop[i] <- "Vegetables"
    mincrop[i] <- "Vegetables"
  } else{
    maincrop[i] <- "Error"
    mincrop[i] <- "Error"
  }
}

table(maincrop)
table(mincrop)

df$maincrop <- maincrop
df$mincrop <- mincrop

df <- df %>% 
  mutate(OGseed_percent_maincrop = case_when(
    maincrop == "Vegetables" ~ OGVegiSeed,
    maincrop == "Field" ~ OGFieldSeed,
    maincrop == "Forage" ~ OGForageSeed,
    T ~ (OGVegiSeed + OGFieldSeed + OGForageSeed)/3, 
  )) %>% 
  mutate(OGseed_percent_mincrop = case_when(
    mincrop == "Vegetables" ~ OGVegiSeed,
    mincrop == "Field" ~ OGFieldSeed,
    mincrop == "Forage" ~ OGForageSeed,
    T ~ (OGVegiSeed + OGFieldSeed + OGForageSeed)/3, 
  )) %>% 
  mutate(TotalAcreage_maincrop = case_when(
    maincrop == "Vegetables" ~ VegiAcreage,
    maincrop == "Field" ~ FieldAcreage,
    maincrop == "Forage" ~ ForageAcreage,
    T ~ (VegiAcreage + FieldAcreage + ForageAcreage)/3, 
  )) %>% 
  mutate(TotalAcreage_mincrop = case_when(
    mincrop == "Vegetables" ~ VegiAcreage,
    mincrop == "Field" ~ FieldAcreage,
    mincrop == "Forage" ~ ForageAcreage,
    T ~ (VegiAcreage + FieldAcreage + ForageAcreage)/3, 
  )) %>% 
  mutate(IncreasedOGSeed = case_when(
    survey_year != 2011 & maincrop == "Vegetables" ~ IncreasedOGVegSeed,
    survey_year != 2011 & maincrop == "Field" ~ IncreasedOGFieldSeed,
    survey_year != 2011 & maincrop == "Forage" ~ IncreasedOGForageSeed,
    survey_year != 2011 & maincrop == "Mix" ~ as.character(IncreasedOGSeed_avg),
    # This icnreased variable is not crop specific in 2011
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
table(df$maincrop)

df$IncreasedOGSeed <- ifelse(df$OGseed_percent_maincrop == 100 & 
                               is.na(df$IncreasedOGSeed), "Already at 100%", 
                             df$IncreasedOGSeed)

# This is for another project, but keep for now
df <- df %>% 
  mutate(year_range = case_when(
    survey_year == 2011 ~ "2002-2008",
    survey_year == 2016 ~ "2009-2013",
    survey_year == 2021 ~ "2014-2018",
    T ~ NA_character_
  ))

# Duty ----
## Checking consistency in question wording across years: Duty
### This one seems like integrity question was always 5 point
table(df$OGSeedOGIntegrity)
table(df$OGSeedOGIntegrity, df$survey_year)

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

## Checking integrity to build construct?
cor(df$OGVarietiesSuccessn, df$OGSeedOGIntegrityn, use = "complete.obs")
#df$CropsNeedBreedn <- as.numeric(factor(df$CropsNeedBreed, c("No", "Yes")))
#cor(df$OGVarietiesSuccessn, df$CropsNeedBreedn, use = "complete.obs")
#cor(df$OGSeedOGIntegrityn, df$CropsNeedBreedn, use = "complete.obs")

ltm::cronbach.alpha(df[,c('OGVarietiesSuccessn', 'OGSeedOGIntegrityn')],
                    na.rm = T)

df$OG_value <- rowMeans(df[,c('OGVarietiesSuccessn', 'OGSeedOGIntegrityn')], na.rm = T)


# Certifier request ----
df$CertifierRequestn <- ifelse(df$CertifierRequest == "No", 0,
                               ifelse(df$CertifierRequest == "Yes", 1, NA))

# There are loads who didn't answer this question but have 100%, so I will say already 100
length(df$OGseed_percent_maincrop[is.na(df$CertifierRequest) & 
                                    !is.na(df$OGseed_percent_maincrop) &
                                    df$OGseed_percent_maincrop >= 98])
df$CertifierRequestf <- ifelse(df$OGseed_percent_maincrop >= 98, "Fully compliant",
                               df$CertifierRequest)

# Seed sources ----
df$noncommercial_seedsource <- df$SeedSourceFarm + df$SeedSourceOwn

# Ability / barriers ----
## Lack of seed treatments, such as pelleting or priming
## Processor (buyer) requires or supplies varieties that are not available organically
## Insufficient quantity of seed
## Save my own seed
## Distrust of organic seed quality
## Price
## Specific variety not available as organic seed
## Lack of desirable genetic traits

## Checking consistency in question wording across years: Ability
## Clearly barrier was 5 point in 2011 and 4 point in others, but more than moderate was left zero...
table(df$NoOGSeedQual)
table(df$NoOGSeedQual, df$survey_year)

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

# Exploratory factor analysis of barriers ----
## So far this is not convincing
mat <- df %>% 
  filter(!is.na(RespondentID)) %>% 
  tibble::column_to_rownames("RespondentID") %>% 
  dplyr::select(barrier_seedTreatlack:barrier_lackdesiredTraits) 

ResponseId <- rownames(mat)

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
ltm::cronbach.alpha(dplyr::select(mat, f), na.rm = T) # .40
f3 <- mat %>% select(f) %>% 
  mutate(barrier_f_misc = rowMeans(., na.rm = T)) %>% 
  select(barrier_f_misc)


saveRDS(df, "~/Box/organicseed_adoption/df.rds")
saveRDS(df, "~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds")

# Lengthening for multiple crops per grower ----
growsveg <- df$RespondentID[df$growsVeg == 1]
growsfield <- df$RespondentID[df$growsField == 1]
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

saveRDS(df.long, "~/Box/organicseed_adoption/df_long.rds")
saveRDS(df.long, "~/OneDrive - University of Exeter/data/organicseed_adoption/df_long.rds")

