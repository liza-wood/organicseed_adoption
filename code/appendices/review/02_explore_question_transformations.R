df11 <- read.csv("~/Dropbox/SOSInteractiveSurvey/2011SurveyData.csv")
df11_clean <- read.csv("~/Dropbox/SOSInteractiveSurvey/data2011_cleaned.csv")

df11$Last.year..what.approximate.percentage....of.total.acreage.of..field.crops.was.planted.with.certified.organic.seed...i..Please.fill.in.number....i.

table(df11$Last.year..what.approximate.percentage....of.total.acreage.of..field.crops.was.planted.with.certified.organic.seed...i..Please.fill.in.number....i.)
table(df11$Organic.seed.is.important.in.maintaining.the.integrity.of.organic.food.production.)

df11_clean$OGFieldSeed

df11_clean <- readRDS("~/Dropbox/SOSInteractiveSurvey/data2011_cleaned.rds") %>% 
  cbind(select(read.csv("~/Dropbox/SOSInteractiveSurvey/2011SurveyData.csv", skip = 2, header = T), 
               NoOGSeedTreat, NoOGSeedProcessor,     
               NoOGSeedQuant, NoOGSeedSave, NoOGSeedQual,          
               NoOGSeedPrice, NoOGSeedVar , NoOGSeedTrait, SeedInterest,
               AddStepsOther)) 
table(df11$Please.indicate.the.reasons.you.have.not.purchased.organic.seed.in.the.past...i..Please.indicate.one.response.per.reason.below....i.)
table(df11_clean$NoOGSeedTreat)
df21 <- read.csv("~/Dropbox/SOSInteractiveSurvey/data2021_cleaned.csv")
table(df21$NoOGSeedTreat)
