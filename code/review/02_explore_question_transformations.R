df11 <- read.csv("~/Dropbox/SOSInteractiveSurvey/2011SurveyData.csv")

df11$Last.year..what.approximate.percentage....of.total.acreage.of..field.crops.was.planted.with.certified.organic.seed...i..Please.fill.in.number....i.

table(df11$Last.year..what.approximate.percentage....of.total.acreage.of..field.crops.was.planted.with.certified.organic.seed...i..Please.fill.in.number....i.)


table(df11$Organic.seed.is.important.in.maintaining.the.integrity.of.organic.food.production.)
table(df11$Please.indicate.the.reasons.you.have.not.purchased.organic.seed.in.the.past...i..Please.indicate.one.response.per.reason.below....i.)
table(df11$X.28)

df11_clean <- readRDS("~/Dropbox/SOSInteractiveSurvey/data2011_cleaned.rds") %>% 
  cbind(select(read.csv("~/Dropbox/SOSInteractiveSurvey/2011SurveyData.csv", skip = 2, header = T), 
               NoOGSeedTreat, NoOGSeedProcessor,     
               NoOGSeedQuant, NoOGSeedSave, NoOGSeedQual,          
               NoOGSeedPrice, NoOGSeedVar , NoOGSeedTrait, SeedInterest,
               AddStepsOther)) 
table(df11$Please.indicate.the.reasons.you.have.not.purchased.organic.seed.in.the.past...i..Please.indicate.one.response.per.reason.below....i.)
table(df11_clean$NoOGSeedTreat)


df16 <- read.csv("~/Dropbox/SOSInteractiveSurvey/2016SurveyData.csv")
table(df16$For.each.reason.listed.below..how.much.of.a.reason.was.it.for.you.to..i.not.purchase..i..organic.seed.over.the.last.three.years..2011.2013....i..Please.indicate.one.response.per.reason.below....i.)
df21 <- read.csv("~/Dropbox/SOSInteractiveSurvey/data2021_cleaned.csv")
table(df21$NoOGSeedTreat)
