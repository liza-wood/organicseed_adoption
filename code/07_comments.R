library(tidyverse)
df <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds") %>% 
  mutate(barrier_allowable = purrr::pmap_dbl(list(barrier_availability,
                                                  barrier_insuffQuantity,
                                                  barrier_lackdesiredTraits,
                                                  barrier_seedTreatlack), max))
df_wtext <- df[!(df$barrier_text %in% c("","-1", "-2", "-3", "Skipped due to branching")) & !is.na(df$barrier_text) & df$barrier_text != "NA",]

summary(df_wtext$OG_value)
summary(df_wtext$barrier_allowable)
# High values, high barriers
df_wtext$barrier_text[df_wtext$OG_value == 5 & df_wtext$barrier_allowable >= 4]
## ""We only by non-organic seed when there is a tried and true variety that just is not available organic.  And this is usually after trialing other varieities to try to replace it.  The only other situation is when a specific company and regionally adapted strains that we know are unique and better."
## ""I grow vegetable, herb and flower starts for market and there are not many organic flower seeds available"
## ""Most commonly the seed is not available even when willing to pay huge shipping fees.    We are committed to using organic seed and will sometimes change crop in order to use organic seed." 
## ""I try very hard to source OG seed. Hybrid broccoli varieties are an exception. There is nothing in OG lines that performs as well...I continue to perform trials.     The OSA organic seed finder is very WEAK when it comes to forage/pasture crops. I had to buy OG seed from MN (Albert Lee) when much of it was produced in Oregon (said so when the bag arrived). Price was outrageous, but I paid it because I believe in OG seed." 
## ""I think being forced to buy Organic seed from Johnny's or Territorial when they admit out right that they do business with Monsanto and Symatis is like forcing me to sleep with Satan. The single best reason I can think of for not continuing to be Certified Organic!!! I want to support Baker Seeds and other heirloom providers, but each year Johnny's or Territorial has yet another of my seeds available organically so I'm forced to buy from them. In the end Monsanto will keep jacking the Organic seed prices up that I am forced to choose and then use Organic Farmers' nonprofitability as proof of our nonviability in the market place. To quote a computer: \" The only way out is not to play.\""
## ""I only use non-organic seed when I cannot source a comparable organic variety" 

df_wtext$RespondentID[df_wtext$barrier_text == "Most commonly the seed is not available even when willing to pay huge shipping fees.    We are committed to using organic seed and will sometimes change crop in order to use organic seed."]
df_wtext$Region[df_wtext$RespondentID == "3349744922"] # West
df_wtext$maincrop[df_wtext$RespondentID == "3349744922"] # Forage
df_wtext$OGseed_percent_maincrop[df_wtext$RespondentID == "3349744922"] # 75% acreage planted
df_wtext$OG_value[df_wtext$RespondentID == "3349744922"] # 5
df_wtext$barrier_availability[df_wtext$RespondentID == "3349744922"] # 4
df_wtext$barrier_insuffQuantity[df_wtext$RespondentID == "3349744922"] # 2
df_wtext$barrier_lackdesiredTraits[df_wtext$RespondentID == "3349744922"] # 1
df_wtext$barrier_seedTreatlack[df_wtext$RespondentID == "3349744922"] # 1
df_wtext$barrier_price[df_wtext$RespondentID == "3349744922"] # 1
df_wtext$barrier_buyerReq[df_wtext$RespondentID == "3349744922"] # 1

df_wtext$RespondentID[df_wtext$barrier_text == "I try very hard to source OG seed. Hybrid broccoli varieties are an exception. There is nothing in OG lines that performs as well...I continue to perform trials.     The OSA organic seed finder is very WEAK when it comes to forage/pasture crops. I had to buy OG seed from MN (Albert Lee) when much of it was produced in Oregon (said so when the bag arrived). Price was outrageous, but I paid it because I believe in OG seed."] 
df_wtext$OGseed_percent_maincrop[df_wtext$RespondentID == "3428933349"] # 0

df_wtext$RespondentID[df_wtext$barrier_text == "We only by non-organic seed when there is a tried and true variety that just is not available organic.  And this is usually after trialing other varieities to try to replace it.  The only other situation is when a specific company and regionally adapted strains that we know are unique and better."] 
df_wtext$OGseed_percent_maincrop[df_wtext$RespondentID == "927278666"] # 95
df_wtext$Region[df_wtext$RespondentID == "927278666"] # Northeast
df_wtext$maincrop[df_wtext$RespondentID == "927278666"] # Vegetables
df_wtext$OG_value[df_wtext$RespondentID == "927278666"] # 5
df_wtext$barrier_availability[df_wtext$RespondentID == "927278666"] # 4
df_wtext$barrier_insuffQuantity[df_wtext$RespondentID == "927278666"] # 2
df_wtext$barrier_lackdesiredTraits[df_wtext$RespondentID == "927278666"] # 3
df_wtext$barrier_seedTreatlack[df_wtext$RespondentID == "927278666"] # 1
df_wtext$barrier_price[df_wtext$RespondentID == "927278666"] # 1
df_wtext$barrier_buyerReq[df_wtext$RespondentID == "927278666"] # 1

df_wtext$RespondentID[df_wtext$barrier_text == "I think being forced to buy Organic seed from Johnny's or Territorial when they admit out right that they do business with Monsanto and Symatis is like forcing me to sleep with Satan. The single best reason I can think of for not continuing to be Certified Organic!!! I want to support Baker Seeds and other heirloom providers, but each year Johnny's or Territorial has yet another of my seeds available organically so I'm forced to buy from them. In the end Monsanto will keep jacking the Organic seed prices up that I am forced to choose and then use Organic Farmers' nonprofitability as proof of our nonviability in the market place. To quote a computer: \" The only way out is not to play.\""]
df_wtext$Region[df_wtext$RespondentID == "885790841"] # West
df_wtext$maincrop[df_wtext$RespondentID == "885790841"] # Vegetables
df_wtext$OGseed_percent_maincrop[df_wtext$RespondentID == "885790841"] # 3% acreage planted
df_wtext$OG_value[df_wtext$RespondentID == "885790841"] # 5
df_wtext$barrier_availability[df_wtext$RespondentID == "885790841"] # 4
df_wtext$barrier_insuffQuantity[df_wtext$RespondentID == "885790841"] # 4
df_wtext$barrier_lackdesiredTraits[df_wtext$RespondentID == "885790841"] # 1
df_wtext$barrier_seedTreatlack[df_wtext$RespondentID == "885790841"] # 3
df_wtext$barrier_price[df_wtext$RespondentID == "885790841"] # 4
df_wtext$barrier_buyerReq[df_wtext$RespondentID == "885790841"] # 1

# High values, low barriers
df_wtext$barrier_text[df_wtext$OG_value == 5 & df_wtext$barrier_allowable <= 2]
## ""Sometimes it is difficult to find organic seeds in the varieties we want, but we make do with what is available." 
## ""I purchased all organic seed but not happy with quality"
## Some take the terms very literal: ""As a certified organic producer, I am required to purchase or save organic seed, making this survey sorta useless for certified organic growers."
## ""Organic seed not available local.  Shipping is very expensive."

# Low values, high barriers - one form of shirking
df_wtext$barrier_text[df_wtext$OG_value <= 2 & df_wtext$barrier_allowable >= 4]
## "The varieties I grow are patented by Monsanto and other very large companies."
## "I produce Foundation and breeders classes of dry beans.  There has been no demand for organic seed"  
## "My certifier makes life difficult if I don't buy organic seed"
## "Isn't the difference of cert. organic versus untreated being stressed too much?  It seems hard enough to make ends meet without the extra cost considering the end results are the same?  Only my opinion!"
## "Largest obstacle is cost and lack of variety for genentic traits."

df_wtext$RespondentID[df_wtext$barrier_text == "The varieties I grow are patented by Monsanto and other very large companies."] 
df_wtext$OGseed_percent_maincrop[df_wtext$RespondentID == "1017024036"] # 0
df_wtext$Region[df_wtext$RespondentID == "1017024036"] # South
df_wtext$maincrop[df_wtext$RespondentID == "1017024036"] # Vegetables
df_wtext$OG_value[df_wtext$RespondentID == "1017024036"] # 2
df_wtext$barrier_availability[df_wtext$RespondentID == "1017024036"] # 4
df_wtext$barrier_insuffQuantity[df_wtext$RespondentID == "1017024036"] # 4
df_wtext$barrier_lackdesiredTraits[df_wtext$RespondentID == "1017024036"] # 4
df_wtext$barrier_seedTreatlack[df_wtext$RespondentID == "1017024036"] # 1
df_wtext$barrier_price[df_wtext$RespondentID == "1017024036"] # 4
df_wtext$barrier_buyerReq[df_wtext$RespondentID == "1017024036"] # 4

df_wtext$RespondentID[df_wtext$barrier_text == "Isn't the difference of cert. organic versus untreated being stressed too much?  It seems hard enough to make ends meet without the extra cost considering the end results are the same?  Only my opinion!"] 
df_wtext$OGseed_percent_maincrop[df_wtext$RespondentID == "952821412"] # 20
df_wtext$Region[df_wtext$RespondentID == "952821412"] # North Central
df_wtext$maincrop[df_wtext$RespondentID == "952821412"] # Vegetables
df_wtext$OG_value[df_wtext$RespondentID == "952821412"] # 2
df_wtext$barrier_availability[df_wtext$RespondentID == "952821412"] # 4
df_wtext$barrier_insuffQuantity[df_wtext$RespondentID == "952821412"] # 1
df_wtext$barrier_lackdesiredTraits[df_wtext$RespondentID == "952821412"] # 4
df_wtext$barrier_seedTreatlack[df_wtext$RespondentID == "952821412"] # 1
df_wtext$barrier_price[df_wtext$RespondentID == "952821412"] # 4
df_wtext$barrier_buyerReq[df_wtext$RespondentID == "952821412"] # 4

# Low values, low barriers - another form of shirking
df_wtext$barrier_text[df_wtext$OG_value <= 2 & df_wtext$barrier_allowable <= 2]
# "do not feel that using organic seed is worth the cost since untreated seeds does not degrade the value of finished (harvested) products."
df_wtext$OGseed_percent_maincrop[df_wtext$barrier_text == "do not feel that using organic seed is worth the cost since untreated seeds does not degrade the value of finished (harvested) products."]
df_wtext$barrier_allowable[df_wtext$barrier_text == "do not feel that using organic seed is worth the cost since untreated seeds does not degrade the value of finished (harvested) products."]
df_wtext$barrier_seedTreatlack[df_wtext$barrier_text == "do not feel that using organic seed is worth the cost since untreated seeds does not degrade the value of finished (harvested) products."]
df_wtext$barrier_availability[df_wtext$barrier_text == "do not feel that using organic seed is worth the cost since untreated seeds does not degrade the value of finished (harvested) products."]
df_wtext$barrier_price[df_wtext$barrier_text == "do not feel that using organic seed is worth the cost since untreated seeds does not degrade the value of finished (harvested) products."]
