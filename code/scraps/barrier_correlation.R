library(dplyr)
df <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds")

# Scale data ----
# Considering acreage as the dependent variable 
## Use c() when scaling numbers because it "strips" the dimension (i.e. scaling converts them into some kind of matrix and Stack Overflow told me that c would strip this)

df <- df %>% 
  filter(Region != "International", maincrop != "Other") %>% 
  select(RespondentID, 
         OGseed_percent_maincrop, 
         barrier_availability,
         barrier_insuffQuantity,
         barrier_lackdesiredTraits,
         barrier_seedTreatlack,
         #barrier_distrustQuality,
         OG_value,
         CertifierRequestn, 
         maincrop, survey_year, Region, 
         TotalAcreage, cropcat_n,
         noncommercial_seedsource
  ) %>% 
  na.omit()

cor_all <- df %>% 
  select(3:ncol(df)) %>% 
  corrr::correlate()

apply(cor_all, 2, function(x) {which(x > .4)})

summary(df$barrier_lackdesiredTraits)
summary(df$barrier_distrustQuality)

cor <- df %>% 
  select(contains('barrier')) %>% 
  corrr::correlate()
upper.tri(cor)
cor

bars <- cor$term

pval <- data.frame(matrix(nrow = 5, ncol = 4))
for(i in 1:4){
  for(j in 2:5){
    pval[j,i] <- cor.test(df[,bars[i]], df[,bars[j]])$p.value
  }
}


calc_ttest_p_value <- function(vec_a, vec_b){
  t.test(vec_a, vec_b)$p.value
}

corrr::colpair_map(select(df, contains('barrier')), calc_ttest_p_value)

