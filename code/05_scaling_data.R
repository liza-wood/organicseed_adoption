library(dplyr)
source("code/functions.R")
df <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds")

# Scale data ----
# Considering acreage as the dependent variable 
## Use c() when scaling numbers because it "strips" the dimension (i.e. scaling converts them into some kind of matrix and Stack Overflow told me that c would strip this)

dfs <- scale_data(df, OGseed_percent_maincrop)
saveRDS(dfs, "~/Box/organicseed_adoption/df_scaled.rds")
saveRDS(dfs, "~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled.rds")

dfs10 <- scale_data(df, OGseed_percent_maincrop, 2011)

saveRDS(dfs10, "~/Box/organicseed_adoption/df_scaled10.rds")
saveRDS(dfs10, "~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled10.rds")

dfs15 <- scale_data(df, OGseed_percent_maincrop, 2016)

saveRDS(dfs15, "~/Box/organicseed_adoption/df_scaled15.rds")
saveRDS(dfs15, "~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled15.rds")

dfs20 <- scale_data(df, OGseed_percent_maincrop, 2021)

saveRDS(dfs20, "~/Box/organicseed_adoption/df_scaled20.rds")
saveRDS(dfs20, "~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled20.rds")

dfs <- scale_data(df, IncreasedOGSeed)

saveRDS(dfs, "~/Box/organicseed_adoption/df_scaled_increase.rds")
saveRDS(dfs, "~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled_increase.rds")

dfs10 <- scale_data(df, IncreasedOGSeed, 2011)

saveRDS(dfs10, "~/Box/organicseed_adoption/df_scaled_increase10.rds")
saveRDS(dfs10, "~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled_increase10.rds")

dfs15 <- scale_data(df, IncreasedOGSeed, 2016)

saveRDS(dfs15, "~/Box/organicseed_adoption/df_scaled_increase15.rds")
saveRDS(dfs15, "~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled_increase15.rds")

dfs20 <- scale_data(df, IncreasedOGSeed, 2021)

saveRDS(dfs20, "~/Box/organicseed_adoption/df_scaled_increase20.rds")
saveRDS(dfs20, "~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled_increase20.rds")

