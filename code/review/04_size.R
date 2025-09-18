# Farm size and crop diversity?
model_ids <- readRDS('data/model_ids.RDS')
df.full <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled.rds") %>% 
  select(TotalAcreage,
         TotalAcreage_maincrop,
         cropcat_n,
         OGseed_percent_maincrop_prop,
         OG_value,
         OG_value_s,
         OG_value_minus1,
         barrier_all_rev_s,
         barrier_all_rev_minus1,
         CertifierRequestn,
         noncommercial_seedsource_s,
         acreage_log,
         acreage_log_s,
         cropcat_n_s,
         years_certified_s,
         maincrop,
         Region,
         survey_year,
         RespondentID) %>% 
  na.omit()

acres_cat7 <- c("1 to 9", "10 to 49", "50 to 179", "180 to 499",
                "500 to 999", "1,000 to 1,999", "2,000 +")

df.full <- df.full %>% 
  mutate(acreage_range = case_when(
    TotalAcreage <10 ~ acres_cat7[1],
    TotalAcreage >= 10 & TotalAcreage < 50 ~ acres_cat7[2],
    TotalAcreage >= 50 & TotalAcreage < 180 ~ acres_cat7[3],
    TotalAcreage >= 180 & TotalAcreage < 500~ acres_cat7[4],
    TotalAcreage >= 500 & TotalAcreage < 1000 ~ acres_cat7[5],
    TotalAcreage >= 1000 & TotalAcreage < 2000 ~ acres_cat7[6],
    TotalAcreage >= 2000 ~ acres_cat7[7],
    T ~ NA
  )) %>% 
  mutate(crops_per_acre = cropcat_n/TotalAcreage,
         crops_per_log_acre = cropcat_n/(acreage_log+0.001))

df.full %>% 
  group_by(acreage_range) %>% 
  summarize(mean(crops_per_acre),
            mean(crops_per_log_acre))

ggplot(df.full, aes(y = factor(cropcat_n), x = acreage_log)) +
  geom_boxplot()

ggplot(df.full, aes(y = cropcat_n, x = acreage_log)) +
  geom_point() +
  geom_smooth(method = 'lm')

ggplot(df.full, aes(y = factor(cropcat_n), x = log(TotalAcreage_maincrop))) +
  geom_boxplot()

ggplot(df.full, aes(y = cropcat_n, x = log(TotalAcreage_maincrop))) +
  geom_point() +
  geom_smooth(method = 'lm')

# Farm size and duty
ggplot(df.full, aes(y = OG_value, x = acreage_log)) +
  geom_point() +
  geom_smooth(method = 'lm')

# Diversity and duty
ggplot(df.full, aes(y = factor(OG_value), x = cropcat_n)) +
  geom_boxplot() 
ggplot(df.full, aes(y = OG_value, x = cropcat_n)) +
  geom_point() +
  geom_smooth(method = 'lm')

source('code/functions.R')
df <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df.rds") %>% 
  filter(RespondentID %in% model_ids)
table(df$maincrop)
round(prop.table(table(df$maincrop)), 2)
table(df$mincrop)
round(prop.table(table(df$mincrop)), 2)
table(df$cropcat_n)
round(prop.table(table(df$cropcat_n)), 2)
dfs <- scale_data(df, OGseed_percent_mincrop) %>% 
  select(OGseed_percent_mincrop_prop,
         OG_value_s,
         OG_value_minus1,
         barrier_all_rev_s,
         barrier_all_rev_minus1,
         CertifierRequestn,
         noncommercial_seedsource_s,
         acreage_log_s,
         acreage_log_mincrop_s,
         cropcat_n_s,
         years_certified_s,
         mincrop,
         maincrop,
         Region,
         survey_year,
         RespondentID) 

df.full_rounded <- dfs
s = 0.5
n = nrow(dfs)
x = df.full_rounded$OGseed_percent_mincrop_prop
rounded = (x*(n-1)+s)/n
summary(rounded)

df.full_rounded$OGseed_percent_mincrop_prop <- (df.full_rounded$OGseed_percent_mincrop_prop * (n-1) + s)/n

m_ix <- betareg(OGseed_percent_mincrop_prop ~ 
                  barrier_all_rev_s +
                  OG_value_s + 
                  barrier_all_rev_s*OG_value_s +
                  CertifierRequestn + 
                  acreage_log_mincrop_s + 
                  cropcat_n_s +
                  noncommercial_seedsource_s +
                  years_certified_s + 
                  mincrop +
                  Region +
                  survey_year,
                data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m_ix)

min_df <- prep_betreg_data(m_ix)
dat_list <- list(min_df)
nmodels <- length(dat_list)
mdf_list <- list()
for(i in 1:nmodels){
  mdf <- dat_list[[i]]
  mdf$Estimate <- round(mdf$mean, 2)
  mdf$Estimate <- ifelse(mdf$Pr...z.. < 0.01, paste0(mdf$Estimate, '***'),
                         ifelse(mdf$Pr...z.. < 0.05, paste0(mdf$Estimate, '**'),
                                ifelse(mdf$Pr...z.. < 0.1, paste0(mdf$Estimate, '*'),
                                       mdf$Estimate)))
  colnames(mdf)[which(stringr::str_detect(colnames(mdf), 'Estimate'))] <- paste0("Estimate", i)
  mdf_list[[i]] <- mdf
}

table_min <- data.frame('term' = mdf_list[[nmodels]]$term)
table_min <- left_join(table_min, mdf_list[[1]][,c('term', 'Estimate1')])

table_min <- table_min %>% 
  mutate(term = factor(term, rev(coef_order))) %>% 
  arrange(rev(term)) %>% 
  mutate(term = as.character(term)) %>% 
  rbind(c("Observations:", m_ix$nobs)) %>% 
  rbind(c("R2", round(m_ix$pseudo.r.squared,3))) %>% 
  mutate(Variables = c(rep('Main effects and interaction', 3), rep('Controls', (nrow(.)-3))))

table_min %>% 
  as_grouped_data(groups = c("Variables")) %>% 
  as_flextable() %>% 
  hline(i = (nrow(mdf_ix)+2), part = "body",
        border = fp_border_default(color = "gray40", width = 1.5)) %>% 
  hline(i =c(1,4,5), part = "body",
        border = fp_border_default(color = "gray40", width = 1.5)) %>% 
  width(width =  c(3,rep(0.8, nmodels))) %>%
  height(height = 0) %>% 
  add_footer_lines("Note: *p<0.1; **p<0.05; ***p<0.01") %>% 
  save_as_docx(path = "tables/model_table_mincrop.docx")
write.csv(table_min, 'tables/reviewer_response_model_mincrop.csv', row.names = F)


df.full <- readRDS("~/OneDrive - University of Exeter/data/organicseed_adoption/df_scaled.rds") %>% 
  select(OGseed_percent_maincrop_prop,
         OG_value_s,
         OG_value_minus1,
         barrier_all_rev_s,
         barrier_all_rev_minus1,
         CertifierRequestn,
         noncommercial_seedsource_s,
         acreage_log_maincrop_s,
         cropcat_n_s,
         years_certified_s,
         maincrop,
         Region,
         survey_year,
         RespondentID) %>% 
  na.omit()

df.full_rounded <- df.full
s = 0.5
n = nrow(df.full)
x = df.full_rounded$OGseed_percent_maincrop_prop
rounded = (x*(n-1)+s)/n
summary(rounded)

df.full_rounded$OGseed_percent_maincrop_prop <- (df.full_rounded$OGseed_percent_maincrop_prop * (n-1) + s)/n

m_ix <- betareg(OGseed_percent_maincrop_prop ~ 
                  barrier_all_rev_s +
                  OG_value_s + 
                  barrier_all_rev_s*OG_value_s +
                  CertifierRequestn + 
                  acreage_log_maincrop_s + 
                  cropcat_n_s +
                  noncommercial_seedsource_s +
                  years_certified_s + 
                  maincrop +
                  Region +
                  survey_year,
                data = df.full_rounded, link = "logit", link.phi = NULL, type = "ML")
summary(m_ix)
