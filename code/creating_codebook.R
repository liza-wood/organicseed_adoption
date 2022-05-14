setwd("~/Dropbox/SOSInteractiveSurvey/")
library(dplyr)
library(stringr)
df11 <- read.csv("2011SurveyData.csv")
row2 <- unlist(df11[2,])
codebook11 <- data.frame(Q = colnames(df11),
                         code = row2)
rownames(codebook11) <- 1:nrow(codebook11)
codebook11$Q_readble <- str_replace_all(codebook11$Q, "\\.", " ")
df16 <- read.csv("2016SurveyData.csv")
row2 <- unlist(df16[2,])
codebook16 <- data.frame(Q = colnames(df16),
                         code = row2)
rownames(codebook16) <- 1:nrow(codebook16)
codebook16$Q_readble <- str_replace_all(codebook16$Q, "\\.", " ")
df21 <- read.csv("NORA19_merged.csv")
# There is already a codebook for this
codebook21 <- read.csv("question_labels.csv") %>% select(Name, Label)

df11f <- readRDS("data2011_cleaned.rds")
df16f <- readRDS("data2016_cleaned.rds")
df21f <- readRDS("data2021_cleaned.rds")

zipcode <- data.table::fread("zipcode.csv")
getzipcode <- zipcode$state
names(getzipcode) <- zipcode$zip
df21f$State <- unname(getzipcode[as.character(df21f[,"Q04A"])])
df21$State <- unname(getzipcode[as.character(df21[,"Q04A"])])

df11_combined <- data.frame(colnames = colnames(df11f), df11 = T) %>% 
  left_join(codebook11, by = c("colnames" = "code"))
df16_combined <- data.frame(colnames = colnames(df16f), df16 = T) %>% 
  left_join(codebook16, by = c("colnames" = "code"))
df21_combined <- data.frame(colnames = colnames(df21f), df21 = T) %>% 
  left_join(codebook21, by = c("colnames" = "Name")) 

df <- full_join(df11_combined, df16_combined, by = "colnames") %>% 
   full_join(df21_combined) %>% 
  select(colnames, Q.x, Q.y, Label, df11, df16, df21)

df.full <- df %>% 
  filter(df11 == T & df16 == T & df21 == T)

setwd("~/Documents/Davis/R-Projects/organicseed/")
write.csv(df, "data/codebook_columns.csv", row.names = F)

# I really only did the first like 50 rows, so could do more
manual <- read.csv("data/codebook_columns_manual.csv")

# if not NA in label and is NA in df21, get the colnames
newcols <- data.frame(newcols = ifelse(!is.na(manual$Label) & 
                                         is.na(manual$df21), manual$colnames, NA),
                      labels = manual$Label) %>% filter(!is.na(newcols))

df21_combined <- filter(df21_combined, !is.na(colnames))

for(i in 1:nrow(df21_combined)){
  for(j in 1:nrow(newcols)){
  df21_combined$colnames[i] <- ifelse(is.na(df21_combined$Label[i]),
                                      df21_combined$colnames[i],
                                   ifelse(df21_combined$Label[i] == newcols$labels[j], 
                                          newcols$newcols[j], df21_combined$colnames[i]))
  }
}
df21_combined <- filter(df21_combined, !is.na(colnames))

df.full <- full_join(df11_combined, df16_combined, by = "colnames") %>% 
  full_join(df21_combined) %>% 
  left_join(codebook21) %>% 
  mutate(Name = case_when(
    is.na(Name) ~ colnames,
    T ~ Name
  )) %>% 
  rename(qcode_11_16 = colnames,
         qcode_21 = Name,
         qfull_11 = Q.x,
         qfull_16 = Q.y,
         qfull_21 = Label)

df.full.3panel <- df.full %>% 
  filter(df11 == T & df16 == T & df21 == T) %>% 
  select(qcode_11_16, qcode_21, qfull_11, qfull_16, qfull_21)

write.csv(df.full.3panel, "data/codebook_3panel.csv", row.names = F)

df.full.2panel <- df.full %>% 
  filter(df11 == T & df16 == T) %>% 
  select(qcode_11_16, qfull_11, qfull_16)

write.csv(df.full.2panel, "data/codebook_2panel.csv", row.names = F)
