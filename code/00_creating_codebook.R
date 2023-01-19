setwd("~/Dropbox/SOSInteractiveSurvey/")
library(dplyr)
library(stringr)
df11 <- read.csv("2011SurveyData.csv")
row1 <- unlist(df11[1,])
row2 <- unlist(df11[2,])
codebook11 <- data.frame(Q = colnames(df11),
                         Roption = row1,
                         Rcode = row2)
rownames(codebook11) <- 1:nrow(codebook11)
codebook11$Q <- str_replace_all(codebook11$Q, "\\.", " ")
codebook11$Q <- str_replace_all(codebook11$Q, "^X", NA_character_)
codebook11 <- codebook11 %>%
  fill(Q, .direction = "down")

df16 <- read.csv("2016SurveyData.csv")
row1 <- unlist(df16[1,])
row2 <- unlist(df16[2,])
codebook16 <- data.frame(Q = colnames(df16),
                         Roption = row1,
                         Rcode = row2)
rownames(codebook16) <- 1:nrow(codebook16)
codebook16$Q <- str_replace_all(codebook16$Q, "\\.", " ")
codebook16$Q <- str_replace_all(codebook16$Q, "^X", NA_character_)
codebook16 <- codebook16 %>%
  fill(Q, .direction = "down")

df21 <- read.csv("NORA19_merged.csv")
# There is already a codebook for this
codebook21 <- read.csv("question_labels.csv") %>% select(Name, Label)

# These were pre-cleaned
df11f <- readRDS("data2011_cleaned.rds")
df16f <- readRDS("data2016_cleaned.rds")
df21f <- readRDS("data2021_cleaned.rds")

zipcode <- data.table::fread("zipcode.csv")
getzipcode <- zipcode$state
names(getzipcode) <- zipcode$zip
df21f$State <- unname(getzipcode[as.character(df21f[,"Q04A"])])
df21$State <- unname(getzipcode[as.character(df21[,"Q04A"])])

df11_combined <- data.frame(cleaned_colnames = colnames(df11f)) %>% 
  full_join(codebook11, by = c("cleaned_colnames" = "Rcode")) %>% 
  mutate(df11 = T)
df16_combined <- data.frame(cleaned_colnames = colnames(df16f)) %>% 
  full_join(codebook16, by = c("cleaned_colnames" = "Rcode")) %>% 
  mutate(df16 = T)
df21_combined <- data.frame(cleaned_colnames = colnames(df21f)) %>% 
  left_join(codebook21, by = c("cleaned_colnames" = "Name"))  %>% 
  mutate(df21 = T)

df <- full_join(df11_combined, df16_combined, by = "cleaned_colnames") %>% 
   full_join(df21_combined) %>% 
  select(cleaned_colnames, Q.x, Q.y, Label, df11, df16, df21)

df.full <- df %>% 
  filter(df11 == T & df16 == T & df21 == T)

setwd("~/Documents/Davis/R-Projects/organicseed_adoption/")
write.csv(df, "data/codebook_columns.csv", row.names = F)

# I really only did the first like 50 rows, so could do more
#manual <- read.csv("data/codebook_columns_manual.csv")
# The changes I did here were line up Label from 21 with the questions in 11 and 16
manual <- read.csv("data/codebook_columns_manual2.csv")
# if not NA in label and is NA in df21, get the colnames
newcols <- data.frame(newcols = ifelse(!is.na(manual$Label) & 
                                         is.na(manual$df21), manual$cleaned_colnames, NA),
                      labels = manual$Label) %>% filter(!is.na(newcols))

df21_combined <- filter(df21_combined, !is.na(cleaned_colnames))

for(i in 1:nrow(df21_combined)){
  for(j in 1:nrow(newcols)){
  df21_combined$cleaned_colnames[i] <- ifelse(is.na(df21_combined$Label[i]),
                                      df21_combined$cleaned_colnames[i],
                                   ifelse(df21_combined$Label[i] == newcols$labels[j], 
                                          newcols$newcols[j], df21_combined$cleaned_colnames[i]))
  }
}
df21_combined <- filter(df21_combined, !is.na(cleaned_colnames))

df.full <- full_join(df11_combined, df16_combined, by = "cleaned_colnames") %>% 
  full_join(df21_combined) %>% 
  left_join(codebook21) %>% 
  mutate(Name = case_when(
    is.na(Name) ~ cleaned_colnames,
    T ~ Name
  )) %>% 
  rename(qcode_11_16 = cleaned_colnames,
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
