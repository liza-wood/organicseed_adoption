setwd("~/Dropbox/SOSInteractiveSurvey/")

# Basic email pattern
library(stringr)
email_pattern <- "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b"

# Find who had split operations in 2021
df21 <- read.csv("~/Dropbox/SOSInteractiveSurvey/NORA19_merged.csv")
omit_answers <- c("No answer", "no answer", "Partial complete", "partial complete",
                  "Skipped due to branching", "skipped due to branching")

df21_trans <- df21[!df21$Q03B_1 %in% omit_answers | 
                     !df21$Q03B_2 %in% omit_answers,]
df21_trans <- df21_trans[df21_trans$Q03C_1 != "0" &  df21_trans$Q03C_2 != "0",]
trans_ids <- df21_trans$RespID

df21_split <- df21[!df21$Q03C_1 %in% omit_answers | 
                     !df21$Q03C_2 %in% omit_answers,]
df21_split <- df21_split[df21_split$Q03C_1 != "0" &  df21_split$Q03C_2 != "0",]
split_ids <- df21_split$RespID

# Trans contacts
contact21a_trans <- df21_trans$Q45_1A[!(df21_trans$Q45_1A %in% c("No answer", "Partial complete", "Partial incomplete",
                                                                 "None", "N/A", "No", "N-A", "Have none", "Same as above",
                                                                 "Branching due to skip", "Skipped due to branching"))]
contact21b_trans <- df21_trans$Q45_2A[!(df21_trans$Q45_2A %in% c("No answer", "Partial complete", "Partial incomplete",
                                                                 "None", "N/A", "No", "N-A", "Have none", "Same as above",
                                                                 "Branching due to skip", "Skipped due to branching"))]
contact21c_trans <- df21_trans$Q45_2B[!(df21_trans$Q45_2B %in% c("No answer", "Partial complete", "Partial incomplete",
                                                                 "None", "N/A", "No", "N-A", "Have none", "Same as above",
                                                                 "Branching due to skip", "Skipped due to branching"))]
contact21d_trans <- df21_trans$Q45_3A[!(df21_trans$Q45_3A %in% c("No answer", "Partial complete", "Partial incomplete",
                                                                 "None", "N/A", "No", "N-A", "Have none", "Same as above",
                                                                 "Branching due to skip", "Skipped due to branching"))]

contacts21_trans <- unique(c(contact21a_trans, contact21b_trans, 
                             contact21c_trans, contact21d_trans))
contacts21_df_trans <- data.frame('entry' = contacts21_trans)
contacts21_df_trans$email <- tolower(str_extract(contacts21_df_trans$entry, email_pattern))
contacts21_df_trans$phone <- str_squish(str_remove_all(str_remove_all(contacts21_df_trans$entry, '\\-'), '\\s'))
contacts21_df_trans$phone <- str_extract(contacts21_df_trans$phone, '\\d{10}')

# Split contacts
contact21a_split <- df21_split$Q45_1A[!(df21_split$Q45_1A %in% c("No answer", "Partial complete", "Partial incomplete",
                                                           "None", "N/A", "No", "N-A", "Have none", "Same as above",
                                                           "Branching due to skip", "Skipped due to branching"))]
contact21b_split <- df21_split$Q45_2A[!(df21_split$Q45_2A %in% c("No answer", "Partial complete", "Partial incomplete",
                                                           "None", "N/A", "No", "N-A", "Have none", "Same as above",
                                                           "Branching due to skip", "Skipped due to branching"))]
contact21c_split <- df21_split$Q45_2B[!(df21_split$Q45_2B %in% c("No answer", "Partial complete", "Partial incomplete",
                                                           "None", "N/A", "No", "N-A", "Have none", "Same as above",
                                                           "Branching due to skip", "Skipped due to branching"))]
contact21d_split <- df21_split$Q45_3A[!(df21_split$Q45_3A %in% c("No answer", "Partial complete", "Partial incomplete",
                                                           "None", "N/A", "No", "N-A", "Have none", "Same as above",
                                                           "Branching due to skip", "Skipped due to branching"))]

contacts21_split <- unique(c(contact21a_split, contact21b_split, 
                             contact21c_split, contact21d_split))
contacts21_df_split <- data.frame('entry' = contacts21_split)
contacts21_df_split$email <- tolower(str_extract(contacts21_df_split$entry, email_pattern))
contacts21_df_split$phone <- str_squish(str_remove_all(str_remove_all(contacts21_df_split$entry, '\\-'), '\\s'))
contacts21_df_split$phone <- str_extract(contacts21_df_split$phone, '\\d{10}')


# Can I find anyone from a split operation in 2016 who was in 2019
df16 <- read.csv("2016SurveyData.csv")
contact16a <- df16$Email.Address[!(df16$Email.Address %in% c("", "Email Address", "EmailAddress"))]
contact16b <- df16[, 162]
contact16b  <- contact16b[!(contact16b %in% c("", "Open-Ended Response", "MailingList"))]
contacts16 <- unique(c(contact16a, contact16b))
contacts16_df <- data.frame('entry' = contacts16)
contacts16_df$email <- tolower(str_extract(contacts16_df$entry, email_pattern))
contacts16_df$phone <- str_squish(str_remove_all(str_remove_all(contacts16_df$entry, '\\-'), '\\s'))
contacts16_df$phone <- str_extract(contacts16_df$phone, '\\d{10}')


table(contacts21_df_split$email[!is.na(contacts21_df_split$email)] %in% 
        contacts16_df$email[!is.na(contacts16_df$email)])
table(contacts21_df_split$phone[!is.na(contacts21_df_split$phone)] %in% 
        contacts16_df$phone[!is.na(contacts16_df$phone)])

# No perfect matches, but anything else?
# farmerjeffolson@gmail.com == Jeff Olson?
colnames(df16)[162] <- 'contact'
df16$RespondentID[df16$contact == "Jeff Olson  2272 140  1st St.  Winfield, IA 52659"]
JO16 <- df16[df16$RespondentID == "3571160026",]

df21$RespID[df21$Q45_3A == "farmerjeffolson@gmail.com"]
41315213
JO21 <- df21[df21$RespID == "41315213",]

split_ids <- c('3571160026', '41315213')

table(contacts21_df_trans$email[!is.na(contacts21_df_trans$email)] %in% 
        contacts16_df$email[!is.na(contacts16_df$email)])

table(contacts21_df_trans$phone[!is.na(contacts21_df_trans$phone)] %in% 
        contacts16_df$phone[!is.na(contacts16_df$phone)])

trans_emails <- contacts21_df_trans$email[!is.na(contacts21_df_trans$email)][which(contacts21_df_trans$email[!is.na(contacts21_df_trans$email)] %in% 
                                                                     contacts16_df$email[!is.na(contacts16_df$email)])]

df16$RespondentID[str_which(tolower(df16$contact), paste0(trans_emails, collapse = "|"))]
ids21_1a <- df21$RespID[tolower(df21$Q45_1A) %in% trans_emails]
ids21_2a <- df21$RespID[tolower(df21$Q45_2A) %in% trans_emails]
ids21_1a <- df21$RespID[tolower(df21$Q45_1A) %in% trans_emails]
ids21_1a <- df21$RespID[tolower(df21$Q45_2A) %in% trans_emails]

JO21 <- df21[df21$RespID == "41315213",]



