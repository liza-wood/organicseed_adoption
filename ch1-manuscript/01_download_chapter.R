# download chapter from drive
googledrive::drive_download('https://docs.google.com/document/d/1T9K308unavSzrx7A4ZsXRTScazhfeAVHGT-rtawP1x4/edit',
                            path = 'ch1-manuscript/02_drive_manuscript.docx', overwrite = T)

# convert the word to text (could do markdown but it does weird things with the backticks)
rmarkdown::pandoc_convert('~/Documents/Davis/R-Projects/organicseed_adoption/ch1-manuscript/02_drive_manuscript.docx',
                          output = "~/Documents/Davis/R-Projects/organicseed_adoption/ch1-manuscript/02_drive_manuscript.txt", 
)

# Here I can manipulate the document
library(stringr)
txt <- readtext::readtext('~/Documents/Davis/R-Projects/organicseed_adoption/ch1-manuscript/02_drive_manuscript.txt')[,2]
# Start at header
start <- str_locate(txt, '#')
txt <- str_sub(txt, start[1], nchar(txt))
# Keep backticks for code chunks
txt <- str_replace_all(txt, '\\\\`\\\\`\\\\`', '```')

# And then make sure code within chunks doesn't have markup
chunks <- data.frame(str_locate_all(txt, '```'))
for(i in seq(1,nrow(chunks), 2)){
  if(i == 1){
    rpl <- str_remove_all(str_sub(txt, chunks$end[i], chunks$start[i+1]), '\\\\')
    rpl <- str_replace_all(rpl, '(?<!\\})\\n(?!`)', ' ')
    str_sub(txt, chunks$end[i], chunks$start[i+1]) <- rpl
  } else {
    newchunks <- data.frame(str_locate_all(txt, '```'))
    rpl <- str_remove_all(str_sub(txt, newchunks$end[i], newchunks$start[i+1]), '\\\\')
    rpl <- str_replace_all(rpl, '(?<!\\})\\n(?!`)', ' ')
    str_sub(txt, newchunks$end[i], newchunks$start[i+1]) <- rpl
  }
}

# Remove {.underline}
txt <- stringr::str_replace_all(txt, '\\{\\.underline\\}', '')
# Remove media files
media <- stringr::str_locate(txt, '\\!\\[\\]\\(media\\/image\\d\\.png\\)')
stringr::str_sub(txt, media[1]-50, media[2])
txt <- stringr::str_replace_all(txt, '\\!\\[\\]\\(media\\/image\\d\\.png\\)', '')

write.table(txt, '~/Documents/Davis/R-Projects/organicseed_adoption/ch1-manuscript/03_manuscript.Rmd', 
            col.names = F, row.names = F)
