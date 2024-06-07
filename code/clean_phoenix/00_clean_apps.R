library(tidyverse)
library(pdftools)

arizona <- pdf_text("data/arizona/Applicant.pdf")

phoenix <- arizona %>% 
  str_split("\n") %>% 
  unlist() %>% 
  as_tibble() %>% 
  extract(value, "applicant_id",
          "(\\d{6})", remove = F) %>% 
  extract(value, "date_applied",
          "(\\d{1,2}/\\d{1,2}/\\d{4})",
          remove = F) %>% 
  extract(value, "race",
          "(HISPA|ASIAN|BLACK|BLCK/WHT|WHITE|OTHRBLND|ASIAN/WHT|AMIND/WH|AMIND/BL|PACIF|NSPEC)",
          remove = F) %>% 
  extract(value, "posting_title",
          "(Police Recruit.{1,10}|Police Officer.{1,10})") 

## this filters to people who want to be a police officer, no lateral transfers. 
## do not want lateral transfers because this does not pick up what i want.
phoenix <- phoenix %>% 
  filter(str_detect(posting_title, "^Police Recruit\\s")) 

phoenix <- phoenix %>% 
  mutate(date_applied = mdy(date_applied),
          year = year(date_applied),
          month = month(date_applied),
          day = day(date_applied),
          year_month = mdy(paste0(month, "-1-", year))) %>% 
  filter(!if_all(everything(), ~is.na(.)))

phoenix %>% 
  write_csv("created_data/phoenix/phoenix_police_apps.csv")
  
  
