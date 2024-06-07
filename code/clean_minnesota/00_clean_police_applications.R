library(tidyverse)

minn <- readxl::read_excel("data/minneapolis/Police Officer Applicant Data 2016-2022_Public_Sent.xlsx") %>% 
  janitor::clean_names()

minn <- minn %>% 
  mutate(application_date =  my(date_received), .before = 1) %>% 
  mutate(date = as_date(application_date),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year))) 

minn <- minn %>% 
  mutate(race = case_when(
    race == "AMIND" ~ "Other",
    race == "ASIAN" ~ "Asian",
    race == "BLACK" ~ "Black",
    race == "HISPA" ~ "Hispanic",
    race == "WHITE" ~ "White"
  )) 

minn %>% 
  write_csv("created_data/minnesota/minnesota_police_apps.csv")
