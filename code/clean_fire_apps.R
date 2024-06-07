library(tidyverse)

louisville_fire <- readxl::read_excel("data/louisville/louisville_fire_department_hired_apps/Fire Recruit Data.xlsx") %>% 
  janitor::clean_names()

louisville_fire <- louisville_fire %>% 
  mutate(date = as_date(date_received),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year)))

louisville_fire <- louisville_fire %>% 
  rename(race = text_what_is_your_race_ethnicity) %>% 
  mutate(race = case_when(
    race == "American Indian or Alaskan Native" ~ "Other",
    race == "Asian" ~ "Asian",
    race == "Black or African American" ~ "Black",
    race == "Do Not Wish To Disclose" ~ NA,
    race == "Hispanic or Latino" ~ "Hispanic", 
    race == "Native Hawaiian or Other Pacific Islander" ~ "Other",
    race == "White or Caucasian" ~ "White")) 


louisville_fire %>% 
  write_csv("created_data/louisville/fire_apps_cleaned.csv")
