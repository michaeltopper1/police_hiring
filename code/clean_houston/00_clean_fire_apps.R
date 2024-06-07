library(tidyverse)

jobs_houston <- read_csv("created_data/houston/non_police_apps.csv")

fire_houston <- jobs_houston %>% 
  filter(department_name == "Houston Fire Department" | department_name == "Fire Classified") %>% 
  mutate(job_title = job_title %>% str_to_lower()) %>% 
  filter(str_detect(job_title, "fire fighter|firefighter"))

fire_houston <- fire_houston %>% 
  mutate(date = as_date(application_date),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year)))

fire_houston <- fire_houston %>% 
  mutate(race = case_when(
    race == "American Indian or Alaska Native" ~ "Other",
    race == "Asian" ~ "Asian",
    race == "Black or African American" ~ "Black",
    race == "Hispanic or Latino" ~ "Hispanic",
    race == "Native Hawaiian or Other Pacific Islander" ~ "Other",
    race == "Not Answered" ~ NA,
    race == "Not Disclosed" ~ NA,
    race == "Two or More Races" ~ "Other",
    race == "White" ~ "White"
  )) 

fire_houston %>% 
  write_csv("created_data/houston/houston_fire_apps.csv")
