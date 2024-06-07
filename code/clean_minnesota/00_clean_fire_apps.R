library(tidyverse)

minn_fire <- readxl::read_excel("data/minneapolis/Firefighter Cadet Applicants 2016-2022_13.43_Redacted.xlsx") %>% 
  janitor::clean_names()

minn_fire <- minn_fire %>% 
  mutate(date = as_date(date_time_received),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year)))

minn_fire %>% 
  write_csv("created_data/minnesota/fire_apps.csv")
