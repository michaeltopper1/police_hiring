library(tidyverse)

min_hires <- readxl::read_excel("data/minneapolis/Police Officers Hired 2016-2022_Public_Sent.xlsx")

min_hires <- min_hires %>% 
  janitor::clean_names()


min_hires <- min_hires %>% 
  mutate(date_hired = as_date(hire_date),
          year = year(date_hired),
          month = month(date_hired),
          day = day(date_hired),
          year_month = mdy(paste0(month, "-1-", year)))

min_hires %>% 
  write_csv("created_data/minnesota/minnesota_police_hires.csv")
