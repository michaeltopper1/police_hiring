library(tidyverse)

balt <- readxl::read_excel("data/baltimore/MPIA Request - BPD Applications 2018-2022.xlsx") %>% 
  janitor::clean_names()

## empty columns
balt <- balt %>% 
  select(-starts_with("x"))

balt <- balt %>% 
  mutate(race = case_when(
    str_detect(ethnicity, "^American Indian") ~ "Other",
    str_detect(ethnicity, "^Asian") ~ "Asian",
    str_detect(ethnicity, "^Black") ~ "Black",
    str_detect(ethnicity, "^Native") ~ "Other",
    str_detect(ethnicity, "^Two") ~ "Other",
    str_detect(ethnicity, "^White") ~ "White",
    str_detect(ethnicity, "Hispanic") ~"Hispanic"
  )) 

## changing offer to correct date
balt <- balt %>% 
  mutate(offer_date = if_else(offer == "No", NA, offer) %>% 
           as.double() %>% 
           janitor::excel_numeric_to_date())


balt <- balt %>% 
  mutate(date = as_date(application_received),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year))) 

balt %>% 
  write_csv("created_data/baltimore/baltimore_police_apps.csv")
