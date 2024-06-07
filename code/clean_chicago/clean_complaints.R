library(tidyverse)

complaints <- read_csv("data/chicago/complaints/P910045-_DATA_SET.csv") %>% 
  janitor::clean_names()

complaints <- complaints %>% 
  mutate(across(starts_with("date"), ~dmy(.)))
  
complaints <- complaints %>% 
  mutate(year_of_incident = year(date_of_incident),
          month_of_incident = month(date_of_incident),
          day_of_incident = day(date_of_incident),
          year_month_of_incident = mdy(paste0(month_of_incident, "-1-", year_of_incident))) %>% 
  mutate(year_of_complaint = year(date_of_complaint),
         month_of_complaint = month(date_of_complaint),
         day_of_complaint = day(date_of_complaint),
         year_month_of_complaint = mdy(paste0(month_of_complaint, "-1-", year_of_complaint)))


complaints <- complaints %>% 
  mutate(cpd_appointed_date_raw = dmy(cpd_appointed_date))

## fixing the appointed dates for when appointed date is less than
## 1968 by lubridate default
complaints <- complaints %>% 
  mutate(cpd_appointed_date = dmy(cpd_appointed_date)) %>% 
  mutate(cpd_appointed_date = if_else(
    cpd_appointed_date >= as_date("2040-01-01"),
    cpd_appointed_date - years(100),
    cpd_appointed_date
  ))


complaints <- complaints %>% 
  mutate(closed_date = dmy(closed_date))




## need to find the complaints post 2020
### I NEED Date applied, date appointed, and date of complaint, or date of first shift



