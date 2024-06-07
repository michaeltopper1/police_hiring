library(tidyverse)

chi <- readxl::read_excel("data/chicago/CPD_Police_Officer_Data_Points_-_11.20.23_Redacted.xlsx") %>% 
  janitor::clean_names()

chi_big <- readxl::read_excel("data/chicago/CPD_Police_Officer_Data_Points_-_Redacted.xlsx") %>% 
  janitor::clean_names()


chi_old <- readxl::read_excel("/Users/michaeltopper/Desktop/05-16-2024_FOIA_Request__due_5_20__CPD.xlsx") %>% 
  janitor::clean_names()

chi_fire <- readxl::read_excel("data/chicago/non_police/FOIA_Request_Received__due_1_24__2022.1_Redacted.xlsx")

## bad data. Looks like a test that occurred 630 times on the same day
chi <- chi %>% 
  filter(req_identifier != "test_4th")

chi <- chi %>% 
  mutate(date = as_date(submission_completed_date),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year)))

chi_big <- chi_big %>% 
  mutate(date = as_date(submission_completed_date),
         year = year(date),
         month = month(date),
         day = day(date),
         year_month = mdy(paste0(month, "-1-", year)))

chi_big %>% 
  count(year_month) %>% View()
chi %>% 
  count(year_month) %>% View()
chi %>% 
  filter(title_name == "9161-POLICE OFFICER") %>%
  count(year) %>% 
  ggplot(aes(year_month, n)) +
  geom_point()

chi_old <- chi_old %>% 
  mutate(date = as_date(submission_completed_date),
         year = year(date),
         month = month(date),
         day = day(date),
         year_month = mdy(paste0(month, "-1-", year)))

chi_old %>% count(year
                  )
  filter(title_name == "9161-POLICE OFFICER") %>% 
  count(year)

chi %>% 
  count(title_bl)

