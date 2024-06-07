library(tidyverse)

okc <- readxl::read_excel("data/okc/ORR-2308-2024 Response Docs.xlsx",
                          col_types = "text") %>% 
  janitor::clean_names()

okc <- okc %>% 
  mutate(submission_completed_date = mdy(submission_completed_date)) %>% 
  mutate(race = case_when(
    race == "American Indian or Alaska Native"~ "Other",
    race == "Asian" ~ "Asian",
    race == "Black or African American" ~ "Black",
    race == "Hispanic or Latino" ~ "Hispanic",
    race == "Native Hawaiian or other Pacific Islander" ~ "Other",
    race == "Not Indicated" ~ NA,
    race == "Two or More" ~ "Other",
    race == "White" ~ "White"
  ))

okc <- okc %>% 
  mutate(date = as_date(submission_completed_date),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year)))

okc_fire <- okc %>% 
  filter(title_bl == "Fire Recruit")

okc_police <- okc %>% 
  filter(title_bl == "Police Officer")


# writing -----------------------------------------------------------------

okc_fire %>% 
  write_csv("created_data/okc/fire_job_apps.csv")

okc_police %>% 
  write_csv("created_data/okc/police_job_apps.csv")
