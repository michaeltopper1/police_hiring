library(tidyverse)

sf <- readxl::read_excel("data/san_fran/PRA_Sworn Officers_2024_JobApps.xlsx") %>% 
  janitor::clean_names(
  )
sf_2 <- readxl::read_excel("data/san_fran/PRA_Sworn Officers_2024_SmartRecruiters.xlsx") %>% 
  janitor::clean_names()


sf_2 <- sf_2 %>% 
  mutate(date = as_date(application_creation_date),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year))) 

sf <- sf %>% 
  mutate(date = as_date(date_applied),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year)))


# harmonizing the two data sets -------------------------------------------

sf <- sf %>% 
  rename(hired_date = app_hire_date,
         race = ethnicity_self_reported)

sf_2 <- sf_2 %>% 
  rename(date_applied = application_creation_date,
         race = race_self_reported,
         hired_date = hire_date,
         job_source = source) %>% 
  select(-job_code_and_title) 


sf_all <- sf %>% 
  bind_rows(sf_2) 


# cleaning ----------------------------------------------------------------

sf_all <- sf_all %>% 
  mutate(race = case_when(
    race == "0" ~ NA,
    race == "Am. Indian or Alaskan Native (not Hispanic)"~ "Other",
    race == "American Indian or Alaska Native" ~ "Other",
    race == "Asian" ~ "Asian",
    race == "Asian (except Filipino)" ~ "Asian",
    race == "Asian or Pacific Islanders (except Filipino)" ~ "Asian",
    race == "Asian-Filipino" ~ "Asian",
    race == "Black or African American" ~ "Black",
    race == "Black or African American (not of Hispanic origin)" ~ "Black",
    race == "Filipino" ~ "Other",
    race == "Hispanic or Latino" ~ "Hispanic",
    race == "Multiracial" ~ "Other",
    race == "Native Hawaiian or Other Pacific Islander" ~ "Other",
    race == "Native Hawaiian or Pac. Islander (not Hispanic)" ~ "Other",
    race == "Prefer not to answer" ~ NA,
    race == "Two or More Races" ~ "Other",
    race == "Undeclared" ~ NA,
    race == "White" ~ "White",
    race == "White (not of Hispanic origin)" ~ "White"
  )) 

sf_all %>% 
  write_csv("created_data/san_fran/police_apps.csv")
