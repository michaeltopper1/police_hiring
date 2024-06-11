## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2024-06-10
##

library(tidyverse)
library(readxl)
library(tidylog)

files <- list.files("data/chicago/applications/", pattern = ".xlsx$")

job_applications <- map_df(files, ~read_excel(paste0("data/chicago/applications/", .)))

job_applications <- job_applications %>%  
  janitor::clean_names()

job_applications <- job_applications %>% 
  drop_na(submission_completed_date)

job_applications <- job_applications %>% 
  group_by(across(-c(submission_education_degree_type, race, gender))) %>% 
  summarize(submission_education_degree_type = paste(submission_education_degree_type,
                                                     collapse = " / "),
            race = paste(race, collapse = "/"),
            gender = paste(gender, collapse = "/")) %>% 
  ungroup()

job_applications <- job_applications %>% 
  mutate(hired = if_else(!is.na(hired_date), 1, 0))

job_applications <- job_applications %>% 
  mutate(date = as_date(submission_completed_date),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year)))


# cleaning race -----------------------------------------------------------

## No Hispanic in here. I'm guessing because this is an ethnicity?
job_applications <- job_applications %>% 
  mutate(race = if_else(str_detect(race, "More Than 1 Choice"), "Other", race),
         race = if_else(str_detect(race,"NA"),NA, race),
         race = if_else(str_detect(race, "Undisclosed"), NA, race),
         race = if_else(str_detect(race, "Race - White"), "White", race),
         race = if_else(str_detect(race, "Race - Black"), "Black", race),
         race = if_else(str_detect(race, "Race - Asian"), "Asian", race),
         race = if_else(str_detect(race, "Race - Indian Native"), "Other", race),
         race = if_else(str_detect(race, "Race - Pacific Native"), "Other", race)) 


# cleaning gender ---------------------------------------------------------

## fixing the gender column
## gave a non-binary to those that listed both genders
job_applications <- job_applications %>% 
  mutate(gender = if_else(gender == "Gender - Female/Gender - Male",
                          "Non-Binary", gender),
         gender = if_else(str_detect(gender, "Gender - Female"), "Female", gender),
         gender = if_else(str_detect(gender, "Gender - Male"), "Male", gender),
         gender = if_else(str_detect(gender, "NA|Undisclosed"), NA, gender)) 

# optional - clean education attainment -----------------------------------



job_applications %>% 
  write_csv("created_data/chicago/police_apps.csv")
