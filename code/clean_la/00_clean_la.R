library(tidyverse)
library(readxl)

la_old <- read_excel("data/los_angeles/PO Applications - 1.1.2016 to 12.31.2022.xlsx",
                        sheet = "1-1-2016 to 3-13-2019") %>% 
  janitor::clean_names()
la_new <- read_excel("data/los_angeles/PO Applications - 1.1.2016 to 12.31.2022.xlsx",
                    
                     sheet = "3-14-2019 to 12-31-2022") %>% 
  janitor::clean_names()


la_old <- la_old %>% 
  mutate(submission_datetime = submission_date,
         submission_date = as_date(submission_date)) %>% 
  mutate( year = year(submission_date),
          month = month(submission_date),
          day = day(submission_date),
          year_month = mdy(paste0(month, "-1-", year)))

la_new <- la_new %>% 
  select(-starts_with("do_not_modify")) %>% 
  mutate(submission_datetime = submission_date,
         submission_date = as_date(submission_date)) %>% 
  mutate( year = year(submission_date),
          month = month(submission_date),
          day = day(submission_date),
          year_month = mdy(paste0(month, "-1-", year)))

la <- la_old %>% 
  rename(education_level = max_grade_level) %>% 
  bind_rows(la_new)

la <- la %>% 
  mutate(race = case_when(
    ethnic_group_race == "African American" ~ "Black",
    ethnic_group_race == "American Indian" ~ "Other",
    ethnic_group_race == "Asian" ~ "Asian",
    ethnic_group_race == "Black" ~ "Black",
    ethnic_group_race == "Caucasian" ~ "White",
    ethnic_group_race == "Filipino" ~ "Other",
    ethnic_group_race == "Hispanic" ~ "Hispanic",
    ethnic_group_race == "I do not wish to disclose" ~ NA,
    ethnic_group_race == "Pacific Islander" ~ "Other",
    ethnic_group_race == "Two or more races" ~ "Other"
  )) 


la <- la %>% 
  mutate(education = case_when(
    education_level == "Associate" ~ "Some College",
    education_level == "Associate's (2-year) Degree" ~ "Some College",
    education_level == "Bachelor" ~ "Bachelor's", 
    education_level == "Bachelor's (4-year) Degree" ~ "Bachelor's",
    education_level == "GED" ~ "High School",
    education_level == "Graduate Degree" ~ "Graduate Degree",
    education_level == "High School" ~ "High School",
    education_level == "Less than High School" ~ "No Degree",
    education_level == "Masters" ~ "Graduate Degree",
    education_level == "PHD" ~ "Graduate Degree",
    education_level == "Some College" ~ "Some College"
  )) 

la %>% 
  write_csv("created_data/los_angeles/los_angeles_police_apps.csv")
