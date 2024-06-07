## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-03-23
##

library(tidyverse)

## first reading in applciation data (pg 1) that needs to be cleaned
application <- read_csv("uncleaned_data/first_page_apps.csv")


## getting rid of unncessary columns
## also parsing the min compensation per hour and per year
application <- application %>% 
  select(-c(value, min_comp, shift_accept, looking_for, work_accept)) %>% 
  mutate(across(starts_with("min"), ~parse_number(.))) 

## cleaning the shift_accept columns to be binaries and NAs if they didn't answer the question
application <- application %>% 
  mutate(across(starts_with("shift"), ~ifelse(!is.na(.), 1, 0))) %>% 
  mutate(acceptshift_unanswered = ifelse(if_all(starts_with("shift"), ~. == 0), 1, 0), .before = 1) %>% 
  mutate(across(starts_with("shift"), ~ifelse(acceptshift_unanswered == 1, NA, .))) %>% 
  select(-acceptshift_unanswered)
         

## cleaning the looking for type of work columns
application <- application %>% 
  mutate(across(starts_with("looking_for"), ~ifelse(!is.na(.), 1, 0))) %>% 
  mutate(lookingfor_unaswered = ifelse(if_all(starts_with("looking_for"), ~. == 0), 1, 0), .before = 1) %>% 
  mutate(across(starts_with("looking_for"), ~ifelse(lookingfor_unaswered == 1, NA, .))) %>% 
  select(-lookingfor_unaswered)

## cleaning the work accept type column
application <- application %>% 
  mutate(across(starts_with("work_accept"), ~ifelse(!is.na(.), 1, 0))) %>%
  mutate(workaccept_unanswered = ifelse(if_all(starts_with("work_accept"), ~.== 0), 1, 0), .before = 1) %>% 
  mutate(across(starts_with("work_accept"), ~ifelse(workaccept_unanswered == 1, NA, .))) %>% 
  select(-workaccept_unanswered)

## splitting names and changing education to dummy column
application <- application %>% 
  separate_wider_delim(cols = name,
                       delim = " ",
                       names = c("first_name", "last_name"),
                       too_many = "merge") %>% 
  mutate(across(c("highest_ed", "first_name", "last_name"), ~str_to_lower(.))) %>%
  mutate(highest_ed = highest_ed %>% str_replace("'s", "")) %>% 
  fastDummies::dummy_cols(select_columns = "highest_ed") 


## getting the date to correct format
application <- application %>% 
  mutate(date_received = mdy_hm(date_received))

application %>% 
  write_csv("created_data/apps_pg1_cleaned.csv")


