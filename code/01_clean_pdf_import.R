## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-03-18
##

library(tidyverse)
# 
# pages_questions %>% 
#   str_trim() %>% 
#   str_squish() %>% 
#   str_detect("^Matthew Johnson")
# pages_questions[[673]] %>% 
#   str_remove_all("\n")

apps <- read_csv("uncleaned_data/questionaire_extracted.csv")

## transforming to dates for the date_received and switching id to a double
apps <- apps %>% 
  mutate(across(everything(), ~str_trim(.))) %>% 
  mutate(date_received = str_trunc(date_received, 17)) %>% 
  mutate(date_received = mdy_hm(date_received)) 


## cleaning first and last name
apps <- apps %>% 
  relocate(name) %>% 
  separate_wider_delim(cols = name,
                       delim = " ",
                       names = c("first_name", "last_name"),
                       too_many = "merge") 


## changing the currently_employed and related to louisville columsn to binaries
apps <- apps %>% 
  mutate(currently_employed_by_louisville = ifelse(currently_employed_by_louisville == "Yes",
                                                   1, 0)) %>% 
  mutate(related_to_louisville_employee = related_to_louisville_employee %>% str_to_lower()) %>% 
  mutate(related_to_louisville_employee_raw = related_to_louisville_employee %>% 
           str_replace("^no.+", "no") %>% 
           str_replace("^i am not.+", "no")%>% 
           str_replace("^n/a", "no") %>% 
           str_replace("^\\.", "no") %>% 
           str_replace("^i have no relations with+.", "no") %>% 
           replace_na("no")) %>% 
  mutate(related_to_louisville_employee = ifelse(related_to_louisville_employee_raw == "no", 0, 1))

## should check the currently employed by louisville column. THre is an NA for matthew johnson. this is ok
## matthew johnson looks like an error of not filling out anthing id: 14844332
## it appears that the laid_off_from_louisville in last 12 month is a colun in which no one answered yes.
## might just delete this column
apps <- apps %>% 
  mutate(completed_highschool_raw = completed_highschool) %>% 
  mutate(completed_highschool = ifelse(completed_highschool == "Yes", 1, 0)) %>% 
  mutate(previously_employed_by_louisville_raw = previously_employed_by_louisville) %>% 
  mutate(previously_employed_by_louisville = ifelse(previously_employed_by_louisville == "Yes", 1, 0)) %>% 
  mutate(attended_vocational_business_or_military_raw = attended_vocational_business_or_military) %>% 
  mutate(attended_vocational_business_or_military = ifelse(attended_vocational_business_or_military == "Yes",
                                                           1, 0 )) %>% 
  mutate(laid_off_from_louisville_12month = ifelse(laid_off_from_louisville_12month == "Yes", 1, 0))

## getting rid of matthew johnson who didn't complete this application at all. Unclear why it is submitted.
## there are two other matthew johnsons though. 
apps <- apps %>% 
  filter(!(id == 14844332 & is.na(previously_employed_by_louisville)))

apps <- apps %>% 
  select(-understand_app, -laid_off_from_louisville_12month) %>% 
  relocate(first_name, last_name,id,  date_received, completed_highschool, related_to_louisville_employee, 
           currently_employed_by_louisville, previously_employed_by_louisville, completed_highschool,
           attended_vocational_business_or_military)

apps <- apps %>% 
  mutate(hear_opportunity = case_when(
    how_hear_about_opportunity == "Current Louisville Metro Government Employee" ~ "current employee",
    how_hear_about_opportunity == "Facebook" ~ "other",
    how_hear_about_opportunity == "Friend" ~ "friend",
    how_hear_about_opportunity == "Industry/Profession Specific Website" ~ "job search website",
    how_hear_about_opportunity == "Internet Job Search Site (for example, Career Builder)" ~ "job search website",
    how_hear_about_opportunity == "LinkedIn" ~ "job search website", 
    how_hear_about_opportunity == "Louisville Metro Website" ~ "louisville website",
    how_hear_about_opportunity == "Newspaper, Online Version" ~ "other",
    how_hear_about_opportunity == "Newspaper, Printed Version" ~ "other",
    how_hear_about_opportunity == "Other" ~ "other",
    how_hear_about_opportunity == "TV Advertisement" ~ "other")) %>% 
  fastDummies::dummy_cols(select_columns = "hear_opportunity") 

apps <- apps %>% 
  mutate(across(ends_with("name"), ~str_to_lower(.))) %>% 
  mutate(year_applied = year(date_received),
         month_applied = month(date_received),
         year_month_applied = mdy(paste0(month_applied, "-1-", year_applied))) 

apps %>% 
  write_csv("created_data/apps_cleaned.csv")
