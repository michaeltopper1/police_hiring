library(tidyverse)
library(pdftools)

houston <- pdf_text("data/houston/police_positions/R149262-120523_report.pdf")

## 2017 and 2022 had issues
houston_17 <- pdf_text("data/houston/police_positions/2017.pdf")
houston_22 <- pdf_text("data/houston/police_positions/2022.pdf")


houston_split <- houston %>% 
  str_split("\n") %>% 
  unlist() %>% 
  as_tibble() %>% 
  slice(-1) 


houston_clean <- houston_split %>% 
  extract(value, "department", "(HOUSTON POLICE DEPARTMENT)", remove = F) %>% 
  extract(value, "job_number", "(\\d{5})", remove = F) %>% 
  extract(value, "position", "\\d{5}\\s{1,}(.{1,})\\)", remove = F) %>% 
  extract(value, "application_date", "(\\d{1,2}/\\d{1,2}/\\d{4})", remove = F) %>% 
  extract(value, "race", "\\d{1,2}/\\d{1,2}/\\d{4}(.{1,})\\s{2,}[FM]", remove = F) %>%
  extract(value, "sex", "(Male|Female)", remove = F) %>% 
  extract(value, "hired", "(Yes)", remove = F) %>% 
  extract(value, "hired_date", "Yes.{1,}(\\d{1,2}/\\d{1,2}/\\d{4})", remove = F) %>% 
  relocate(-value)


houston_clean <- houston_clean %>% 
  drop_na(department) %>% 
  mutate(across(everything(), ~str_trim(.))) 

houston_clean <- houston_clean %>% 
  mutate(hired = if_else(is.na(hired), "No", hired)) %>% 
  mutate(hired = if_else(hired == "Yes", 1, 0)) %>% 
  mutate(male = if_else(sex == "Male", 1, 0)) %>% 
  mutate(across(ends_with("date"), ~mdy(.)))

houston_clean <- houston_clean %>% 
  mutate(race = str_trim(race)) %>% 
  mutate(race_2 = case_when(
    str_detect(race, "American") ~ "Black",
    str_detect(race, "Asian") ~ "Asian",
    str_detect(race, "Hispanic") ~ "Hispanic or Latino",
    str_detect(race, "White") ~ "White",
    str_detect(race, "Two or More Races") ~ "Other",
    str_detect(race, "Islander") ~ "Other",
    str_detect(race, "Alaska") ~ "Other"
  )) 

houston_clean <- houston_clean %>% 
  mutate(date = as_date(application_date),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year)))
  
houston_clean <- houston_clean %>% 
  mutate(race = race_2) %>% 
  select(-race_2)


# filter out 2017 and 2022 ------------------------------------------------

houston_clean <- houston_clean %>% 
  filter(!year %in% c(2017, 2022))


# clean 2017 and 2022 and append ------------------------------------------


houston_22 <- houston_22 %>% 
  str_split("\n") %>% 
  unlist() %>% 
  as_tibble() %>% 
  slice(-1) 

houston_split_17 <- houston_17 %>% 
  str_split("\n") %>% 
  unlist() %>% 
  as_tibble() %>% 
  slice(-1) 


houston_split_17 <- houston_split_17 %>% 
  bind_rows(houston_22)

houston_clean_17 <- houston_split_17 %>% 
  extract(value, "department", "(HOUSTON POLICE DEPARTMENT)", remove = F) %>% 
  extract(value, "job_number", "(\\d{5})", remove = F) %>% 
  extract(value, "position", "\\d{5}\\s{1,}(.{1,})\\)", remove = F) %>% 
  extract(value, "application_date", "(\\d{1,2}/\\d{1,2}/\\d{4})", remove = F) %>% 
  extract(value, "race", "\\d{1,2}/\\d{1,2}/\\d{4}(.{1,})\\s{2,}[FM]", remove = F) %>%
  extract(value, "sex", "(Male|Female)", remove = F) %>% 
  extract(value, "hired", "(Yes)", remove = F) %>% 
  extract(value, "hired_date", "Yes.{1,}(\\d{1,2}/\\d{1,2}/\\d{4})", remove = F) %>% 
  relocate(-value)

houston_clean_17 <- houston_clean_17 %>% 
  drop_na(department) %>% 
  mutate(across(everything(), ~str_trim(.))) 

houston_clean_17 <- houston_clean_17 %>% 
  mutate(hired = if_else(is.na(hired), "No", hired)) %>% 
  mutate(hired = if_else(hired == "Yes", 1, 0)) %>% 
  mutate(male = if_else(sex == "Male", 1, 0)) %>% 
  mutate(across(ends_with("date"), ~mdy(.)))

houston_clean_17 <- houston_clean_17 %>% 
  mutate(race = str_trim(race)) %>% 
  mutate(race_2 = case_when(
    str_detect(race, "American") ~ "Black",
    str_detect(race, "Asian") ~ "Asian",
    str_detect(race, "Hispanic") ~ "Hispanic or Latino",
    str_detect(race, "White") ~ "White",
    str_detect(race, "Two or More Races") ~ "Other",
    str_detect(race, "Islander") ~ "Other",
    str_detect(race, "Alaska") ~ "Other"
  )) 

houston_clean_17 <- houston_clean_17 %>% 
  mutate(date = as_date(application_date),
         year = year(date),
         month = month(date),
         day = day(date),
         year_month = mdy(paste0(month, "-1-", year)))

houston_clean_17 <- houston_clean_17 %>% 
  mutate(race = race_2) %>% 
  select(-race_2)


# combine -----------------------------------------------------------------


houston_clean <- houston_clean %>% 
  bind_rows(houston_clean_17)


houston_clean  %>% 
  write_csv("created_data/houston/houston_police_apps.csv")
