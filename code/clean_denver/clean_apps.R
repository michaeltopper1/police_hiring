library(tidyverse)


# reading in data ---------------------------------------------------------


denver <- readxl::read_excel("data/denver/CORA Request -Firefighter  Police 2018-2023.xlsx",
                             col_types = "text") %>% 
  janitor::clean_names() %>% 
  mutate(across(c(date_applied, hire_date), ~. %>%  as.numeric(date_applied) %>% 
           janitor::excel_numeric_to_date()) )


# creating dates ----------------------------------------------------------


denver <- denver %>% 
  mutate(date = as_date(date_applied),
          year = year(date),
          month = month(date),
          day = day(date),
          year_month = mdy(paste0(month, "-1-", year)))


# splitting since fire and police are together ----------------------------

denver_fire <- denver %>%
  filter(agency == "DFD Denver Fire Department Agency")

denver_police <- denver %>% 
  filter(agency == "DPD Denver Police Department Agency")



# cleaning the race -------------------------------------------------------


denver_police <- denver_police %>% 
  mutate(race_ethnicity = str_replace(race_ethnicity, "\\(United States of America\\)", "") %>% 
           str_trim()) %>% 
  mutate(race_ethnicity = case_when(
    race_ethnicity == "American Indian or Alaska Native" ~ "Other",
    race_ethnicity == "Black or African American" ~"Black",
    race_ethnicity == "Decline to Identify" ~ NA,
    race_ethnicity == "Hispanic/Latino" ~ "Hispanic", 
    race_ethnicity == "Native Hawaiian or Other Pacific Islander" ~ "Other",
    race_ethnicity == "Two or More Races" ~ "Other",
    race_ethnicity == "White" ~ "White",
    race_ethnicity == "Asian" ~ "Asian",
  )) 



denver_fire <- denver_fire %>% 
  mutate(race_ethnicity = str_replace(race_ethnicity, "\\(United States of America\\)", "") %>% 
           str_trim()) %>% 
  mutate(race_ethnicity = case_when(
    race_ethnicity == "American Indian or Alaska Native" ~ "Other",
    race_ethnicity == "Black or African American" ~"Black",
    race_ethnicity == "Decline to Identify" ~ NA,
    race_ethnicity == "Hispanic/Latino" ~ "Hispanic", 
    race_ethnicity == "Native Hawaiian or Other Pacific Islander" ~ "Other",
    race_ethnicity == "Two or More Races" ~ "Other",
    race_ethnicity == "White" ~ "White",
    race_ethnicity == "Asian" ~ "Asian",
  )) 


# saving ------------------------------------------------------------------

denver_police %>% 
  write_csv("created_data/denver/denver_police_apps.csv")

denver_fire %>% 
  write_csv("created_data/denver/denver_fire_apps.csv")


