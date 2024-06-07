library(tidyverse)



# these are the ones that I trust -----------------------------------------

fire_denver <- read_csv("created_data/denver/denver_fire_apps.csv")

fire_houston <- read_csv("created_data/houston/houston_fire_apps.csv")

fire_louisville <- read_csv("created_data/louisville/fire_apps_cleaned.csv")

fire_minn <- read_csv("created_data/minnesota/fire_apps.csv")

fire_okc <- read_csv("created_data/okc/fire_job_apps.csv")



# columns wanted ----------------------------------------------------------

columns_wanted <- c("job_id", "application_received", "sex", "race", "date", "year", "year_month",
                    "age", "city", "position", "hired_date", "education")




# denver ------------------------------------------------------------------

fire_denver <- fire_denver %>% 
  rename(job_id = job_profile,
         race = race_ethnicity,
         application_received = date_applied,
         hired_date = hire_date) %>% 
  mutate(sex = NA,
         city = "Denver",
         age = NA,
         position = NA,
         education = NA,
         job_id = as.character(job_id)) %>% 
  select(all_of(columns_wanted))


# houston fire ------------------------------------------------------------

fire_houston <- fire_houston %>% 
  rename(position = job_title,
         job_id = job_number,
         application_received = application_date,
         hired_date = date_hired) %>% 
  mutate(education = NA,
         city = "Houston",
         age = NA,
         sex = NA,
         job_id = job_id %>% as.character()) %>% 
  select(all_of(columns_wanted))


# louisville fire ---------------------------------------------------------

fire_louisville <- fire_louisville %>% 
  rename(position = job_title,
         application_received = date_received,
         hired_date = hire_date,
         education = highest_education_level,
         sex = text_gender) %>% 
  mutate(city = "Louisville",
         age = NA,
         job_id = NA) %>% 
  select(all_of(columns_wanted))


# minnesota fire ----------------------------------------------------------
## for minnesota, looks like some uneven sort of numbees going on here

fire_minn <- fire_minn %>% 
  rename(position = job_title,
         application_received = date_time_received,
         hired_date = hire_date) %>% 
  mutate(job_id = NA,
         sex = NA,
         race = NA,
         city = "Minneapolis",
         age = NA,
         education = NA) %>% 
  select(all_of(columns_wanted))


# okc ---------------------------------------------------------------------

fire_okc <- fire_okc %>% 
  rename(application_received = submission_completed_date,
         sex = gender,
         job_id = req_identifier,
         position = title_bl) %>% 
  mutate(city = "Oklahoma City",
         age = NA,
         hired_date = NA,
         education = NA,
         job_id = as.character(job_id)) %>% 
  select(all_of(columns_wanted))


# merging all -------------------------------------------------------------

fire_apps <- fire_denver %>% 
  bind_rows(fire_houston,
            fire_louisville,
            fire_minn,
            fire_okc)


# saving ------------------------------------------------------------------

fire_apps %>% 
  write_csv("analysis_data/fire_job_applications.csv")








