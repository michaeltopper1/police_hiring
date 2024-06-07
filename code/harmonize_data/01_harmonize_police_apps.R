library(tidyverse)

## importing data in

balt <- read_csv("created_data/baltimore/baltimore_police_apps.csv")
denver <- read_csv("created_data/denver/denver_police_apps.csv")
houston <- read_csv("created_data/houston/houston_police_apps.csv")
la <- read_csv("created_data/los_angeles/los_angeles_police_apps.csv")
louisville <- read_csv("created_data/louisville/apps_cleaned.csv")
minnesota <- read_csv("created_data/minnesota/minnesota_police_apps.csv")
okc <- read_csv("created_data/okc/police_job_apps.csv")
phoenix <- read_csv("created_data/phoenix/phoenix_police_apps.csv")
san_antonio <- read_csv("created_data/san_antonio/san_antonio_police_apps.csv")
sf <- read_csv("created_data/san_fran/police_apps.csv")


# columns wanted ----------------------------------------------------------

columns_wanted <- c("job_id", "application_received", "sex", "race", "date", "year", "year_month",
  "age", "city", "position", "hired_date", "education")

# baltimore harmonize -----------------------------------------------------

balt <- balt %>% 
  mutate(hired = if_else(!is.na(offer_date), 1, 0)) %>% 
  mutate(age = NA,
         city = "Baltimore",
         position = NA,
         hired_date = NA,
         education = NA,
         job_id = as.character(job_id)) %>% 
  rename(sex = gender) %>% 
  select(all_of(columns_wanted))

# denver harmonize --------------------------------------------------------

denver <- denver %>% 
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

# houston harmonize -------------------------------------------------------

houston <- houston %>% 
  rename(application_received = application_date,
         job_id = job_number) %>% 
  mutate(city = "Houston",
         age = NA,
         education = NA,
         job_id = job_id %>% as.character()) %>% 
  select(all_of(columns_wanted))


# la harmonize ------------------------------------------------------------

la <- la %>% 
  rename(application_received = submission_datetime,
         sex = gender,
         date = submission_date) %>% 
  mutate(job_id = NA,
         age = NA,
         city = "Los Angeles",
         position = "Police Officer",
         hired_date = NA) %>% 
  select(all_of(columns_wanted))


# louisville harmonize ----------------------------------------------------


## on hold


# minnesota ---------------------------------------------------------------

minnesota <- minnesota %>% 
  rename(date = application_date,
         application_received = application_date,
         job_id = job_title) %>% 
  mutate(age = NA,
         city = "Minneapolis",
         hired_date = NA,
         sex = NA,
         position = job_id,
         education = NA) %>% 
  select(all_of(columns_wanted))


# okc ---------------------------------------------------------------------

okc <- okc %>%
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

# phoenix harmonize -------------------------------------------------------

phoenix <- phoenix %>% 
  rename(position = posting_title,
         application_received = date_applied) %>% 
  mutate(city = "Phoenix",
         age = NA,
         sex = NA,
         job_id = NA,
         date = application_received,
         hired_date = NA,
         education = NA) %>% 
  select(all_of(columns_wanted))


# san antonio harmonize ---------------------------------------------------

san_antonio <- san_antonio %>% 
  rename(application_received = date_applied) %>% 
  mutate(city = 'San Antonio',
         date = application_received,
         sex = NA,
         job_id = NA,
         position = "Police Officer",
         hired_date = date_hired,
         education = NA) %>% 
  select(all_of(columns_wanted))


# sf harmonize ------------------------------------------------------------

sf <- sf %>% 
  rename(application_received = date_applied) %>% 
  mutate(job_id = NA,
         sex = NA,
         age = NA,
         city = "San Francisco",
         position = "Police Officer",
         education = NA) %>% 
  select(all_of(columns_wanted))


# combining all -----------------------------------------------------------

job_applications <- balt %>% 
  bind_rows(denver, houston, la, minnesota,
            okc, phoenix, san_antonio,
            sf)


# cleaning race -----------------------------------------------------------


job_applications <- job_applications %>% 
  mutate(race = str_to_lower(race) %>% 
           str_trim()) %>% 
  mutate(race = case_when(
    race == "african american" ~ "black",
    race == "hispa" ~"hispanic",
    race == "asian/pacific islander" ~"asian",
    race == "amercian indian/alaskan native" ~"other",
    race == "othrblnd" ~ "other",
    race == "hispanic or latino" ~ "hispanic",
    race == "unknown" ~ NA,
    race == "pacif" ~"other",
    race == "blck/whte" ~ "other",
    race == "amind/wh" ~ "other",
    race == "amind/bl" ~ "other",
    race == "blck/wht" ~ "other",
    race == "nspec" ~ NA,
    race == "race" ~ NA,
    race == "#n/a" ~ NA,
    .default = race))



job_applications %>% 
  write_csv("analysis_data/job_applications.csv")
