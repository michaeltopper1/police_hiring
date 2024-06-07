library(tidyverse)


applications <- read_csv("analysis_data/job_applications.csv")
fire_applications <- read_csv("analysis_data/fire_job_applications.csv")


## connecting the applications

applications <- applications %>% 
  mutate(application_type = "Police")

fire_applications <- fire_applications %>% 
  mutate(application_type = "Firefighter")

all_apps <- applications %>% 
  bind_rows(fire_applications)

all_apps <- all_apps %>% 
  drop_na(year_month)

# putting in police killing dates and creating treatments -----------------

all_apps <- all_apps %>% 
  mutate(floyd_date = as_date("2020-06-01"))


# aggregation -------------------------------------------------------------


all_apps <- all_apps %>% 
  mutate(post_floyd = if_else(date >= floyd_date, 1, 0),
         police = if_else(application_type == "Police", 1, 0)) 

## creating dummy columns for race
all_apps <- all_apps %>% 
  mutate(race = race %>% str_to_lower()) %>% 
  fastDummies::dummy_cols(select_columns = c("race"))

## getting quarters
all_apps <- all_apps %>% 
  mutate(quarter =quarter(year_month, type = "year.quarter"),
         quarter_floor = floor_date(year_month, "quarter"), 
                                    .before = 1) 


## aggregation by month
all_apps_monthly <- all_apps %>% 
  group_by(city, year_month, application_type) %>% 
  summarize(across(starts_with("race_"), ~sum(.,na.rm = T)),
            number_applications = n(), .before = 1) 

## aggregation by quarter
all_apps_quarterly <- all_apps %>% 
  group_by(city, quarter_floor, application_type) %>% 
  summarize(across(starts_with("race_"), ~sum(.,na.rm = T)),
            number_applications = n(), .before = 1) %>% 
  mutate(date = quarter_floor)



# create panels -----------------------------------------------------------

panel_dates_monthly <- birthsst::create_panel(start_date = "2016-01-01",
                                      end_date = "2022-12-01",
                                      by = "month")

panel_dates_quarterly <- birthsst::create_panel(start_date = "2016-01-01",
                                                end_date = "2022-12-01",
                                                by = "quarter")

## getting unique cities
cities <- all_apps %>% 
  distinct(city)

panel_dates_monthly <- panel_dates_monthly %>% 
  cross_join(cities) 

panel_dates_quarterly <- panel_dates_quarterly %>% 
  cross_join(cities)


# connecting to the panels ------------------------------------------------


all_apps_monthly <- panel_dates_monthly %>% 
  left_join(all_apps_monthly)

all_apps_quarterly <- panel_dates_quarterly %>% 
  left_join(all_apps_quarterly)



# creating treatment variables --------------------------------------------

all_apps_monthly <- all_apps_monthly %>% 
  mutate(floyd_date = as_date("2020-06-01"),
         post_floyd = if_else(date >= floyd_date, 1, 0),
         police = if_else(application_type == "Police", 1, 0)) 

all_apps_quarterly <- all_apps_quarterly %>% 
  mutate(floyd_date = as_date("2020-06-01"),
         post_floyd = if_else(date >= floyd_date, 1, 0),
         police = if_else(application_type == "Police", 1, 0)) 




# writing to csv ----------------------------------------------------------

all_apps_monthly %>% 
  write_csv("analysis_data/xxmonthly_apps.csv")

all_apps_quarterly %>% 
  write_csv("analysis_data/xxquarterly_apps.csv")
