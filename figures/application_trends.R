library(tidyverse)

job_applications <- read_csv("analysis_data/job_applications.csv")
job_apps <- read_csv("analysis_data/xxmonthly_apps.csv")
job_apps_q <- read_csv("analysis_data/xxquarterly_apps.csv")

theme_set(theme_minimal())


# goal:find out smoothness ------------------------------------------------

cities <- job_apps %>% 
  distinct(city) %>% 
  pull()

city_plots <- list()
for (i in cities) {
  city_plots[[i]] <- job_apps %>% 
    filter(application_type == "Police") %>% 
    filter(city == i) %>% 
    filter(year >= 2019) %>% 
    group_by(year_month, application_type) %>% 
    summarize(number_applications = sum(number_applications, na.rm = T)) %>% 
    ggplot(aes(year_month,number_applications)) +
    geom_point() +
    geom_text(aes(label = number_applications), vjust = -1,
              size = 3) +
    geom_vline(xintercept = as_date("2020-05-24"),
               linetype  = "dashed") +
    labs(title = i)
} 

## baltimore
city_plots[1]
## looks like smooth from 2019 through 2023 or so at the monthly level

## Denver
city_plots[2]
## quarterly looks like the way to go here. 2019 through 2023

## Houston
city_plots[3]
## monthly looks really good. 2019 through 2023

## LA
city_plots[4]
## also very smooth at monthly. start from March 2019 through 2023

## Minneapolis
city_plots[5]
## quarterly looks like the way to go here.2019 maybe through 2023


##OKC
city_plots[6]
## looks like monthly data is good, but there is missing June - September 2020
## either no apps during this time, or something weird with data
## I have followed up with them in email


##Phoenix
city_plots[7]
## looks good monthly, but there is weird number going on in early 2020
## none in 2020-04 and only 1 in March 2020
## asking Malik about this.

## San Antonio
city_plots[8]
## looks good monthly. Nothing weird inherently sticks out.


## Louisville
city_plots[9]
## nothing in yet

## San Francisco
##monthly looks really good.
city_plots_fire <- list()
for (i in cities) {
  city_plots_fire[[i]] <- job_apps %>% 
    filter(application_type == "Firefighter") %>% 
    filter(city == i) %>% 
    filter(year >= 2019) %>% 
    group_by(year_month, application_type) %>% 
    summarize(number_applications = sum(number_applications, na.rm = T)) %>% 
    ggplot(aes(year_month,number_applications)) +
    geom_point() +
    geom_text(aes(label = number_applications), vjust = -1,
              size = 3) +
    geom_vline(xintercept = as_date("2020-05-24"),
               linetype  = "dashed") +
    labs(title = i)
} 

## Houston looks like the only useable one
city_plots_fire[9]


## taking only the places with monthly data
monthly_cities <- c("Baltimore",
                    "Houston",
                    "Los Angeles",
                    "Phoenix",
                    "San Antonio",
                    "San Francisco")


motivation_graph <- job_apps %>% 
  filter(city %in% monthly_cities) %>% 
  filter(application_type == "Police") %>% 
  filter(year_month >= as_date('2019-03-01')) %>% 
  group_by(year_month) %>% 
  summarize(number_applications = sum(number_applications, na.rm = T)) %>% 
  ggplot(aes(year_month, number_applications)) +
  geom_point() +
  geom_vline(xintercept = as_date("2020-05-24"),
             linetype  = "dashed") +
  labs(x = "Year-Month",
       y = "Number of Police Officer Applications",
       title = "Police Officer Applications Over Time",
       subtitle = "Six Departments: Baltimore, Houston, LA, Phoenix, San Antonio, San Francisco")

job_apps %>% 
  filter(city %in% monthly_cities) %>% 
  filter(application_type == "Police") %>% 
  filter(year_month >= as_date('2019-03-01')) %>% 
  group_by(year_month) %>% 
  summarize(number_applications = sum(number_applications, na.rm = T)) %>% 
  ggplot(aes(year_month, number_applications)) +
  geom_point() +
  geom_vline(xintercept = as_date("2020-05-24"),
             linetype  = "dashed")

race_graph <- job_apps %>% 
  filter(city %in% monthly_cities) %>% 
  filter(application_type == "Police") %>% 
  filter(year_month >= as_date('2019-03-01')) %>% 
  pivot_longer(cols = starts_with("race_"),
               names_to = "race",
               values_to = "number_apps_race") %>% 
  mutate(race = str_replace(race, "race_", "") %>% 
           str_to_title()) %>% 
  group_by(year_month, race) %>% 
  summarize(number_applications = sum(number_apps_race, na.rm = T)) %>% 
  ggplot(aes(year_month, number_applications)) +
  geom_point() +
  geom_vline(xintercept = as_date("2020-05-24"),
             linetype  = "dashed") +
  facet_wrap(~race) +
  labs(x = "Year-Month",
       y = "Number of Police Officer Applications",
       title = "Police Officer Applications Over Time",
       subtitle = "Six Departments: Baltimore, Houston, LA, Phoenix, San Antonio, San Francisco")


fire_apps_graph <- job_apps %>% 
  # filter(city %in% monthly_cities) %>% 
  filter(year >= 2019) %>% 
  filter(application_type == "Firefighter") %>% 
  group_by(year_month,city) %>% 
  summarize(number_applications = sum(number_applications, na.rm = T)) %>% 
  ggplot(aes(year_month, number_applications)) +
  geom_point() +
  geom_vline(xintercept = as_date("2020-05-24"),
             linetype  = "dashed") +
  facet_wrap(~city) +
  labs(title = "FireFighter Applications Over Time",
       subtitle = "Dashed line is George Floyd Murder",
       x = "Month by Year",
       y = "Number of Applications Received")

ggsave(motivation_graph, filename = "figures/applications_aggregate.jpeg", width = 7, height = 5)
ggsave(race_graph, filename = "figures/applications_aggregate_race.jpeg", width = 7, height = 5)
ggsave(fire_apps_graph, filename = "figures/firefighter_by_city.jpeg", width = 7, height = 5)



