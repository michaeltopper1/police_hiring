library(tidyverse)

la_new <- readxl::read_excel("data/los_angeles/All Applications Received 3.14.19 through 12.20.23.xlsx") %>% 
  janitor::clean_names()


la_old <- readxl::read_excel("data/los_angeles/PO Applications - 1.1.2016 to 12.31.2022.xlsx",
                     
                     sheet = "1-1-2016 to 3-13-2019") %>% 
  janitor::clean_names() %>% 
  select(-starts_with("do_not_modify")) %>% 
  mutate(submission_datetime = submission_date,
         submission_date = as_date(submission_date)) %>% 
  mutate( year = year(submission_date),
          month = month(submission_date),
          day = day(submission_date),
          year_month = mdy(paste0(month, "-1-", year)))



la_new <- la_new %>% 
  select(-starts_with("do_not"))

la_new <- la_new %>%
  mutate(submission_date= as_date(submission_date),
          year = year(submission_date),
          month = month(submission_date),
          day = day(submission_date),
          year_month = mdy(paste0(month, "-1-", year)))



la_old <- la_old %>% 
  rename(education_level = max_grade_level)


la_new <- la_new %>% 
  mutate(lapd_only = if_else(reserve == "No" &
                               lapd == "Yes" &
                               port == "No" &
                               airport == "No", 
                             1, 0),
         reserve_only = if_else(reserve == "Yes" &
                                  lapd == "No" &
                                  port == "No" &
                                  airport == "No", 
                                1, 0),
         port_only = if_else(reserve == "No" &
                               lapd == "No" &
                               port == "Yes" &
                               airport == "No", 
                             1, 0),
         airport_only = if_else(reserve == "No" &
                                  lapd == "No" &
                                  port == "No" &
                                  airport == "Yes", 
                                1, 0)) %>% 
  filter(!airport_only == 1) %>% 
  filter(!port_only == 1) %>% 
  filter(!reserve_only == 1) 


la <- la_new %>% 
  # filter(lapd_only == 1) %>%
  # filter(lapd == "Yes") %>% 
  bind_rows(la_old)

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


la %>% 
  # filter(lapd_only == 1 | is.na(lapd_only)) %>% 
  filter(lapd == "Yes" | is.na(lapd)) %>% 
  filter(submission_date < as_date("2020-03-01")) %>% 
  filter(submission_date > as_date("2017-10-01")) %>% 
  count(year_month, race) %>% 
  ggplot(aes(year_month,n)) +
  geom_point() +
  geom_vline(xintercept = as_date("2019-03-01"), linetype = "dashed") +
  facet_wrap(~race) +
  labs(x = "Year-Month",
       y = "Number of Applications",
       title = "Number of Job Applications",
       subtitle = "LAPD: 2017-2020")+
  theme_minimal()

