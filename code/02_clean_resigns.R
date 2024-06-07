## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-03-20
##

library(tidyverse)

resigns <- readxl::read_excel("data/resignations.xlsx") %>% 
  janitor::clean_names()


resigns <- resigns %>% 
  rename(first_name = f_name,
         last_name = l_name) %>% 
  mutate(across(where(is.character), ~str_to_lower(.))) %>% 
  mutate(separation_year = year(separation_date),
         separation_month = month(separation_date),
         separation_year_month = mdy(paste0(separation_month, "-1-", separation_year)))

resigns <- resigns %>% 
  filter(separation_year > 2012)


resigns <- resigns %>% 
  rename(separation_reason = reason) %>% 
  fastDummies::dummy_cols(select_columns = "separation_reason")


resigns %>% 
  write_csv("created_data/resigns_cleaned.csv")


# resigns %>% 
#   group_by(separation_year_month) %>% 
#   count() %>% 
#   ggplot(aes(separation_year_month, n)) +
#   geom_line() +
#   theme_minimal()
  


