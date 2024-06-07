## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-03-20
##

library(tidyverse)

demographics <- read_csv("created_data/officer_demographics_cleaned.csv")

resigns <- read_csv("created_data/resigns_cleaned.csv")


demographics %>% 
  slice(20) %>% View()

## david stettler is in here twice. It is very clear he retired and came back to help out
## an then resigned 
merged <- demographics %>% 
  left_join(resigns, by = join_by(officer_first_name == first_name,
                                  officer_last_name == last_name)) 

## problem here is that first and last name matches to multiple people
## not enough information on their badge number. Might have to make
## inferences
demographics %>% 
  group_by(officer_last_name, officer_first_name) %>% 
  add_tally() %>% 
  filter(n>1) %>% View()
  count(officer_first_name, officer_last_name, sort = T) %>% View()
merged %>% 
  group_by(officer_last_name, officer_first_name, badge) %>% 
  add_tally()%>% 
  filter(n>1) %>% View()
