library(tidyverse)
library(pdftools)

sa <- pdf_text("data/san_antonio/W569879-120623__Topper_Michael_.pdf")

sa_pre2021 <- sa[1:230]
sa_post2021 <- sa[231:288]

sa_pre2021 <- sa_pre2021 %>% 
  unlist() %>% 
  str_split("\n") %>% 
  unlist() %>% 
  str_split_fixed("\\s{2,}", n = 5) %>% 
  as_tibble() %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()

sa_post2021 %>% 
  pluck(1) %>% 
  unlist() %>% 
  str_split("\n") %>% 
  unlist() %>% 
  str_split_fixed("\\s{4,}", n = 4) %>% 
  as_tibble() %>% View()


sa_pre2021 <- sa_pre2021 %>% 
  rename(badge_number = x,
         date_hired = date_hired_badge_number)

  
## fails to parse 4 which delineate between years with columns
## this is totally ok
sa_pre2021 <- sa_pre2021 %>% 
  mutate(across(everything(), ~str_trim(.))) %>% 
  mutate(across(everything(), ~if_else(. == "", NA,.))) %>% 
  filter(!if_all(everything(), ~is.na(.))) %>% 
  mutate(date_applied = mdy(applied_on),
          year = year(date_applied),
          month = month(date_applied),
          day = day(date_applied),
          year_month = mdy(paste0(month, "-1-", year))) %>% 
  mutate(date_hired = mdy(date_hired)) %>% 
  select(-applied_on)



sa_post2021 <- sa_post2021 %>% 
  unlist() %>% 
  str_split("\n") %>% 
  unlist() %>% 
  str_replace_all("Hired", "Hired     ") %>% 
  str_split_fixed("\\s{2,}", n = 4) %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  extract(v4, into = c("date_hired"),
          "(\\d{1,2}/\\d{1,2}/\\d{4})", remove = F) %>%
  extract(v4, into = "badge", "\\d{1,2}/\\d{1,2}/\\d{4}\\s+(\\d{1,4}|NA)",
          remove = F) %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names() 

sa_post2021 <- sa_post2021 %>% 
  rename(badge_number = na,
         date_hired = na_2) %>% 
  select(-date_hired_badge_number_referral_source)

sa_post2021 <- sa_post2021 %>% 
  mutate(across(everything(), ~str_trim(.))) %>% 
  mutate(date_hired = mdy(applied_on),
          year = year(date_hired),
          month = month(date_hired),
          day = day(date_hired),
          year_month = mdy(paste0(month, "-1-", year))) %>% 
  mutate(date_applied = mdy(applied_on)) %>% 
  select(-applied_on)

sa_all <- sa_pre2021 %>% 
  bind_rows(sa_post2021)


sa_all %>% 
  write_csv("created_data/san_antonio/san_antonio_police_apps.csv")
