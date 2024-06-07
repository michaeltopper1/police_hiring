library(tidyverse)
library(pdftools)

phires <- pdf_text("data/arizona/Hires.pdf")

phires <- phires %>% 
  unlist() %>% 
  str_split("\n") %>% 
  unlist() %>% 
  as_tibble() %>% 
  slice(-1) %>% 
  c() %>% 
  unlist() %>% 
  str_split_fixed("\\s{2,}", n = 4) %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  extract(v2, "date_hired", "(\\d{1,2}/\\d{1,2}/\\d{4})",
          remove = F) %>% 
  extract(v2, "title",
          "(Police Recruit|Police Officer)")

phires <- phires %>% 
  mutate(rehire = if_else(v1 == "REH", 1, 0)) %>% 
  mutate(new_hire = if_else(v1 == "HIR", 1, 0)) %>% 
  mutate(date_hired = mdy(date_hired)) %>% 
  rename(age = v4,
         race = v3) %>% 
  select(-v1)

phires %>% 
  write_csv("created_data/phoenix/phoenix_police_hires.csv")
