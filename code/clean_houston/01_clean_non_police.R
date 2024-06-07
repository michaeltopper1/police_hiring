library(tidyverse)
library(pdftools)


houston_2016 <- pdf_text("data/houston/non_police_positions/R154265_2016.pdf")
houston_2017 <- pdf_text("data/houston/non_police_positions/R154265_2017pdf.pdf")
houston_2018 <- pdf_text("data/houston/non_police_positions/R154265_2018.pdf")
houston_2019 <- pdf_text("data/houston/non_police_positions/R154265_2019.pdf")
houston_2020 <- pdf_text("data/houston/non_police_positions/R154265_2020.pdf")
houston_2021 <- pdf_text("data/houston/non_police_positions/R154265_2021.pdf")
houston_2022 <- pdf_text("data/houston/non_police_positions/R154265_2022.pdf")


clean_houston <- function(pdf) {
  ## gets the pdf into a nice readable tibble
  pdf_clean <- pdf %>% 
    unlist() %>% 
    str_split("\n") %>% 
    unlist() %>% 
    as_tibble() %>% 
    slice(-1) %>% 
    c() %>% 
    unlist() %>% 
    str_split_fixed("\\s{2,}", n = 3) %>% 
    as_tibble() %>% 
    janitor::clean_names()
  ## extracts the columns
  pdf_clean <- pdf_clean %>% 
    extract(v2, into = c("job_number", "job_title"),
            "(\\d{2,})\\s(.+)") %>% 
    extract(v3, into = c("application_date", "race", "hired"),
            "(\\d{1,2}/\\d{1,2}/\\d{4})\\s(.+)Not Answered\\s+(Yes|No)", remove = F) %>% 
    mutate(race = str_trim(race)) %>% 
    extract(v3, "date_hired",
            ".+Yes\\s+(\\d{1,2}/\\d{1,2}/\\d{4})") 
  pdf_clean <- pdf_clean %>% 
    rename(department_name = v1) %>% 
    mutate(across(matches("date"), ~mdy(.)))
  return(pdf_clean)
}


houston <- map_df(list(houston_2016, houston_2017,
            houston_2018, houston_2019,
            houston_2020, houston_2021,
            houston_2022), ~clean_houston(.))

houston <- houston %>% 
  drop_na(application_date)

houston %>% 
  write_csv("created_data/houston/non_police_apps.csv")
