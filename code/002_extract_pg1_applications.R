## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2023-03-18
##

library(tidyverse)
library(pdftools)

files <- list.files("data/louisville/", pattern = "^POLICE")

pages <- map(files, ~pdf_text(paste0("data/louisville/", .))) %>% unlist() %>% 
  as.list()


## getting an example of the string to match on: Agency-Wide Questions
page <- pages[[1]] %>% 
  str_split("\n") %>% 
  unlist() %>% 
  str_trim() %>% 
  str_squish() %>% 
  pluck(6) 

files
page_numbers <- 1

## finding where the other pages are that have Agency-Wide Questions
for (i in 1:length(pages)){
  page <- pages[[i]] %>% 
    str_split("\n") %>% 
    unlist() %>% 
    str_trim() %>% 
    str_squish() %>% 
    pluck(6)
  if (!is.null(page)){
    if (str_detect(page,"EMPLOYMENT APPLICATION")){
      page_numbers <- c(i, page_numbers)
    }
  }
}

## getting the distinct pages only. Gets rid of the duplicate 3
page_numbers <- page_numbers %>% unique() %>% rev()

## filtering to only the agency-wide questions
pages_employment <- pages[page_numbers]


extract_name <- function(x){
  x <- x %>% extract(value, into = "name",
                     "(.+)Person ID", remove = F)
  return(x)
}

extract_id <- function(x){
  x <- x %>% extract(value, into = "id",
                     "Person ID:\\s(\\d{4,8}).+Received:",
                     remove = F)
  return(x)
}

extract_date <- function(x){
  x <- x %>% extract(value, into = "date_received",
                     "Received:(.+)EMPLOYMENT APPLICATION",
                     remove = F)
  return(x)
}

extract_education <- function(x){
  x <- x %>% extract(value, into = "highest_ed",
                     "What is your highest level of education\\?(.+)PREFERENCE",
                     remove = F)
  return(x)
}

# reformat and put into tibble --------------------------------------------

first_page_app <- map_df(pages_employment, ~.x %>% 
                         str_split("\n") %>% 
                         unlist() %>% 
                         str_squish() %>% 
                         str_trim() %>% 
                         paste(collapse=" ") %>% 
                         as_tibble())


extract_work_accept <- function(x){
  x <-  x %>% extract(value, into = "work_accept",
                      "TYPES OF WORK YOU WILL ACCEPT(.{30})",
                      remove = F) %>% 
    extract(work_accept, "work_accept_fulltime",
            "(Full Time)", remove = F) %>% 
    extract(work_accept, "work_accept_parttime",
            "(Part Time)", remove = F) %>% 
    extract(work_accept, "work_accept_perdiem",
            "(Per Diem)", remove = F) 
  return(x)
}

extract_looking_for <- function(x) {
  x <-  x %>% extract(value, into = "looking_for",
          "WHAT TYPE OF JOB ARE YOU LOOKING FOR\\?(.{40})",
          remove = F) %>% 
    extract(looking_for,"looking_for_regular",
            "(Regular)", remove = F) %>% 
    extract(looking_for, "looking_for_parttime",
            "(Part Time)", remove = F) %>% 
    extract(looking_for, "looking_for_internship",
            "(Internship)", remove = F) %>% 
    extract(looking_for, "looking_for_seasonal",
            "(Seasonal)", remove = F)
  return(x)
}

extract_shift_accept <- function(x){
  x <- x %>% 
    extract(value, "shift_accept",
            "SHIFTS YOU WILL ACCEPT(.{60})",
            remove = F) %>%
    extract(shift_accept, "shift_accept_day",
            "(Day)", remove = F) %>% 
    extract(shift_accept, "shift_accept_evening",
            "(Evening)", remove = F) %>% 
    extract(shift_accept, "shift_accept_night",
            "(Night)", remove = F) %>% 
    extract(shift_accept, "shift_accept_oncall",
            "(On Call)", remove = F) %>% 
    extract(shift_accept, "shift_accept_weekends",
            "(Weekends)", remove = F) %>% 
    extract(shift_accept, "shift_accept_rotating",
            "(Rotating)",remove = F ) 
  return(x)
}

extract_min_comp <- function(x){
  x <- x %>% extract(value, "min_comp", "MINIMUM COMPENSATION:(.{70})",
            remove = F) %>% 
    mutate(min_comp = min_comp %>% str_remove("ARE YOU WILLING TO RELOCATE?")) %>% 
    extract(min_comp, "min_comp_hourly",
            "(.{7})per hour", remove = F) %>% 
    extract(min_comp, "min_comp_yearly",
            "(.{11})per year",
            remove = F) 
  return(x)
}

extract_schooling <- function(x){
  x <- x %>% 
    mutate(schooling = value %>% 
             str_extract_all("SCHOOL NAME:.{60}")) %>% 
    unnest(schooling) %>% 
    mutate(schooling = schooling %>% 
             str_remove("LOC.+") %>% 
             str_remove_all("SCHOOL NAME:") %>% 
             str_remove_all("From: \\d{1,2}/\\d{2,4}") %>% 
             str_remove_all("To: \\d{1,2}/\\d{2,4}") %>% 
             str_remove_all("To: Present") %>% 
             str_squish() %>% 
             str_trim(),.before = 1) %>% 
    group_by(date_received, id, name, value) %>% 
    mutate(spread_id = row_number()) %>% 
    pivot_wider(names_from = "spread_id",
                names_glue = "schooling_{spread_id}",
                values_from = "schooling") %>% 
    ungroup()
  return(x)
}

first_page <- first_page_app %>% 
  extract_name() %>% 
  extract_date() %>% 
  extract_id() %>% 
  extract_education() 


first_page <- first_page %>% 
  extract_work_accept() %>% 
  extract_looking_for() %>% 
  extract_shift_accept() %>% 
  extract_min_comp()

first_page <- first_page %>% 
  extract_schooling()

first_page %>% 
  write_csv("uncleaned_data/first_page_apps.csv")

