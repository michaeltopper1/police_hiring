library(tidyverse)
library(pdftools)

files <- list.files("data/louisville/", pattern = "^POLICE")

pages <- map(files, ~pdf_text(paste0("data/louisville/", .))) %>% unlist() %>% 
  as.list()

page <- pages[[1]] %>% 
  str_split("\n") %>% 
  unlist() %>% 
  str_trim() %>% 
  str_squish() %>% 
  pluck(6) 

page_numbers<- 1

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

page_numbers <- page_numbers %>% unique()

pages_names <- pages[page_numbers]

# function for getting first line -----------------------------------------
## this function extracts the first line of each PDF page
grab_first_line <- function(data) {
  first_line <- data %>% 
    str_split("\n") %>% 
    unlist() %>% 
    pluck(1) %>% 
    str_squish()
}


# get first line ----------------------------------------------------------
## the first line contains the name, id, and date applied
## putting into a tibble
pdfs <- map(pages_questions, grab_first_line) %>% 
  unlist() %>% 
  as_tibble() 



# putting into columns using delimiters -----------------------------------
## first replacing PERSON ID and RECEIVED with nothing since I want to use
## : as a delimiiter
pdfs <- pdfs %>% 
  mutate(value = value %>% 
           str_replace("Person ID", "") %>% 
           str_replace("Received", "")) %>% 
  separate_wider_delim(cols = everything(),
                       delim = ":",
                       names = c("name", "id", "date", "time"),
                       too_few= "align_start",
                       cols_remove = F)



# dropping all NAs and uniting the time and date columns back toge --------

pdfs <- pdfs %>% 
  drop_na(id) %>%
  unite(col = "date", date, time, sep = ":") %>% 
  mutate(across(everything(), ~str_trim(.))) %>% 
  separate_wider_delim(col = name,
                 delim = " ",
                 names = c("first_name", "last_name"),
                 too_few = "align_start",
                 too_many = "merge") %>% 
  mutate(date = mdy_hm(date))
  

# getting other info ------------------------------------------------------


# questionaire info -------------------------------------------------------

pages_questions[[1]] %>% 
  

map(pages_questions, grab_first_line) %>% 
  unlist() %>% 
  as_tibble() 
