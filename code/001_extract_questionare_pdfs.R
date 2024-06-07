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
page <- pages[[3]] %>% 
  str_split("\n") %>% 
  unlist() %>% 
  str_trim() %>% 
  str_squish() %>% 
  pluck(6) 

## first page that starts with this
page_numbers <- 3

## finding where the other pages are that have Agency-Wide Questions
for (i in 1:length(pages)){
  page <- pages[[i]] %>% 
    str_split("\n") %>% 
    unlist() %>% 
    str_trim() %>% 
    str_squish() %>% 
    pluck(6)
  if (!is.null(page)){
    if (str_detect(page,"Agency-Wide Questions")){
      page_numbers <- c(i, page_numbers)
    }
  }
}

## getting the distinct pages only. Gets rid of the duplicate 3
page_numbers <- page_numbers %>% unique()

## filtering to only the agency-wide questions
pages_questions <- pages[page_numbers]


############ FUNCTIONS TO HELP CLEAN AND EXTRACT ##########################
# push question numbers to correct line -----------------------------------

reformat_questions <- function(string) {
  cleaned_string <- string %>% 
    str_replace("Did you list", "9. Did you list") %>% 
    str_replace("Are you related", "10. Are you related") %>% 
    str_replace("I understand that only the education", "11. I understand that only the education")
  return(cleaned_string)
}


# gets rid of lines with only 11. 10. 9. etc ------------------------------
withdraw_single_lines <- function(string){
  string <- string[!string %in% c("11.", "10.", "9.")]
  return(string)
}


# makes the questions appear on one line ----------------------------------

reformat_question_11 <- function(string) {
  string <- string %>% 
    str_replace("I understand that only the education.+",
                "I understand that only the education, experience, and training I list in, or attach to, this application will be used to determine whether I meet the minimum requirements of the job.") 
  return(string)
  }

reformat_question_10 <- function(string){
  string <- string %>% 
    str_replace("If so, please provide.+", "If so, please provide the name(s), the department(s) in which they work and your relationship to the employee(s).")
  return(string)
  }

reformat_question_9 <- function(string) {
  string <- string %>% 
    str_replace("Did you list all employers.+",
                "Did you list all employers you have had in the last 10 years? If not, list all previous employers in the last 10 years not listed on your application or resume, including dates of employment.")
  return(string)
  }



# removes lines with half-questions  -------------------------
remove_halfquestion_lines <- function(vector) {
  vector <- vector[!vector %in% c("determine whether I meet the minimum requirements of the job.",
                                   "department(s) in which they work and your relationship to the employee(s).",
                                   "listed on your application or resume, including dates of employment.")]
  return(vector)
}


# extract questions -------------------------------------------------------

extract_q1 <- function(x) {
  x <- x %>% extract(value,
                into = "currently_employed_by_louisville",
                "Are you currently employed by Louisville Metro Government\\?(.+)2\\. If yes",
                remove = F)
  return(x)
}

extract_q2 <- function(x){
  x <- x %>% extract(value, into = "currently_employed_by_louisville_department",
                     "If yes, what department do you work for\\?(.+)3\\. Were you previously employed by Louisville",
                     remove = F) 
  return(x)
}

extract_q3 <- function(x){
  x <- x %>% extract(value, into = "previously_employed_by_louisville",
                     "by the City of Louisville, or by Jefferson County\\?(.+)4\\. If yes,",
                     remove = F) 
  return(x)
}

extract_q4 <- function(x) {
  x <- x %>% extract(value, into = "previous_louisville_job",
                     "If yes, please list the Location.Job Title and the Employment Dates(.+)5\\. Have you completed High School",
                     remove = F)
  return(x)
}

extract_q5 <- function(x){
  x <- x %>% extract(value, into = "completed_highschool",
                     "earned the equivalent \\(GED\\)\\?(.+)6\\. Have you attended any Vocational",
                     remove = F) 
  return(x)
}

extract_q6 <- function(x){
  x <-  x %>% extract(value, into = "attended_vocational_business_or_military",
            " Military training or school\\?(.+)7\\. If yes, please give",
            remove = F)
  return(x)
}

extract_q7 <- function(x) {
  x <-  x %>% extract(value, into = "vocational_business_or_military_school",
                      "School Name, City, and State\\.(.+)8\\. Where did you first hear about this opportunity",
                      remove = F) 
  return(x)
}

extract_q8 <- function(x) {
  x <- x %>% extract(value, into = "how_hear_about_opportunity",
                     "hear about this opportunity\\?(.+)9\\. Did you list all employers",
                     remove = F)
  return(x)
}

extract_q9 <- function(x) {
  x <- x %>% extract(value, into = "list_all_employers_not_on_cv",
                     "application or resume, including dates of employment\\.(.+)10\\. Are you related to anyone",
                     remove = F) 
  return(x)
}

extract_q10 <- function(x){
  x <- x %>% extract(value, into = "related_to_louisville_employee",
                     "work and your relationship to the employee\\(s\\)\\.(.+)11. I understand that only the education",
                     remove = F)
  return(x)
}

extract_q11 <- function(x){
  x <- x %>% extract(value, into = "understand_app",
                     "I meet the minimum requirements of the job.(.+)12\\. Have you been laid off",
                     remove = F)
  return(x)
}

extract_q12 <- function(x) {
  x <- x %>% extract(value, into = "laid_off_from_louisville_12month",
                     "laid off from Metro Government within the last 12 months\\?(.+)")
  return(x)
}


extract_name <- function(x){
  x <- x %>% extract(value, into = "name",
                     "(.+)Person ID", remove = F)
  return(x)
}

extract_id <- function(x){
  x <- x %>% extract(value, into = "id",
                     "Person ID:(.+)Received",
                     remove = F)
  return(x)
}

extract_date <- function(x){
  x <- x %>% extract(value, into = "date_received",
                     "Received:(.+)Agency",
                     remove = F)
  return(x)
}




# reformat and put into tibble --------------------------------------------

questionaire <- map_df(pages_questions, ~.x %>% 
                         reformat_questions() %>% 
                         withdraw_single_lines() %>% 
                         reformat_question_9() %>% 
                         reformat_question_10() %>% 
                         reformat_question_11() %>% 
                         remove_halfquestion_lines() %>% 
                         paste0(collapse = " ") %>% as_tibble())


# extract all questions ---------------------------------------------------

questionaire <- questionaire %>%
  mutate(value = str_squish(value) %>% 
           str_replace_all("\n", " ")) %>% 
  extract_name() %>% 
  extract_date() %>% 
  extract_id() %>% 
  extract_q1() %>% 
  extract_q2() %>% 
  extract_q3() %>% 
  extract_q4() %>% 
  extract_q5() %>% 
  extract_q6() %>% 
  extract_q7() %>% 
  extract_q8() %>% 
  extract_q9() %>% 
  extract_q10() %>% 
  extract_q11() %>% 
  extract_q12() %>% 
  mutate(list_all_employers_not_on_cv = list_all_employers_not_on_cv %>% str_replace("9. listed on your application or resume, including dates of employment.",
                                                          "")) %>% 
  mutate(related_to_louisville_employee = related_to_louisville_employee %>% 
           str_replace("10. department\\(s\\) in which they work and your relationship to the employee\\(s\\).", "")) %>% 
  mutate(understand_app = understand_app %>% str_replace("11. determine whether I meet the minimum requirements of the job.", "")) 


questionaire %>% 
  write_csv("uncleaned_data/questionaire_extracted.csv")


