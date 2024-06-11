

sample <- readxl::read_excel("data/los_angeles/academy/academy_17_18.xlsx", col_types = "text") %>% 
  janitor::clean_names() %>% 
  mutate(across(starts_with("date"), ~janitor::excel_numeric_to_date(. %>% 
                                                                       as.numeric)))


sample %>% 
  mutate(across(-c("recruit_class", "date_commenced", "date_graduated"), ~ as.double(.))) %>% 
  mutate(total_c = female_commenced + male_commenced,
         total_g = female_graduate + male_graduate) %>% 
  relocate(total_commenced, total_c, total_graduate, total_g)
