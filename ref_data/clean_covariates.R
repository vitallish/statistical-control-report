# Script to 

library(readr)
library(dplyr)
library(lubridate)

d_cov_names <- readr::read_lines('ref_data/sick.names.txt', skip = 9)
d_cov_names <- c(gsub(":.+", "", d_cov_names), "diagnosis")

center_func <-function(x){
  UseMethod("center_func")
}
center_func.numeric <- function(x){
  mean(x, na.rm = TRUE)
}

center_func.character <- function(x){
  
  names(sort(table(x), decreasing = TRUE)[1])
  
}

center_func.logical <- function(x){
  
  names(sort(table(x), decreasing = TRUE)[1])
  
}

d_cov <- read_csv('ref_data/sick.data.txt',
                  col_names = d_cov_names,
                  col_types = list(age = col_integer(),
                                   TSH = col_double(),
                                   T3  = col_double(),
                                   TT4 = col_double(),
                                   T4U = col_double(),
                                   FTI = col_double(),
                                   TBG = col_character())) %>% 
  janitor::clean_names() %>% 
  select(-contains('measured'), -tbg, -diagnosis) %>% 
  mutate_at(vars(on_thyroxine:psych),
            funs(case_when(
              . == "t" ~ TRUE,
              . == "f" ~ FALSE
            ))) %>% 
  mutate(sex = if_else(sex == "?", NA_character_, sex)) %>% 
  mutate_if(is.numeric, 
            funs(ifelse(. >= quantile(., probs = .01, na.rm = T) & 
                          . <= quantile(., probs = .99, na.rm = T),
                        .,
                        NA))) %>% 
  mutate_all(funs(ifelse(is.na(.), center_func(.), .))) %>% 
  mutate_at(vars(referral_source, sex), as.factor)

save(d_cov, file = "ref_data/sick_data.Rdata")
