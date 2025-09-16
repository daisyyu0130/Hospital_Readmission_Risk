#############################################
# Create response variable
# Author: Daisy Yu
# Date: 2023-01-12
# Updated: 2023-01-12
#############################################

# Libraries
pkgs = c('tidyverse', 'lubridate', 'data.table', 'dplyr','tableone','readxl','xlsx','fastDummies')
lapply(pkgs, library, character.only = TRUE)
mydir <- "R:/working/G78717-Cohort/data/merged data/"
setwd(mydir)

  
# Read in dad
cohort <- read.csv('cohort_2022-11-18.csv') %>%
  mutate(addate.dad = as.Date(addate.dad), sepdate.dad = as.Date(sepdate.dad))
hosp <- readRDS('dad.rds')

# Get response variable: readmission within 30 days of discharge
# Get response variable: death within 30 days of discharge
cohort_with_response <- cohort %>% select(studyid.dad, sepdate.dad) %>% 
  left_join(hosp %>% select(STUDYID, ADDATE) %>% mutate(readmit_date=ADDATE), 
            by = c("studyid.dad"='STUDYID')) %>% 
  filter(readmit_date >= sepdate.dad & readmit_date <= sepdate.dad + 30) %>%
  group_by(studyid.dad, sepdate.dad) %>% 
  slice_min(readmit_date) %>%
  select(-ADDATE) %>%
  unique() %>% 
  right_join(cohort, by=c('studyid.dad','sepdate.dad')) %>%
  mutate(readmit = case_when(is.na(readmit_date) ~ 0, 
                             TRUE ~ 1),
         death = case_when(is.na(deathdt.vs) ~ 0,
                           as.Date(deathdt.vs) <= sepdate.dad + 30 ~ 1,
                           TRUE ~ 0),
         readmit_or_death = as.numeric(readmit|death)) 


# Save file
write.csv(cohort_with_response, file='cohort_with_response_2023-03-01.csv', row.names = FALSE)




    






