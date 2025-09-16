##################################################
# Linking MSP data with studyIDs
# Author: Daisy Yu
# Date: 2022-08-25
# Updated: 2022-09-15
#################################################

# libraries
library(tidyverse)
library(readxl)
library(lubridate)
setwd("R:/working/G78717-Cohort/data/Merged data/")

# Read in cohort
id <- read.csv("U:/Readmits/Data/Merged data/cohortID_2022-11-18.csv")
id <- id %>%
  mutate(sepdate.dad = ymd(sepdate.dad))


filelist <- c('msp2012','msp2013','msp2014','msp2015','msp2016')

map_dfr(filelist,function(f){
  # Read in msp data
  msp <- read.csv(paste0("R:/working/G78717-Cohort/data/raw data/",f,'.csv')) %>%
    as_tibble() %>%
    clean_names() %>%
    rename(pracnum = `pracnum.`,
           paynum = `paynum.`)  %>%
    mutate(servdate = ymd(servdate), # convert to date and then link by dates
           datayr = as.integer(datayr)) %>% # Convert datayr.msp into integer (because indexyr is an integer)
    select(-version, -seqno) 
  
  # Add .df suffix to all vars
  colnames(msp) <-  paste0(colnames(msp), ".msp")
  
  # needs to be joined by 
  a <- id %>% 
    left_join(msp2016, by=c('studyid.dad' = 'studyid.msp',
                            'sepdate.dad' = 'servdate.msp'))
  
  # a %>% group_by(studyid.dad, sepdate.dad) %>% summarise(n=n()) %>% arrange(desc(n))
  # even though it's matched by sepdate now, multiple rows due to different servecode and feeitem
  
  
  # one row linked to multiple rows
  write.csv(a, paste0("coh_",f,".csv"), row.names = FALSE)
})










