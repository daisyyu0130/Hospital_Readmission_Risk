#############################################
# Get medication history
# Author: Daisy Yu
# Date: 2022-10-27
# Updated: 2022-10-27
#############################################
# libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(readxl)


# tableA cohort
cohort <- read.csv("R:/working/G78717-COhort/data/merged data/cohort_2022-11-18.csv") %>%
  mutate(addate.dad = as.Date(addate.dad))


##########################################################
# Get number of total medications active at baseline
# filter data such that prescription service dates fall between admission date - 90 days and admission date
# admission date - 90 <= service date < admission date
pnet <- read.csv('R:/working/G78717-COhort/data/raw data/pnet_all.csv') %>%
    mutate(DE.SRV_DATE = as.Date(DE.SRV_DATE))

# Get number of medications prescribed active at baseline 
pharm_active <- pnet %>%  
  inner_join(cohort %>% select(studyid.dad,addate.dad),by = c("STUDYID" = "studyid.dad")) %>% 
  filter((addate.dad > DE.SRV_DATE) & (addate.dad <= DE.SRV_DATE + DE.DSPD_DAYS_SPLY))

num_active_baseline <- pharm_active %>%
  select(STUDYID,addate.dad,DE.SRV_DATE,HP.DIN_PIN) %>% 
  distinct() %>%
  group_by(STUDYID,addate.dad) %>%
  summarise(num_active_med = n()) %>% 
  right_join(cohort %>% select(studyid.dad, addate.dad),by = c("STUDYID" = "studyid.dad", "addate.dad")) %>%
  mutate(num_active_med = replace_na(num_active_med, 0)) %>%
  select(STUDYID,addate.dad,num_active_med)

# Read in XWs file to get dinpin categories
pnet_xw <- read_excel("R:/working/G78717-Cohort/data/crosswalks/G78717-Cohort - PNet XW (from MedGrp.slim) - 2022-11-16.xls")

# Get number of medications prescribed within 90 days of admission date
get_medications <- function(lst_of_meds, df) {
  get_din_pin <- lapply(lst_of_meds,function(x) pnet_xw %>% filter(HIVE_GRP.desc == x) %>% pull(DIN_PIN))
  for (i in 1:length(lst_of_meds)) {
    name <- lst_of_meds[i]
    df[name] <- as.numeric(df$HP.DIN_PIN %in% get_din_pin[[i]])
  } 
  return(df)
}

meds_baseline <- pnet %>% inner_join(cohort %>% select(studyid.dad,addate.dad),by = c("STUDYID" = "studyid.dad")) %>%  
  filter((addate.dad > DE.SRV_DATE) & (addate.dad <= DE.SRV_DATE + 90)) %>% 
  select(STUDYID,addate.dad,DE.SRV_DATE,HP.DIN_PIN) %>% 
  distinct() %>% 
  get_medications(lst_of_meds=unique(pnet_xw$HIVE_GRP.desc)) %>%
  select(-HP.DIN_PIN) %>% 
  group_by(STUDYID,addate.dad) %>%
  summarise(across(-DE.SRV_DATE, sum)) %>% 
  right_join(cohort %>% select(studyid.dad,addate.dad),by = c("STUDYID" = "studyid.dad","addate.dad")) %>% 
  replace(is.na(.), 0)


##########################################################
# Save
meds_baseline %>% inner_join(num_active_baseline, by=c('STUDYID', 'addate.dad')) %>%
  write_csv("R:/working/G78717-COhort/data/merged data/table-A-medications.csv")

