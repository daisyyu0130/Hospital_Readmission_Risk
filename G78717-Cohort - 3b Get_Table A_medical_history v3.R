#############################################
# Get medical history
# Author: Daisy Yu
# Date: 2022-10-13
# Updated: 2022-10-27
#############################################

# libraries
library(tidyverse)
library(data.table)
library(lubridate)

# directory
mydir <- "R:/working/G78717-Cohort/data/merged data/"
setwd(mydir)

cohort <- read.csv(paste0(mydir,"cohort_2022-11-18.csv"))

##########################################################
# Admissions (DAD)
##########################################################
dd_dir <- "R:/DATA/2018-06-20/docs/data_dictionary_discharge-abstracts-database.xlsx"
types <- c("G", "H", "N")
fileDir <- "R:/DATA/2018-06-20/hospital/"

# hospital data
(hosp <- map_dfr(types, function(t){
  
  # get data dictionary
  (dd <- read_xlsx(dd_dir, sheet = paste0("hospital.", t)) %>%
     filter(!Name %in% "Line Feed") %>%
     select(col_width = Length, col_name = `Name Abbrev`))
  
  # list all data files for version of interest
  (fileNames <- paste0(fileDir, 
                       list.files("R:/DATA/2018-06-20/hospital/", pattern = t)))
  
  # read files 
  map_df(fileNames, function(file){
    read_fwf(file, col_positions = fwf_widths(widths = dd$col_width, col_names = dd$col_name),
             col_types = cols(.default = "c")) %>%
      mutate(fileName = file) %>%
      select(fileName, STUDYID, ADDATE, 
             starts_with("SEP"), TDAYS,
             starts_with("DIAG")) %>% 
      filter(STUDYID %in% cohort$studyid.dad) %>% 
      mutate(SEPDATE = as.Date(SEPDATE)) %>% 
      filter(SEPDATE >= "2011-06-01")
  })
  
}))


# for cases with duplicates for studyid, admission date, discharge date, and LOS
# group into single row, combining diagnostic codes
# if different diagnostic codes for given column, e.g. DIAG1 different in two rows
# take the first one
hosp <- hosp %>% 
  group_by(STUDYID, ADDATE, SEPDATE, TDAYS, SEPDISP) %>% 
  summarise(across(everything(), function(x) ifelse(all(is.na(x)), NA, (na.omit(x)[1])))) %>% 
  ungroup()

hosp <- hosp %>%
  mutate(SEPDATE = ymd(SEPDATE), 
         ADDATE = ymd(ADDATE), 
         tlos = as.period(ADDATE %--% SEPDATE),
         los.hosp = as.duration(tlos) / ddays(1)) %>% 
  select(-tlos)


# save file
saveRDS(hosp,paste0(mydir,'dad.rds')) 


##########################################################
# Physician visits (MSP)
##########################################################
# data
fileDir <- "R:/DATA/2018-06-20/msp/"
(fileNames <- c(list.files(fileDir, "2011"),
                list.files(fileDir, "2012"),
                list.files(fileDir, "2013"),
                list.files(fileDir, "2014"),
                list.files(fileDir, "2015"),
                list.files(fileDir, "2016")))

# read in data dictionary
dd_dir <- "R:/DATA/2018-06-20/docs/data_dictionary_medical-services-plan-payment-information-file.xlsx"
dd <- read_xlsx(dd_dir, sheet = "msp.C") %>%
    filter(!Name %in% "Line Feed") %>%
    select(col_width = Length, col_name = `Name Abbrev`)


# number of records in reach file
file.sizes <- c(68927629,89883596,81069453,105703477,83875479,71585816)

# read data in chunks
readRow <- 10000000  ## read 10 million rows at a time
# create chunks to read in file
file.cuts <- lapply(file.sizes, function(x) seq(0,x, by = readRow))

# use for loops - map functions caused memory error
msp_lst <- list()

for(i in 1:length(fileNames)){
  fileName <- fileNames[i]
  file.cut <- file.cuts[[i]]
  print(paste(fileName, "with", file.sizes[i], "records"))
  
  msp_file_lst <- list()
  index <- 1
  getF <- paste0(fileDir, fileName)
  
  for(j in 1:length(file.cut)){
    print(paste("reading chunk", j, "out of", length(file.cut)))
    
    msp_chunk <- tryCatch({
      # read data
      read_fwf(getF, fwf_widths(widths=dd$col_width, col_names=dd$col_name),
               col_types=cols(.default="c"),
               n_max=readRow, skip=file.cut[j]) %>% 
        as_tibble() %>%
        select(STUDYID, servdate, servloc, feeitem, icd9, contains("icd9")) %>% 
        filter(STUDYID %in% cohort$studyid.dad) %>% 
        distinct() # throw away duplicate rows with same studyid, servicedate, and icd codes
    }, error=function(e){
      NULL
    })
    
    if(!is.null(msp_chunk)){
      msp_file_lst[[index]] <- msp_chunk
      index <- index + 1
    } 
    
  }
  
  msp_file <- as_tibble(do.call("rbind", msp_file_lst)) %>% distinct()
  msp_lst[[i]] <- msp_file
}


# bind all years of visits together
msp <- as_tibble(do.call("rbind", msp_lst)) %>% distinct()

# save file
saveRDS(msp,paste0(mydir,'msp.rds'))  


##########################################################
# Get number of hospitalizations in prior year
##########################################################
# discharge date within 365 days, and at least 1 day before event index date
hosp <- readRDS(paste0(mydir,'dad.rds'))  
prior_hosp <- hosp %>% 
    inner_join(cohort, by = c("STUDYID" = "studyid.dad")) %>% 
    filter(as.Date(sepdate.dad) - as.Date(SEPDATE) <= 365 & as.Date(sepdate.dad) - as.Date(SEPDATE) >= 1) %>%
    group_by(STUDYID, sepdate.dad) %>% 
    summarise(num_hosp_prev_year=n(), tdays_prev_year=sum(los.hosp)) %>% 
    right_join(cohort, by = c("STUDYID" = "studyid.dad","sepdate.dad")) %>%
    mutate(num_hosp_prev_year = replace_na(num_hosp_prev_year, 0),
           tdays_prev_year = replace_na(tdays_prev_year, 0),
           tdays_prev_year = tdays_prev_year + los) %>% # this should include days in the index hospitalization
    select(STUDYID, sepdate.dad, num_hosp_prev_year, tdays_prev_year,los)


##############################################################################
# Get number of physician clinic visits in prior year
##############################################################################
msp <- readRDS(paste0(mydir,'msp.rds')) 
prior_clinic <- msp %>% filter(!servloc %in% c('D','E','F','G','H','I')) %>% 
  select(STUDYID, servdate) %>% distinct() %>%
  # msp table rows represent billing items, so only count unique dates as one visit
  mutate(servdate = as.Date(as.character(servdate), format = "%Y%m%d")) %>% 
  inner_join(cohort, by = c("STUDYID" = "studyid.dad")) %>%
  filter(as.Date(sepdate.dad) - as.Date(servdate) <= 365 & as.Date(sepdate.dad) - as.Date(servdate) >= 1) %>% 
  group_by(STUDYID, sepdate.dad) %>% 
  summarise(num_clinic_prev_year = n()) %>% 
  right_join(cohort, by = c("STUDYID" = "studyid.dad","sepdate.dad")) %>% 
  mutate(num_clinic_prev_year = replace_na(num_clinic_prev_year, 0)) %>%
  select(STUDYID, sepdate.dad, num_clinic_prev_year)


##############################################################################
# Get # DAD admissions in previous 1 year for comorbidities
##############################################################################
# read in XW files 
icd9_cm <- readxl::read_xlsx("U:/Readmits/Data/Crosswalks/MVC-ICD - ICD9CM comorbidity XW - 2022-02-21.xlsx")
icd10_cm <- readxl::read_xlsx("U:/Readmits/Data/Crosswalks/MVC-ICD - ICD10CA comorbidity XW - 2022-02-21.xlsx")

# dad data icd10 cols:
### DIAGX1-25
icd10_cols <- paste0("DIAGX",1:25)

# dad data w icd9 cols:
### DIAG1-25
icd9_cols <- paste0("DIAG",1:25) # don't have this in readmits data

get_comorbidity_dad <- function(lst_of_coms, df) {
  get_icd_codes <- lapply(lst_of_coms, function(x) icd10_cm %>% filter(pmh_lab == x) %>% pull(icd10_code))
  for (i in 1:length(lst_of_coms)) {
    name <- icd10_cm %>% filter(pmh_lab == lst_of_coms[i]) %>% pull(pmh_name) %>% unique()
    df[name] <- as.numeric(apply(df[icd10_cols],1,function(x) any(x %in% get_icd_codes[[i]])))
  }
  return(df)
}

comorbids_dad <- hosp %>% 
  inner_join(cohort %>% select(studyid.dad, sepdate.dad), by = c("STUDYID" = "studyid.dad")) %>% 
  filter(as.Date(sepdate.dad) - as.Date(SEPDATE) <= 365 & as.Date(sepdate.dad) - as.Date(SEPDATE) >= 0) %>% 
  select(STUDYID,sepdate.dad,SEPDATE,any_of(icd10_cols)) %>% 
  get_comorbidity_dad(lst_of_coms=unique(icd10_cm$pmh_lab)) %>%
  select(-contains("DIAG")) %>% 
  group_by(STUDYID, sepdate.dad, SEPDATE) %>% summarise(across(everything(), function(x) {ifelse(sum(x) >= 1, 1, 0)})) %>% #if multiple comorbidities of a given group in 1 SEPDATE, just count as 1
  ungroup %>% 
  group_by(STUDYID, sepdate.dad) %>% summarise(across(-SEPDATE, sum)) %>% # sum each comorbidity for each patient
  right_join(cohort %>% select(studyid.dad, sepdate.dad), by = c("STUDYID" = "studyid.dad", "sepdate.dad")) %>% 
  replace(is.na(.), 0)


##############################################################################
# Get # MSP admissions in previous 3 years for comorbidities
# For list of comorbidities see MVC-ICD/5 Manuscripts/1 Cohort/MVC-ICD - tables_coh v3.xlsx
##############################################################################
icd9_cols_msp <- c("icd9", "icd9_1","icd9_2","icd9_3","icd9_4","icd9_5")

get_comorbidity_msp <- function(lst_of_coms, df) {
  get_icd_codes <- lapply(lst_of_coms, function(x) icd9_cm %>% filter(pmh_lab == x) %>% pull(icd9_code))
  for (i in 1:length(lst_of_coms)) {
    name <- icd9_cm %>% filter(pmh_lab == lst_of_coms[i]) %>% pull(pmh_name) %>% unique()
    #print(i);print(name)
    df[name] <- as.numeric(apply(df[icd9_cols_msp],1,function(x) any(x %in% get_icd_codes[[i]])))
  }
  return(df)
}

comorbids_msp <- msp %>% filter(servloc %in% c('A','C','T','M')) %>% select(STUDYID, servdate, icd9_cols_msp) %>% distinct() %>%
  mutate(servdate = as.Date(as.character(servdate), format = "%Y%m%d")) %>% 
  inner_join(cohort %>% select(studyid.dad, sepdate.dad), by = c("STUDYID" = "studyid.dad")) %>%
  filter(as.Date(sepdate.dad) - as.Date(servdate) <= 365 & as.Date(sepdate.dad) - as.Date(servdate) >= 0) %>%    
  select(STUDYID,sepdate.dad,servdate,any_of(icd9_cols_msp)) %>% 
  get_comorbidity_msp(lst_of_coms=unique(icd9_cm$pmh_lab)) %>%
  select(-starts_with('icd')) %>% 
  group_by(STUDYID, sepdate.dad, servdate) %>% summarise(across(everything(), function(x) {ifelse(sum(x) >= 1, 1, 0)})) %>% #if multiple comorbidities of a given group in 1 SEPDATE, just count as 1
  ungroup %>% 
  group_by(STUDYID, sepdate.dad) %>% summarise(across(-servdate, sum)) %>% # sum each comorbidity for each patient
  right_join(cohort %>% select(studyid.dad, sepdate.dad), by = c("STUDYID" = "studyid.dad", "sepdate.dad"))  %>% 
  replace(is.na(.), 0)


##############################################################################
# Save
##############################################################################

comorbids_dad %>% inner_join(prior_hosp) %>% 
  inner_join(comorbids_msp, by = c("STUDYID",'sepdate.dad'),
             suffix = c("_dad", "_msp")) %>% 
  inner_join(prior_clinic, by = c("STUDYID",'sepdate.dad')) %>%
  rename(studyid.dad = STUDYID) %>%
  write_csv(paste0(mydir,'table-A-med-history.csv'))




