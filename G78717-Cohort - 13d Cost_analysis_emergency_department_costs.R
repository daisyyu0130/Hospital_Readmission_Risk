#############################################
# Cost analysis - Emergency department costs
# Author: Daisy Yu
# Date: 2023-05-18
# Updated: 2023-06-29
#############################################

# Libraries
pkgs = c('tidyverse', 'lubridate', 'data.table','PSweight','dplyr','tableone','readxl','xlsx','fastDummies',
         'car', 'corrplot', 'irr', 'performance','lme4', 'janitor')
lapply(pkgs, library, character.only = TRUE)
mydir <- "R:/working/G78717-Cohort/results/"
setwd(mydir)

# Read in the cohort data
cohort <- read.csv('R:/working/G78717-Cohort/data/merged data/cohort_2022-11-18.csv') %>%
  mutate(addate.dad = as.Date(addate.dad), sepdate.dad = as.Date(sepdate.dad))

# Read in NARCS file
dd <- read_xlsx("R:/DATA/2018-06-29/docs/data_dictionary_national-ambulatory-care-reporting-system-201213-onwards.xlsx",
                sheet = "nacrs.A") %>%
  filter(!Name %in% "Line Feed") %>%
  select(col_width = Length, col_name = `Name Abbrev`)

nacrs <- read_fwf("R:/DATA/2018-06-29/nacrs/nacrs_rpt.dat.gz",
                  col_positions = fwf_widths(widths = dd$col_width,
                                             col_names = dd$col_name),
                  col_types = cols(.default = "c"))

# Read in CACS files
files_cacs <- paste0("R:/working/G78717-Cohort/data/CACS XW tables/",
                     list.files("R:/working/G78717-Cohort/data/CACS XW tables/", pattern="CACS_ICD10CA_CODE_FINDER"))
cacs <- map_dfr(files_cacs, function(f){
  df <- read.delim(f)
  df$indexyr <- as.integer(paste0('20',str_sub(f,71,72)))
  return(df)
}) 

files_cacs_base <- paste0("R:/working/G78717-Cohort/data/CACS XW tables/",
                     list.files("R:/working/G78717-Cohort/data/CACS XW tables/", pattern="CACS_BASE_RIW"))
cacs_base <- map_dfr(files_cacs_base, function(f){
  read.delim(f)
}) 


# Have CSHS and CPI corresponding to each fiscal year
healthcare_cost <- data.frame(fiscal_year=c(2010:2017),
                              cshs=c(5589,5329,5804,5883,5903,6030,6156,6539),
                              cpi=c(0.909,0.929,0.936,0.934,0.946,0.958,0.977,0.998))

# Find all eligible emergency visits
coh <- cohort %>% 
  select(studyid.dad,addate.dad,sepdate.dad) %>% 
  left_join(nacrs,by=c('studyid.dad'='NACRS.STUDYID')) %>%
  mutate(NACRS.TRIAGEDATETIME = as.Date(NACRS.TRIAGEDATETIME)) %>%
  filter(NACRS.TRIAGEDATETIME >= sepdate.dad & NACRS.TRIAGEDATETIME <= sepdate.dad + 30) 
  # right_join(cohort %>% 
  #              select(studyid.dad,addate.dad,sepdate.dad), 
  #            by=c('studyid.dad','sepdate.dad')) 

coh1 <- coh %>%
  # step 1
  mutate(NACRS.AGEYRS=as.numeric(NACRS.AGEYRS),
         cacs_agecat=case_when(0<=NACRS.AGEYRS & NACRS.AGEYRS<=7 ~'G',
                               8<=NACRS.AGEYRS & NACRS.AGEYRS<=17 ~'H',
                               18<=NACRS.AGEYRS & NACRS.AGEYRS<=59 ~'R',
                               60<=NACRS.AGEYRS ~'S',
                               is.na(NACRS.AGEYRS) ~'R'),
  # step 2 
         cacs_partition=case_when(addate.dad == NACRS.TRIAGEDATETIME | addate.dad == NACRS.TRIAGEDATETIME + 1 ~'A',
                                 TRUE ~'D'),
  # step 3 
         mac='EV') %>%
  # step 4: missing cacs age category/no value for cacs GENDER
  mutate(indexyr = as.integer(format(as.Date(NACRS.TRIAGEDATETIME, format="%Y-%m-%d"), "%Y"))) %>%
  left_join(cacs,by=c('cacs_partition'='PARTITION','mac'='MAC','indexyr'='indexyr','NACRS.EDDIAG1'='DIAGNOSIS.CODE')) %>%
  # step 5:
  mutate(CACS_new = case_when(is.na(CACS)~'B999', TRUE~CACS)) %>%
  left_join(cacs_base %>% filter(CACS.ANAESTHETIC.CODE==8), by=c('cacs_agecat'='AGE.GROUP','CACS_new'='CACS'))

# Impute missing values in BASE.RIW
df_to_imp <- coh1 %>% select(BASE.RIW,indexyr,cacs_agecat,cacs_partition,CACS_new)
library('mice')
imp <- mice(df_to_imp,m=5,method='pmm')
res <- data.frame(complete(imp,action='repeated',include=T)[,1:6],df_to_imp[,2:5]) %>% 
  rowwise() %>%
  mutate(BASE.RIW.avg = mean(BASE.RIW.1,BASE.RIW.2,BASE.RIW.3,BASE.RIW.4,BASE.RIW.5))

# Calculate the ED visits costs
coh2 <- coh1 %>% 
  mutate(BASE.RIW.imp=res$BASE.RIW.avg) %>% 
  select(studyid.dad,sepdate.dad,BASE.RIW.imp,indexyr) %>% 
  right_join(cohort %>%
               select(studyid.dad,addate.dad,sepdate.dad),
             by=c('studyid.dad','sepdate.dad')) %>%
  left_join(healthcare_cost,by=c('indexyr'='fiscal_year')) %>% # Append CSHS 
  group_by(studyid.dad,sepdate.dad) %>%
  summarise(ed_vists_cost=sum(BASE.RIW.imp*cshs/cpi)) %>% 
  ungroup() %>%
  mutate(ed_vists_cost=replace_na(ed_vists_cost,0))


saveRDS(coh2,file='ed_visits_cost.rds')











