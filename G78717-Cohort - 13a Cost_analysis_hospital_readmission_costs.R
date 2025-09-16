#############################################
# Cost analysis - Hospital readmission costs
# Author: Daisy Yu
# Date: 2023-05-18
# Updated: 2023-06-18
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

# Read in hosptial CMG file
dd <- read_xlsx("R:/DATA/2018-06-20/docs/data_dictionary_discharge-abstracts-database.xlsx",
                 sheet = "hospitalcmg.C") %>%
    filter(!Name %in% "Line Feed") %>%
    select(col_width = Length, col_name = `Name Abbrev`)
files_exp <- paste0("R:/DATA/2018-06-20/hospital/",list.files("R:/DATA/2018-06-20/hospital", pattern="cmg"))
cmg_exp <- map_dfr(files_exp, function(f){
  
  read_fwf(f, 
           col_positions = fwf_widths(widths = dd$col_width, 
                                      col_names = dd$col_name),
           col_types = cols(.default = "c"))
  
})

# Read in dad file
dad <- read.csv("R:/working/G78717-Cohort/data/Raw data/dad_admits_all.csv") %>% 
  as_tibble() %>%
  clean_names() %>%
  mutate(sepdate = ymd(sepdate), 
         addate = ymd(addate),
         tdays = as.numeric(tdays)) %>%
  select(-ver_num, -seqno, -version) # Remove the variables we most likely won't need

# Have CSHS and CPI corresponding to each fiscal year
healthcare_cost <- data.frame(fiscal_year=c(2010:2017),
                              cshs=c(5589,5329,5804,5883,5903,6030,6156,6539),
                              cpi=c(0.909,0.929,0.936,0.934,0.946,0.958,0.977,0.998))

# Find all urgent (non-elective only) eligible hospitalizations
coh_urgent <- cohort %>% 
  select(studyid.dad,sepdate.dad) %>% 
  left_join(dad %>% 
              filter(admit == 'U') %>% 
              rename(readmit_addate=addate,readmit_sepdate=sepdate) %>% 
              select(studyid,readmit_addate,readmit_sepdate,recrdnum), 
            by = c("studyid.dad"='studyid')) %>% 
  filter(readmit_addate >= sepdate.dad & readmit_addate <= sepdate.dad + 30) %>%
  unique() %>% 
  right_join(cohort, by=c('studyid.dad','sepdate.dad')) %>%
  select(studyid.dad,sepdate.dad,readmit_addate,readmit_sepdate,indexyr,recrdnum) %>%
  # Link CMG file to dad
  left_join(cmg_exp %>% 
              mutate(CMGP_RIW=as.numeric(CMGP_RIW)) %>%
              select(STUDYID,RECRDNUM,CMGP_RIW) %>%
              mutate(RECRDNUM=as.numeric(RECRDNUM)) %>% 
              group_by(STUDYID,RECRDNUM) %>% 
              slice_head(n=1), 
            by=c('recrdnum'='RECRDNUM','studyid.dad'='STUDYID')) %>%
  left_join(healthcare_cost,by=c('indexyr'='fiscal_year')) %>%
  # Append CSHS 
  group_by(studyid.dad,sepdate.dad) %>%
  summarise(hosp_readmit_urgent_cost=sum(CMGP_RIW*cshs/cpi)) %>% 
  ungroup() %>%
  mutate(hosp_readmit_urgent_cost=replace_na(hosp_readmit_urgent_cost,0)) 

saveRDS(coh_urgent,file='hosp_readmit_urgent_cost.rds')

# Find all elective only eligible hospitalizations
coh_elective <- cohort %>% 
  select(studyid.dad,sepdate.dad) %>% 
  left_join(dad %>% 
              filter(admit == 'L') %>% 
              rename(readmit_addate=addate,readmit_sepdate=sepdate) %>% 
              select(studyid,readmit_addate,readmit_sepdate,recrdnum), 
            by = c("studyid.dad"='studyid')) %>% 
  filter(readmit_addate >= sepdate.dad & readmit_addate <= sepdate.dad + 30) %>%
  unique() %>% 
  right_join(cohort, by=c('studyid.dad','sepdate.dad')) %>%
  select(studyid.dad,sepdate.dad,readmit_addate,readmit_sepdate,indexyr,recrdnum) %>%
  # Link CMG file to dad
  left_join(cmg_exp %>% 
              mutate(CMGP_RIW=as.numeric(CMGP_RIW)) %>%
              select(STUDYID,RECRDNUM,CMGP_RIW) %>%
              mutate(RECRDNUM=as.numeric(RECRDNUM)) %>% 
              group_by(STUDYID,RECRDNUM) %>% 
              slice_head(n=1), 
            by=c('recrdnum'='RECRDNUM','studyid.dad'='STUDYID')) %>%
  left_join(healthcare_cost,by=c('indexyr'='fiscal_year')) %>%
  # Append CSHS 
  group_by(studyid.dad,sepdate.dad) %>%
  summarise(hosp_readmit_elective_cost=sum(CMGP_RIW*cshs/cpi)) %>% 
  ungroup() %>%
  mutate(hosp_readmit_elective_cost=replace_na(hosp_readmit_elective_cost,0)) 

saveRDS(coh_elective,file='hosp_readmit_elective_cost.rds')








# # Link CMG file to dad
# coh1 <- coh %>% 
#   
# 
# # Append CSHS 
# coh2 <- coh1 %>% 
#   group_by(studyid.dad,sepdate.dad) %>%
#   summarise(hosp_readmit_cost=sum(CMGP_RIW*cshs/cpi)) %>% 
#   ungroup() %>%
#   mutate(hosp_readmit_cost=replace_na(hosp_readmit_cost,0))
# 
# saveRDS(coh2,file='hosp_readmit_cost.rds')




 
  
  




  








