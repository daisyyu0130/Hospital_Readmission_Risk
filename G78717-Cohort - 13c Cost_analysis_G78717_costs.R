#############################################
# Cost analysis - G78717 payment costs
# Author: Daisy Yu
# Date: 2023-07-13
# Updated: 2023-07-13
#############################################

# Libraries
pkgs = c('tidyverse','lubridate', 'data.table','PSweight','dplyr','tableone','readxl','xlsx','fastDummies',
         'car', 'corrplot', 'irr', 'performance','lme4', 'janitor')
lapply(pkgs, library, character.only = TRUE)
mydir <- "R:/working/G78717-Cohort/results/"
setwd(mydir)

# Read in the cohort data
cohort <- read.csv('R:/working/G78717-Cohort/data/merged data/cohort_2022-11-18.csv') %>%
  mutate(addate.dad = as.Date(addate.dad), sepdate.dad = as.Date(sepdate.dad))

# Read in MSP data
msp <- readRDS("R:/working/G78717-Cohort/data/merged data/msp.rds")
feecode_xw <- read.csv("R:/working/G78717-Cohort/data/crosswalks/G78717-Cohort - msp_xw from MVC-ICD 2023-06-07.csv") %>%
  mutate(indexyr=as.integer(str_sub(FY,1,4)),
         feeitem=as.character(feeitem),
         feeitem=case_when(nchar(feeitem)==2~paste('000',feeitem,sep=''),
                           nchar(feeitem)==3~paste('00',feeitem,sep=''),
                           nchar(feeitem)==4~paste('0',feeitem,sep=''),
                           nchar(feeitem)==5~feeitem)) 
feecode_paidamt <- read_excel('R:/working/G78717-Cohort/data/crosswalks/feecode_tbl_v2_IR.xlsx')

healthcare_cost <- data.frame(indexyr=c(2010:2017),
                              cpi=c(0.909,0.929,0.936,0.934,0.946,0.958,0.977,0.998))

coh <- cohort %>%
  dplyr::select(studyid.dad,addate.dad,sepdate.dad,indexyr) %>%
  left_join(msp %>% 
              mutate(servdate = as.Date(servdate, format = "%Y%m%d")) %>%
              dplyr::select(STUDYID,servdate,feeitem),
            by=c('studyid.dad'='STUDYID')) %>%
  # Find the all eligible services 
  #filter(feeitem == 78717) %>% 
  filter(servdate >= sepdate.dad & servdate <= sepdate.dad + 3) %>%
  filter(servdate <= sepdate.dad & servdate >= sepdate.dad - 3) %>%
  # Find the price of feeitem
  left_join(feecode_xw %>% dplyr::select(indexyr,feeitem,mode_paidamt), 
            by=c('indexyr','feeitem')) %>%
  mutate(mode_paidamt=replace_na(mode_paidamt,0)) %>% 
  # Link to cpi
  left_join(healthcare_cost,by='indexyr') %>%
  # Sum the costs and divide by cpi
  group_by(studyid.dad,sepdate.dad) %>%
  summarise(G78717_cost=sum(mode_paidamt/cpi)) %>% 
  ungroup() %>%
  right_join(cohort %>% dplyr::select(studyid.dad,sepdate.dad),
             by=c('studyid.dad','sepdate.dad')) %>%
  mutate(G78717_cost=replace_na(G78717_cost,0))

saveRDS(coh,file='G78717_cost.rds')