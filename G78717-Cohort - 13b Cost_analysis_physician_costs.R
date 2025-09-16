#############################################
# Cost analysis - Physician costs
# Author: Daisy Yu
# Date: 2023-05-18
# Updated: 2023-06-09
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
  select(studyid.dad,addate.dad,sepdate.dad,indexyr) %>%
  left_join(msp %>% 
              mutate(servdate = as.Date(servdate, format = "%Y%m%d")) %>%
              select(STUDYID,servdate,feeitem),
            by=c('studyid.dad'='STUDYID')) %>%
  # Find the all eligible services 
  filter(servdate >= sepdate.dad & servdate <= sepdate.dad + 30) %>%
  # Find the price of feeitem
  left_join(feecode_xw %>% select(indexyr,feeitem,mode_paidamt), 
            by=c('indexyr','feeitem')) %>%
  mutate(mode_paidamt=replace_na(mode_paidamt,0)) %>% 
  # Link to cpi
  left_join(healthcare_cost,by='indexyr') %>%
  # Sum the costs and divide by cpi
  group_by(studyid.dad,sepdate.dad) %>%
  summarise(physician_cost=sum(mode_paidamt/cpi)) %>% 
  ungroup() %>%
  right_join(cohort %>% select(studyid.dad,sepdate.dad),
             by=c('studyid.dad','sepdate.dad')) %>%
  mutate(physician_cost=replace_na(physician_cost,0))

saveRDS(coh,file='physician_cost.rds')




# feeitem_no_paidamt_v1 <- coh %>%
#   filter(is.na(mode_paidamt)) %>%
#   select(feeitem,indexyr) %>%
#   group_by(feeitem,indexyr) %>%
#   count() %>%
#   arrange(desc(n)) %>%
#   pivot_wider(names_from=indexyr,values_from=n,names_prefix='year') %>%
#   mutate(Total=sum(c(year2012,year2013,year2014,year2015,year2016))) %>%
#   mutate(year2012=if_else(year2012<5,0,year2012),
#          year2013=if_else(year2013<5,0,year2013),
#          year2014=if_else(year2014<5,0,year2014),
#          year2015=if_else(year2015<5,0,year2015),
#          year2016=if_else(year2016<5,0,year2016)) %>%
#   as.data.frame()
# 
# feeitem_no_paidamt_v2 <- coh %>% 
#   filter(is.na(mode_paidamt)) %>% 
#   select(feeitem,indexyr) %>% 
#   group_by(feeitem,indexyr) %>%
#   count() %>%
#   arrange(desc(n)) %>%
#   pivot_wider(names_from=indexyr,values_from=n,names_prefix='year') %>%
#   mutate(Total=sum(c(year2012,year2013,year2014,year2015,year2016)),
#          year2012=if_else(year2012<5,100000,year2012),
#          year2013=if_else(year2013<5,100000,year2013),
#          year2014=if_else(year2014<5,100000,year2014),
#          year2015=if_else(year2015<5,100000,year2015),
#          year2016=if_else(year2016<5,100000,year2016)) %>%
#   as.data.frame() 
# 
# write.xlsx(feeitem_no_paidamt_v1,file='feecode_tbl_v1.xlsx',row.names = F)
# write.xlsx(feeitem_no_paidamt_v2,file='feecode_tbl_v2.xlsx',row.names = F)

# Create new crosswalk file
feecode_xw_new <- feecode_xw %>% mutate(imputed=NA)
need_imputed_feecode <- feecode_xw %>% group_by(feeitem) %>% count() %>% filter(n < 5) %>% pull(feeitem)
for (i in need_imputed_feecode) {
  df <- feecode_xw_new %>% filter(feeitem==i)
  if(!2012 %in% df$indexyr) {
    if(feecode_paidamt %>% filter(feeitem==i) %>% nrow()==0) {impute_val <- 0}
    else if(is.na(feecode_paidamt %>% filter(feeitem==i) %>% pull(feeamt2015))) {impute_val <- 0}
    else {impute_val <- feecode_paidamt %>% filter(feeitem==i) %>% pull(feeamt2015)}
    feecode_xw_new <- rbind(feecode_xw_new,c('2012-13',i,NA,NA,NA,2012,impute_val))
  }
  if(!2013 %in% df$indexyr) {
    if(feecode_paidamt %>% filter(feeitem==i) %>% nrow()==0) {impute_val <- 0}
    else if(is.na(feecode_paidamt %>% filter(feeitem==i) %>% pull(feeamt2015))) {impute_val <- 0}
    else {impute_val <- feecode_paidamt %>% filter(feeitem==i) %>% pull(feeamt2015)}
    feecode_xw_new <- rbind(feecode_xw_new,c('2013-14',i,NA,NA,NA,2013,impute_val))
  }
  if(!2014 %in% df$indexyr) {
    if(feecode_paidamt %>% filter(feeitem==i) %>% nrow()==0) {impute_val <- 0}
    else if(is.na(feecode_paidamt %>% filter(feeitem==i) %>% pull(feeamt2015))) {impute_val <- 0}
    else {impute_val <- feecode_paidamt %>% filter(feeitem==i) %>% pull(feeamt2015)}
    feecode_xw_new <- rbind(feecode_xw_new,c('2014-15',i,NA,NA,NA,2014,impute_val))
  }
  if(!2015 %in% df$indexyr) {
    if(feecode_paidamt %>% filter(feeitem==i) %>% nrow()==0) {impute_val <- 0}
    else if(is.na(feecode_paidamt %>% filter(feeitem==i) %>% pull(feeamt2015))) {impute_val <- 0}
    else {impute_val <- feecode_paidamt %>% filter(feeitem==i) %>% pull(feeamt2015)}
    feecode_xw_new <- rbind(feecode_xw_new,c('2015-16',i,NA,NA,NA,2015,impute_val))
  }
  if(!2016 %in% df$indexyr) {
    if(feecode_paidamt %>% filter(feeitem==i) %>% nrow()==0) {impute_val <- 0}
    else if(is.na(feecode_paidamt %>% filter(feeitem==i) %>% pull(feeamt2015))) {impute_val <- 0}
    else {impute_val <- feecode_paidamt %>% filter(feeitem==i) %>% pull(feeamt2015)}
    feecode_xw_new <- rbind(feecode_xw_new,c('2016-17',i,NA,NA,NA,2016,impute_val))
  }
}

feecode_xw_new <- feecode_xw_new %>% 
  mutate(paidamt = coalesce(mode_paidamt,imputed))

write.xlsx(feecode_xw_new,file='msp_feecode_price_with_imputation.xlsx',row.names = F)

