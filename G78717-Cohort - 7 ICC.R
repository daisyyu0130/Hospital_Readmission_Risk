#############################################
# Use of ICC to determine if a multilevel model is necessary
# Author: Daisy Yu
# Date: 2023-03-22
# Updated: 2023-03-22
#############################################

# Libraries
pkgs = c('tidyverse', 'lubridate', 'WeightIt', 'data.table','PSweight','dplyr','tableone','readxl','xlsx','fastDummies',
         'car', 'corrplot', 'irr', 'performance','lme4')
lapply(pkgs, library, character.only = TRUE)
mydir <- "R:/working/G78717-Cohort/results"
setwd(mydir)

# Read in the cohort data
cohort <- read.csv('R:/working/G78717-Cohort/data/merged data/cohort_for_modelling_2023-03-22.csv') %>%
  mutate(most_resp_serv = as.factor(most_resp_serv)) 


com <- c('mi','chf', 'pvd', 'cvd', 'dem', 'copd', 'rheum', 'pud', 'liver_mild', 'dm_nc', 'dm_compl', 'paraplegia',
         'renal', 'liver_modsev', 'cancer', 'mets', 'hiv', 'drugs')
med <- colnames(cohort)[352:384] 
mrd <- cohort %>% select(starts_with('Description_')) %>% colnames() 
cov <- c("sex", "age", "hhd_income", "popl_dens", "num_hosp_prev_year", "num_clinic_prev_year", "cci",  # comorbidities + medications
         "num_med", "fiscal_year", "ambulanc", "hosp_size", "most_resp_serv", # most responsible diagnosis
         "icu_stay", "los", "mrp_sex", "grad_exp", "grad_plc", "mrp_spec" )

covform <- paste0(c(cov,com,med,mrd), collapse = "+" )
ps.formula <- formula(paste("G78717_feecode~", covform))


# Task 1
prop_feecode_by_physician <- cohort %>% group_by(respphys.dad) %>%
  summarise(sum = sum(G78717_feecode),n=n()) %>%
  mutate(freq = sum/n)

prop_feecode_by_hospital <- cohort %>% group_by(hosp.dad) %>%
  summarise(sum = sum(G78717_feecode),n=n()) %>%
  mutate(freq = sum/n)

par(mfrow=c(1,2))
hist(prop_feecode_by_physician$freq, 
     main='Histogram illustrating variability in the proportion of \n
     eligible hospitalization exposed to G787171',
     xlab='proportion of eligible hospitalization exposed to G78717', ylab='number of phycisians')

hist(prop_feecode_by_hospital$freq, 
     main='Histogram illustrating variability in the proportion of \n 
     eligible hospitalization exposed to G787171',
     xlab='proportion of eligible hospitalization exposed to G78717', ylab='number of hospitals')


# Task 2
prop_outcome_by_physician <- cohort %>% group_by(respphys.dad) %>%
  summarise(sum = sum(readmit_or_death),n=n()) %>%
  mutate(freq = sum/n)

prop_outcome_by_hospital <- cohort %>% group_by(hosp.dad) %>%
  summarise(sum = sum(readmit_or_death),n=n()) %>%
  mutate(freq = sum/n)

par(mfrow=c(1,2))
hist(prop_outcome_by_physician$freq, 
     main='Histogram illustrating variability in the proportion of \n
     eligible hospitalization with the outcome of interest',
     xlab='proportion of eligible hospitalization with the outcome of interest', ylab='number of phycisians')

hist(prop_outcome_by_hospital$freq, 
     main='Histogram illustrating variability in the proportion of \n
     eligible hospitalization with the outcome of interest',
     xlab='proportion of eligible hospitalization with the outcome of interest', ylab='number of hospitals')


# Task 3
phy_tbl <- cohort %>% group_by(respphys.dad) %>%
  count() %>%
  mutate(prop = case_when(n<=1~'1.<=1',
                          n>1 & n<=2~'2.<=2',
                          n>2 & n<=5~'3.<=5',
                          n>5 & n<=10~'4.<=10',
                          n>10 & n<=50~'5.<=50',
                          n>50 & n<=100~'6.<=100',
                          TRUE~'7.>100')) %>%
  group_by(prop) %>% count() %>% rename(Physician_level=n)


hosp_tbl <- cohort %>% group_by(hosp.dad) %>%
  count() %>%
  mutate(prop = case_when(n<=1~'1.<=1',
                          n>1 & n<=2~'2.<=2',
                          n>2 & n<=5~'3.<=5',
                          n>5 & n<=10~'4.<=10',
                          n>10 & n<=50~'5.<=50',
                          n>50 & n<=100~'6.<=100',
                          TRUE~'7.>100')) %>%
  group_by(prop) %>% count() %>% rename(hopsital_level=n)

tbl <- phy_tbl %>% left_join(hosp_tbl, by='prop')



# Task 4
num_mrp_by_hospital <- cohort %>% group_by(hosp.dad) %>% 
  summarise(sum=n_distinct(respphys.dad))

hist(num_mrp_by_hospital$sum, 
     main='Histogram illustrating variability in the number of \n
     MRP for eligible hospitalization hy hospital',
     xlab='Number of MRP associated', ylab='number of hospitals')


# Task 5: 
# calculate ICC for level 1 = patients, level 2 = physicians
g1_null <- glmer(readmit_or_death~(1|respphys.dad),data=cohort,family='binomial')
#icc <- g1_null@theta[1]^2/(g1_null@theta[1]^2+(3.1415926^2/3)) 
performance::icc(g1_null, by_group=T) # 0.038

# calculate ICC for level 1 = patients, level 2 = hospitals
g2_null <- glmer(readmit_or_death~(1|hosp.dad),data=cohort,family='binomial')
performance::icc(g2_null, by_group=T) # 0.013

# calculate ICC for:
# level 1 = patients 
# level 2 = physicians
# level 3 = hospitals
g3_null <- glmer(readmit_or_death~(1|respphys.dad)+(1|hosp.dad),data=cohort,family='binomial')
performance::icc(g3_null, by_group=T) # physician level: 0.034; hospital level: 0.007


## MOR ##
m1 <- glmer(readmit_or_death~G78717_feecode+(1|respphys.dad),data=cohort,family='binomial')
v <- m1@theta[1]^2
mor <- exp(sqrt(2*v)*qnorm(0.75))





# library('broom')
# glm(readmit_or_death~G78717_feecode,data=cohort,family='binomial') %>%
#   tidy(., exponentiate=T,conf.int=T)
# 
# glm(readmit_or_death~G78717_feecode,data=cohort,family='binomial',weight=w_WeightIt)%>%
#   tidy(., exponentiate=T,conf.int=T)

