#############################################
# Cost analysis - Table F
# Author: Daisy Yu
# Date: 2023-06-29
# Updated: 2023-07-21
#############################################

# Libraries
pkgs = c('tidyverse', 'lubridate', 'data.table','PSweight','dplyr','tableone','readxl','xlsx','fastDummies',
         'car', 'corrplot', 'irr', 'performance','lme4', 'janitor','sandwich')
lapply(pkgs, library, character.only = TRUE)
mydir <- "R:/working/G78717-Cohort/results/"
setwd(mydir)


# Read in the cohort data
cohort <- read.csv('R:/working/G78717-Cohort/data/merged data/cohort_for_modelling_2023-03-22.csv') %>%
  mutate(addate.dad = as.Date(addate.dad),
         sepdate.dad = as.Date(sepdate.dad),
         most_resp_serv = as.factor(most_resp_serv),
         surgcase2.dad = case_when(surgcase2.dad=='Y'~'Yes',TRUE~'No'))

# Read in costs
hosp_readmit_urgent_cost <- readRDS(file='hosp_readmit_urgent_cost.rds')
hosp_readmit_elective_cost <- readRDS(file='hosp_readmit_elective_cost.rds')
ed_visits_cost <- readRDS(file='ed_visits_cost.rds')
physician_cost <- readRDS(file='physician_cost.rds')

coh_final <- cohort %>% 
  left_join(hosp_readmit_urgent_cost,by=c('studyid.dad','sepdate.dad')) %>%
  left_join(hosp_readmit_elective_cost,by=c('studyid.dad','sepdate.dad')) %>%
  left_join(ed_visits_cost,by=c('studyid.dad','sepdate.dad')) %>%
  left_join(physician_cost,by=c('studyid.dad','sepdate.dad')) %>%
  rowwise() %>% 
  mutate(total_cost = sum(hosp_readmit_urgent_cost,hosp_readmit_elective_cost,ed_vists_cost,physician_cost),
         total_cost_non_elective = sum(hosp_readmit_urgent_cost,ed_vists_cost,physician_cost)) 

###### Get descriptive statistics ######
summary(coh_final$hosp_readmit_urgent_cost)
summary(coh_final$hosp_readmit_elective_cost)
summary(coh_final$physician_cost)
summary(coh_final$ed_vists_cost)
summary(coh_final$total_cost)
summary(coh_final$total_cost_non_elective)

# Stratify by exposure
summary(coh_final[coh_final$G78717_feecode==0,]$hosp_readmit_urgent_cost)
summary(coh_final[coh_final$G78717_feecode==0,]$hosp_readmit_elective_cost)
summary(coh_final[coh_final$G78717_feecode==0,]$physician_cost)
summary(coh_final[coh_final$G78717_feecode==0,]$ed_vists_cost)
summary(coh_final[coh_final$G78717_feecode==0,]$total_cost)
summary(coh_final[coh_final$G78717_feecode==0,]$total_cost_non_elective)

summary(coh_final[coh_final$G78717_feecode==1,]$hosp_readmit_urgent_cost)
summary(coh_final[coh_final$G78717_feecode==1,]$hosp_readmit_elective_cost)
summary(coh_final[coh_final$G78717_feecode==1,]$physician_cost)
summary(coh_final[coh_final$G78717_feecode==1,]$ed_vists_cost)
summary(coh_final[coh_final$G78717_feecode==1,]$total_cost)
summary(coh_final[coh_final$G78717_feecode==1,]$total_cost_non_elective)

# Add 1 dollar to costs
cohort_add_one_cost <- coh_final %>% 
  mutate(hosp_readmit_urgent_cost = hosp_readmit_urgent_cost + 1,
         hosp_readmit_elective_cost = hosp_readmit_elective_cost + 1,
         physician_cost = physician_cost + 1,
         ed_vists_cost = ed_vists_cost + 1,
         total_cost = total_cost + 1,
         total_cost_non_elective = total_cost_non_elective + 1)

# Backward selection using total costs as outcome
variable_name <- read_excel('R:/working/G78717-Cohort/documentation/G78711-cohort - variable_name - JAS.xlsx')

force <- variable_name %>% filter(force==1) %>% pull(`Variable name`)
keep <- variable_name %>% filter(keep==1 & force==0) %>% pull(`Variable name`)

force_variable <- force[!force %in% c('readmit','death','readmit_or_death','ps_weight')]
keep_variable <- keep

##########################################################
# Remove NAs
cohort_new <- cohort_add_one_cost %>% 
  dplyr::select(one_of(c(force_variable,keep_variable)),total_cost,ps_weight) %>%
  na.omit() 

##########################################################
# Fit global model
form_full <- formula(paste("log(total_cost)~", paste0(c(force_variable,keep_variable), collapse = "+" )))
full_model <- lm(form_full,data=cohort_new,weight=ps_weight)

# check VIF
library('MASS')
library('car')
library('corrplot')
vif_full_model <- round(vif(full_model),3) # Looks fine!

##########################################################
# Perform variable selection via backward elimination
# smallest possible model with those variable to be forced into the model
form_lower <- formula(paste("log(total_cost)~", paste0(force_variable,collapse = '+')))
lower_model <- lm(form_lower,data=cohort_new,weight=ps_weight)

backward <- stepAIC(object=full_model,scope=list(lower=lower_model),direction='backward',trace=0)
elim_vars <- names(coef(full_model))[!names(coef(full_model)) %in% names(coef(backward))]
save(backward,file='stepAIC_out_cost_analysis.RData')

##########################################################
# Load backward selection results
load('stepAIC_out_cost_analysis.RData')

summLM <- function(lmFit){
  coef <- summary(lmFit)$coef
  
  # robust SE
  cov_robust <- vcovHC(lmFit, type = "HC1")
  se_robust = sqrt(diag(cov_robust))
  
  ci <- cbind(coef[,1] - qnorm(0.975)*se_robust, coef[,1] + qnorm(0.975)*se_robust)
  
  data.frame(rownames(coef), coef, ci) %>% 
    as_tibble %>% 
    rename(coef = 1, est = 2, se = 3, z = 4, pvalue = 5, ciLow = 6, ciHigh = 7) %>% 
    mutate(coef = as.character(coef),
           CR = exp(est)) %>% 
    mutate(CR.ci = paste0(formatC(CR, digits = 2, format = "f"), " (",
                          formatC(exp(ciLow), digits = 2, format = "f"), ", ",
                          formatC(exp(ciHigh), digits = 2, format = "f"), ")")) %>%
    filter(coef=='G78717_feecode') %>%
    dplyr::select(CR.ci,pvalue) %>%
    mutate(pvalue=round(pvalue,3))
}

# Adjusted OR
hosp_readmit_urgent_cost <- lm(update(formula(backward),log(hosp_readmit_urgent_cost)~.),data=cohort_add_one_cost,weight=ps_weight)
hosp_readmit_elective_cost <- lm(update(formula(backward),log(hosp_readmit_elective_cost)~.),data=cohort_add_one_cost,weight=ps_weight)
physician_cost <- lm(update(formula(backward),log(physician_cost)~.),data=cohort_add_one_cost,weight=ps_weight)
ed_visits_cost <- lm(update(formula(backward),log(ed_vists_cost)~.),data=cohort_add_one_cost,weight=ps_weight)
total_cost <- lm(update(formula(backward),log(total_cost)~.),data=cohort_add_one_cost,weight=ps_weight)
total_cost_non_elective <- lm(update(formula(backward),log(total_cost_non_elective)~.),data=cohort_add_one_cost,weight=ps_weight)

tableF_adj <- rbind(summLM(hosp_readmit_urgent_cost),summLM(hosp_readmit_elective_cost),
                summLM(physician_cost),summLM(ed_visits_cost),
                summLM(total_cost),summLM(total_cost_non_elective))

# Unadjusted OR
hosp_readmit_urgent_cost <- lm(log(hosp_readmit_urgent_cost)~G78717_feecode,data=cohort_add_one_cost,weight=ps_weight)
hosp_readmit_elective_cost <- lm(log(hosp_readmit_elective_cost)~G78717_feecode,data=cohort_add_one_cost,weight=ps_weight)
physician_cost <- lm(log(physician_cost)~G78717_feecode,data=cohort_add_one_cost,weight=ps_weight)
ed_visits_cost <- lm(log(ed_vists_cost)~G78717_feecode,data=cohort_add_one_cost,weight=ps_weight)
total_cost <- lm(log(total_cost)~G78717_feecode,data=cohort_add_one_cost,weight=ps_weight)
total_cost_non_elective <- lm(log(total_cost_non_elective)~G78717_feecode,data=cohort_add_one_cost,weight=ps_weight)

tableF_unadj <- rbind(summLM(hosp_readmit_urgent_cost),summLM(hosp_readmit_elective_cost),
                    summLM(physician_cost),summLM(ed_visits_cost),
                    summLM(total_cost),summLM(total_cost_non_elective))



