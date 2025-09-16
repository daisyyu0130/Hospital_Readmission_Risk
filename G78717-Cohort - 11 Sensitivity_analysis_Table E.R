#############################################
# Sensitivity analysis
# Author: Daisy Yu
# Date: 2023-05-10
# Updated: 2023-05-11
#############################################

# Libraries
pkgs = c('tidyverse', 'lubridate', 'data.table','PSweight','dplyr','tableone','readxl','xlsx','fastDummies',
         'car', 'corrplot', 'irr', 'performance','lme4','sandwich')
lapply(pkgs, library, character.only = TRUE)
mydir <- "R:/working/G78717-Cohort/results/"
setwd(mydir)

# Read in the cohort data
cohort <- read.csv('R:/working/G78717-Cohort/data/merged data/cohort_for_modelling_2023-03-22.csv') %>%
  mutate(addate.dad = as.Date(addate.dad),
         sepdate.dad = as.Date(sepdate.dad),
         most_resp_serv = as.factor(most_resp_serv),
         surgcase2.dad = case_when(surgcase2.dad=='Y'~'Yes',TRUE~'No'))
hosp <- readRDS('R:/working/G78717-Cohort/data/merged data/dad.rds')


# Get the backward selection result
load('stepAIC_out.RData')

# Load function
summGLM <- function(glmFit){
  coef <- summary(glmFit)$coef
  
  # robust SE
  cov_robust <- vcovHC(glmFit, type = "HC1")
  se_robust = sqrt(diag(cov_robust))
  
  ci <- cbind(coef[,1] - qnorm(0.975)*se_robust, coef[,1] + qnorm(0.975)*se_robust)
  
  data.frame(rownames(coef), coef, ci) %>% 
    as_tibble %>% 
    rename(coef = 1, est = 2, se = 3, z = 4, pvalue = 5, ciLow = 6, ciHigh = 7) %>% 
    mutate(coef = as.character(coef),
           OR = exp(est)) %>% 
    mutate(OR.ci = paste0(formatC(OR, digits = 2, format = "f"), " (",
                          formatC(exp(ciLow), digits = 2, format = "f"), ", ",
                          formatC(exp(ciHigh), digits = 2, format = "f"), ")"))
}

SummTableE <- function(data,out_adj,out_unadj) {
  out <- cbind(
    # Case and Control
    data %>%
      mutate(exposure = case_when(G78717_feecode==0~"Control",G78717_feecode==1~"Case")) %>%
      group_by(exposure) %>% 
      count() %>%
      pivot_wider(names_from=exposure,values_from=n) %>%
      mutate(prop_case=paste0('(',round(100*Case/(data %>%
                                          mutate(exposure = case_when(G78717_feecode==0~"Control",G78717_feecode==1~"Case")) %>%
                                          group_by(exposure) %>% 
                                          count() %>%
                                          pivot_wider(names_from=exposure,values_from=n) %>% pull(Case)),1),')'),
             prop_control=paste0('(',round(100*Control/(data %>%
                                                          mutate(exposure = case_when(G78717_feecode==0~"Control",G78717_feecode==1~"Case")) %>%
                                                          group_by(exposure) %>% 
                                                          count() %>%
                                                          pivot_wider(names_from=exposure,values_from=n) %>% pull(Control)),1),')')),
    # Unadjusted and adjutsed OR
    as.tibble(cbind(summGLM(out_adj) %>% 
                      filter(coef=='G78717_feecode') %>%
                      select(OR.ci,pvalue) %>%
                      rename(OR.ci.adj=OR.ci,pvalue.adj=pvalue) %>% 
                      mutate(pvalue.adj=round(pvalue.adj,3)),
                    summGLM(out_unadj) %>% 
                      filter(coef=='G78717_feecode') %>%
                      select(OR.ci,pvalue) %>%
                      rename(OR.ci.unadj=OR.ci,pvalue.unadj=pvalue) %>%
                      mutate(pvalue.unadj=round(pvalue.unadj,3))))
  ) %>%
    unite(col='Case',c('Case','prop_case'),sep=' ') %>%
    unite(col='Control',c('Control','prop_control'),sep=' ') %>%
    unite(col='Unadjusted',c('OR.ci.unadj','pvalue.unadj'),sep=', ') %>%
    unite(col='Adjusted',c('OR.ci.adj','pvalue.adj'),sep=', ') %>%
    select(Case, Control, Unadjusted, Adjusted)
  
  return(out)
}

updateTableE <- function(summ,name) {
  tableE <- bind_rows(tableE, summ %>% mutate(Category=name) %>% relocate(Category)) %>%
    na.omit()
  return(tableE)
}
 
# Get different outcome interval
tableE <- data.frame(Category=NA,
                     Case=NA,
                     Control=NA,
                     Unadjusted=NA,
                     Adjusted=NA)
outcome_interval <- c(14,90,365)
for (i in 1:length(outcome_interval)) {
  day <- outcome_interval[i]
  
  # Get new outcome for the new interval
  cohort_new <- cohort %>% dplyr::select(studyid.dad, sepdate.dad) %>% 
    left_join(hosp %>% dplyr::select(STUDYID, ADDATE) %>% mutate(readmit_date=ADDATE),
              by = c("studyid.dad"='STUDYID')) %>% 
    filter(readmit_date >= sepdate.dad & readmit_date <= sepdate.dad + day) %>%
    group_by(studyid.dad, sepdate.dad) %>% 
    slice_min(readmit_date) %>%
    dplyr::select(-ADDATE) %>%
    unique() %>% 
    ungroup() %>% 
    rename(readmit_date_new=readmit_date) %>%
    right_join(cohort, by=c('studyid.dad','sepdate.dad')) %>%
    mutate(readmit_new = case_when(is.na(readmit_date_new) ~ 0, TRUE ~ 1),
           death_new = case_when(is.na(deathdt.vs) ~ 0,
                                    as.Date(deathdt.vs) <= sepdate.dad + day ~ 1,
                                    TRUE ~ 0),
           readmit_or_death_new = as.numeric(readmit_new|death_new)) %>%
    dplyr::select(-readmit_new,-death_new)
  
    # Perform the logistic regression
    form <- update(backward$formula, readmit_or_death_new~.)
    out_adj <- glm(form,data=cohort_new,family='binomial',weight=ps_weight)
    out_unadj <- glm(readmit_or_death_new~G78717_feecode,data=cohort_new,family='binomial',weight=ps_weight)
    
    # Append the output to the table
    tableE <- updateTableE(SummTableE(cohort_new %>% filter(readmit_or_death_new==1),out_adj,out_unadj),
                           paste0(as.character(day),' days'))
}

 
# Without PS weighting
out_adj <- glm(backward$formula,data=cohort,family='binomial')
out_unadj <- glm(readmit_or_death~G78717_feecode,data=cohort,family='binomial')
tableE <- updateTableE(SummTableE(cohort,out_adj,out_unadj),
                       'Cohort without PS weighting')


# Random select one eligible hospitalization from each patient
set.seed(1234) # set randome seed to generate the same results every time
cohort_one_per_patient <- cohort %>% 
  group_by(studyid.dad) %>% 
  slice_sample(n=1) %>% 
  ungroup() 
out_adj <- glm(backward$formula,data=cohort_one_per_patient,family='binomial',weight=ps_weight)
out_unadj <- glm(readmit_or_death~G78717_feecode,data=cohort_one_per_patient,family='binomial',weight=ps_weight)
tableE <- updateTableE(SummTableE(cohort_one_per_patient,out_adj,out_unadj),
                       'Random select one eligible hospitalization from each patient')

# Save file
write.xlsx(tableE, file='G78717-Cohort - TableE v1.xlsx', row.names = F)






