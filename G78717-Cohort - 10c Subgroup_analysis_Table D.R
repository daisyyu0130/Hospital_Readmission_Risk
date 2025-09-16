#############################################
# Subgroup analysis: Create Table D
# Author: Daisy Yu
# Date: 2023-05-03
# Updated: 2023-05-04
#############################################


# Libraries
pkgs = c('tidyverse', 'lubridate', 'data.table','PSweight','dplyr','tableone','readxl','xlsx','fastDummies',
         'car', 'corrplot', 'irr', 'performance','lme4','sandwich')
lapply(pkgs, library, character.only = TRUE)
mydir <- "R:/working/G78717-Cohort/results"
setwd(mydir)

# Read data
cohort <- read.csv('R:/working/G78717-Cohort/data/merged data/cohort_for_modelling_2023-03-22.csv')

cohort <- cohort %>%
  mutate(most_resp_serv = as.factor(most_resp_serv),
         surgcase2.dad = case_when(surgcase2.dad=='Y'~'Yes',TRUE~'No'),
         sex = na_if(sex,'U'),
         hhd_income = na_if(hhd_income,'Missing'),
         popl_dens = na_if(popl_dens,'Missing'),
         most_resp_serv = case_when(most_resp_serv=='Medicine'~'Medicine',
                                    most_resp_serv=='Psychiatry'~'Psychiatry',
                                    most_resp_serv=='Surgery'~'Surgery',
                                    TRUE~"Others"),
         num_hosp_prev_year = case_when(num_hosp_prev_year<=1~'<=1',num_hosp_prev_year>=2~'>=2'),
         cci = as.factor(cci),
         icu_stay = as.factor(icu_stay),
         fiscal_year = as.factor(fiscal_year),
         hosp_size = case_when(hosp_size=='Community-Large' | hosp_size=='Community-Medium/small'~'Community',
                               hosp_size=='Teaching'~'Academic'),
         hosp_size = na_if(hosp_size,'Missing'),
         mrp_sex = na_if(mrp_sex,'Missing'),
         grad_exp = na_if(grad_exp,'Missing'),
         grad_plc = na_if(grad_plc,'Missing'))

# Read subgroup analysis results
results.subgroups <- read.csv("R:/working/G78717-Cohort/results/G78717-Cohort - results.subgroups - 2023-09-18.csv")
results.subgroups <- rapply(object=results.subgroups,f=round,classes='numeric',how='replace',digits=3)

## Vector of subgroups of interest for use 
subgroups <- c("sex","age_cat","hhd_income","popl_dens","most_resp_serv",
               "num_hosp_prev_year","cci","icu_stay","los","ambulanc","fiscal_year",
               'hosp_size',"mrp_spec","mrp_sex","grad_exp","grad_plc")

# Create unadjusted results
form <- formula("readmit_or_death ~ G78717_feecode")
subgrp_fxn_unadj = function(s, subgroup_data){
  fars.subgp <- subgroup_data %>% 
    rename(strata_name = which(names(subgroup_data)==s))
  
  # for each category of strata_name, fit a regression (adjusted OR)
  res <- fars.subgp %>%
    filter(!is.na(strata_name)) %>% 
    nest(data = -strata_name) %>%
    mutate(
      fit = map(data, ~ glm(form, data = .x,family='binomial',weight=ps_weight)),
      tidied = map(fit,broom::tidy),
      cov_robust = map(fit, ~ vcovHC(.x, type = "HC1")),
      se_robust = map(cov_robust, ~ sqrt(diag(.x))[2])) %>%  
    select(-data) %>%
    unnest(tidied) %>%
    filter(term == 'G78717_feecode') %>%
    mutate(
      se_robust = unlist(se_robust),
      conf.low.unadj = exp(estimate-qnorm(0.975)*se_robust),
      conf.high.unadj = exp(estimate+qnorm(0.975)*se_robust),
      estimate.unadj = exp(estimate),
      p.value.unadj = p.value,
      subgroup_name = s) %>%
    select(subgroup_name, strata_name, estimate.unadj, conf.low.unadj, conf.high.unadj, p.value.unadj)
}


## Map the subgroup function and save results  
results.subgroups.unadj <- map_df(subgroups, ~ subgrp_fxn_unadj(.x, subgroup_data = cohort))
results.subgroups.unadj <- rapply(object=results.subgroups.unadj,f=round,classes='numeric',how='replace',digits=3)

tableD <- results.subgroups %>%
  left_join(results.subgroups.unadj, by=c('subgroup_name',"strata_name")) %>%
  group_by(subgroup_name) %>% 
  mutate(prop_case=paste0('(',round(prop.table(Case)*100,1),')'),
         prop_control=paste0('(',round(prop.table(Control)*100,1),')')) %>%
  ungroup() %>%
  mutate(conf.low.unadj = paste0('(',conf.low.unadj,','),
         conf.high.unadj = paste0(conf.high.unadj,')'),
         conf.low = paste0('(',conf.low,','),
         conf.high = paste0(conf.high,')')
         ) %>%
  unite(col='Case',c('Case','prop_case'), sep=' ') %>%
  unite(col='Control',c('Control','prop_control'), sep=' ') %>%
  unite(col='CI_unadjusted',c('conf.low.unadj','conf.high.unadj'), sep='') %>%
  unite(col='CI_adjusted',c('conf.low','conf.high'), sep='') %>%
  unite(col='unadjusted',c('estimate.unadj','CI_unadjusted','p.value.unadj'), sep=', ') %>%
  unite(col='adjusted',c('estimate','CI_adjusted','p.value'), sep=', ') %>%
  select(subgroup_name, strata_name, Case, Control, unadjusted, adjusted)


# Save file
write.xlsx(tableD, file='G78717-Cohort - TableD v1.xlsx', row.names = F)

