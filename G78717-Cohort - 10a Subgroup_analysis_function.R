#############################################
# Subgroup analysis function: Creating the function to complete subgroup analyses
# Author: Daisy Yu, JAS
# Date: 2023-04-28
# Updated: 2023-08-02
#############################################

# Libraries
pkgs = c('tidyverse', 'lubridate', 'data.table','PSweight','dplyr','tableone','readxl','xlsx','fastDummies',
         'car', 'corrplot', 'irr', 'performance','lme4')
lapply(pkgs, library, character.only = TRUE)
mydir <- "R:/working/G78717-Cohort/results"
setwd(mydir)
 
# Load the final model
load('stepAIC_out.RData')
var <- all.vars(backward$formula[-2]) 

subgrp_fxn = function(s, subgroup_data){
  fars.subgp <- subgroup_data %>% 
    rename(strata_name = which(names(subgroup_data)==s))
  
  # get the formula
  form <- formula(paste0('readmit_or_death~', paste0(var[!var %in% s],collapse = '+')))
  
  # for each category of strata_name, fit a regression (adjusted model)
  res <- fars.subgp %>%
    filter(!is.na(strata_name)) %>% 
    mutate(exposure = case_when(G78717_feecode==0~"Control",G78717_feecode==1~"Case")) %>%
    group_by(strata_name,exposure) %>%
    count() %>%
    pivot_wider(names_from=exposure,values_from=n) %>%
    left_join(.,
              fars.subgp %>% filter(!is.na(strata_name)) %>%
                nest(data = -strata_name) %>%
                mutate(
                  fit = map(data, ~ glm(form, data = .x,family='binomial',weight=ps_weight)),
                  tidied = map(fit,broom::tidy),
                  cov.robust = map(fit, ~ vcovHC(.x, type = "HC1")),
                  se.robust = map(cov.robust, ~ sqrt(diag(.x))[2])) %>%  
                select(-data) %>%
                unnest(tidied) %>%
                filter(term == 'G78717_feecode') %>%
                mutate(
                  se.robust = unlist(se.robust),
                  conf.low = exp(estimate-qnorm(0.975)*se.robust),
                  conf.high = exp(estimate+qnorm(0.975)*se.robust),
                  estimate = exp(estimate),
                  subgroup_name = s,
                  rtype = 'NBR',
                  adj = 'adjusted'),
              by = "strata_name") %>% 
    select(subgroup_name, strata_name, Case, Control, estimate, conf.low, conf.high, p.value, std.error, se.robust, rtype, adj)
}



# fars.subgp %>%
#   filter(!is.na(strata_name)) %>%
#   group_by(strata_name) %>%
#   nest() %>%
#   mutate(model_results = map(data, ~ glm(form, data = .x,family='binomial',weight=ps_weight)) %>%
#            map(broom::tidy, exponentiate=TRUE, conf.int=TRUE) %>%
#            map(select, c(estimate, conf.low, conf.high, p.value, std.error)) %>%
#            bind_rows(.id="term") %>%
#            mutate(subgroup_name = s,
#                   rtype = "NBR",
#                   adj = "adjusted")) %>%
#   select(-data) %>%
#   unnest(model_results)





