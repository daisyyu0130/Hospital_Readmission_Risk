#############################################
# Subgroup analysis
# Author: Daisy Yu, JAS
# Date: 2023-05-02
# Updated: 2023-08-02
#############################################

# Libraries
pkgs = c('tidyverse', 'lubridate', 'data.table','PSweight','dplyr','tableone','readxl','xlsx','fastDummies',
         'car', 'corrplot', 'irr', 'performance','lme4','sandwich')
lapply(pkgs, library, character.only = TRUE)
mydir <- "R:/working/G78717-Cohort/results"
setwd(mydir)

# Load function to complete subgroup analyses
source("R:/working/G78717-Cohort/code/G78717-Cohort - 10a Subgroup_analysis_function.R") 

# hosp_size: combine Community-Large and Community-Medium/small, and Teaching


###############################################################################
# IMPORT DATA
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


###############################################################################
# Complete the subgroup analyses using the "subgrp_fxn" function that was created in "Staples Lab - Subgroup analysis function v1.R"

## Vector of subgroups of interest for use 
subgroups <- c("sex","age_cat","hhd_income","popl_dens","most_resp_serv",
               "num_hosp_prev_year","cci","icu_stay","los","ambulanc","fiscal_year",
               "hosp_size","mrp_sex","grad_exp","grad_plc") # create vector of subgroups

## Map the subgroup function and save results  
results.subgroups <- map_df(subgroups, ~ subgrp_fxn(.x, subgroup_data = cohort))


###############################################################################
# Export subgroup results
#export(results.subgroups, paste0("G78717-Cohort - results.subgroups - ", today(), ".csv"))
write.csv(results.subgroups,file=paste0("G78717-Cohort - results.subgroups - ", today(), ".csv"),row.names = F)




