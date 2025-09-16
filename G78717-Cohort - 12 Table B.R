#############################################
# Create Table B
# Author: Daisy Yu, JAS
# Date: 2023-05-04
# Updated: 2023-05-04
#############################################

# Libraries
pkgs = c('tidyverse', 'lubridate', 'data.table', 'dplyr','tableone','readxl','xlsx','fastDummies')
lapply(pkgs, library, character.only = TRUE)
setwd("R:/working/G78717-Cohort/")

# Read in the cohort data for Table B
cohort <- read.csv('R:/working/G78717-Cohort/data/merged data/cohort_for_modelling_2023-03-22.csv')

######################################
##          Create Table B          ##
######################################
tableD <- cohort %>% select(mrp_sex,mrp_spec,grad_plc,grad_exp,G78717_feecode)

# Vector of variables to summarize
myVars <- c('mrp_sex',"mrp_spec",'grad_plc','grad_exp')

# Vector of categorical variables
catVars <- myVars

# Group by exposure category (G78717_feecode)
tab1 <- CreateTableOne(vars = myVars, strata = 'G78717_feecode', data = tableD, factorVars = catVars)
tab1_csv <- print(tab1, showAllLevels = T, formatOptions = list(big.mark=','), smd=TRUE, printToggle = F)

# Save tables
write.xlsx(tab1_csv,file="R:/working/G78717-Cohort/results/G78717-Cohort - TableB v1.xlsx")

