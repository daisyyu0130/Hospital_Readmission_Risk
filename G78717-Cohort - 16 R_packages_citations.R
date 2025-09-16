#############################################
# Create 
# Author: Daisy Yu
# Date: 2023-09-14
# Updated: 2023-09-14
#############################################

mydir <- "R:/working/G78717-Cohort/results/"
setwd(mydir)
pkgs = c('tidyverse', 'dplyr', 'lubridate', 'PSweight','tableone','sandwich','janitor','data.table', 'readxl')
sink(paste0("G78717-Cohort - R package citations.txt"),
     append=T) 
pkgs %>% map(citation) %>% print(style='text') 
sink()


