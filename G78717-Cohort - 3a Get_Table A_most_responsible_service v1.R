#############################################
# Get most responsible service
# Author: Daisy Yu
# Date: 2022-10-17
# Updated: 2022-10-27
#############################################

cohort <- read.csv("U:/Readmits/Data/Merged data/cohort_2022-10-13.csv")

# Most responsible service
# Create the freqency table
a <- cohort %>% group_by(dsc.dad) %>% summarise(n=n(), pct=round(100*n()/nrow(.),2)) 
b <- cohort %>% filter(feeitem.msp == 1) %>% group_by(dsc.dad) %>%
  summarise(with_feeitem=n(),pct_with_feeitem=round(100*n()/nrow(.),2))
c <- cohort %>% filter(feeitem.msp == 0) %>% group_by(dsc.dad) %>%
  summarise(without_feeitem=n(),pct_without_feeitem=round(100*n()/nrow(.),2))

# Read in crosswalk file
dsc_xw <- read_xlsx("R:/DATA/2018-06-20/docs/data_dictionary_discharge-abstracts-database.xlsx", sheet = "diagnosticshortcodes", skip=4) 
dsc_freq_tab <- dsc_xw %>% mutate(DSC_2005 = as.integer(DSC_2005)) %>%
  left_join(a, by=c('DSC_2005' = 'dsc.dad')) %>%
  left_join(b, by=c('DSC_2005' = 'dsc.dad')) %>%
  left_join(c, by=c('DSC_2005' = 'dsc.dad')) %>%
  mutate(n = replace_na(n,0),pct = replace_na(pct,0),
         with_feeitem = replace_na(with_feeitem,0),pct_with_feeitem = replace_na(pct_with_feeitem,0),
         without_feeitem = replace_na(without_feeitem,0),pct_without_feeitem = replace_na(pct_without_feeitem,0))

# Save file
write.xlsx(dsc_freq_tab,'U:/Readmits/results/G78717-Cohort - Tables v3_DY.xlsx', sheet='diagnostic_service_code', append = T)








