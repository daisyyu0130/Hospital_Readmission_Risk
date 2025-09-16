###########################################################
# Data linkage: DAD, Census, demographics, MSP-Practitioner, Registry and Deaths
# Create Table A Cohort
# Author: Daisy Yu 
# Date: 2022-08-25
# Updated: 2022-11-18
##########################################################

# Libraries
pkgs = c('tidyverse', 'lubridate', 'data.table', 'dplyr', 'readxl', 'janitor')
lapply(pkgs, library, character.only = TRUE)
setwd("R:/working/G78717-Cohort/data/merged data/")


###############################################################
# Load DAD-admits all 
###############################################################
hosp <- read.csv("R:/working/G78717-Cohort/data/Raw data/dad_admits_all.csv") %>% 
  as_tibble() %>%
  clean_names() %>%
  mutate(sepdate = ymd(sepdate), 
         addate = ymd(addate),
         tdays = as.numeric(tdays)) %>%
  # Remove the variables we most likely won't need
  select(-ver_num, -seqno, -version) 
  
df_dad <- hosp
rm(hosp)

# Add dad suffix to all colnames
colnames(df_dad) <- paste0(colnames(df_dad), ".dad")

# Create new variable for length of stay (los) because TDAYS=1 for many dates with same admission and discharge dates
df_dad <- df_dad %>%
  mutate(tlos = as.period(addate.dad %--% sepdate.dad),
         los = as.duration(tlos) / ddays(1))

# Check number of rows and number of individuals
nrow(df_dad)
length(unique(df_dad$studyid.dad))


#############################################################
# Filter on time interval: 2012-06-01 to 2017-01-31
#############################################################
dad_coh <- df_dad %>% filter(sepdate.dad >= '2012-06-01' & sepdate.dad <= '2017-01-31')

# Check number of rows and number of individuals
nrow(dad_coh)
length(unique(dad_coh$studyid.dad))

# Save file 
write.csv(dad_coh, "dad_coh.csv", row.names = FALSE)


#############################################################
# Merge with demographics data 
#############################################################
demo <- read.csv("U:/Readmits/Data/Raw data/demo_all.csv") %>% 
  as_tibble() %>%
  clean_names() %>% # rename_all(tolower) %>%
  # Combine year, month of birth into one variable
  mutate(birthdt = as.Date(with(., ym(paste(dobyyyy, dobmm, sep="-"))),"%Y%m")) %>% 
  # Remove the variables we most likely won't need
  select(-dobyyyy, -dobmm, -version, -seqno, -source)

# Left-join dad_coh to dem -> dad_demo
dad_demo <- dad_coh %>%
  left_join(demo, by = c("studyid.dad" = "studyid")) %>% 
  mutate(birthdt = as.Date(birthdt, format="%Y%m%d"), 
         sepdate.dad = as.Date(sepdate.dad, format="%Y%m%d"))

# Create variable for indexyear, convert to integer (census year is integer) 
# Create age variable 
dad_demo <- dad_demo %>% 
  mutate(indexyr = as.integer(format(as.Date(sepdate.dad, format="%Y-%m-%d"), "%Y")),
         age = (as.period(interval(birthdt, sepdate.dad)))@year) 

# Check NAs
dad_demo %>% select(studyid.dad, birthdt, sex, addate.dad, sepdate.dad, sepdisp.dad) %>%
  filter(is.na(sepdate.dad))
dad_demo <- dad_demo %>%
  filter(!is.na(sepdate.dad))

# Check number of rows and number of individuals
nrow(dad_demo)
length(unique(dad_demo$studyid.dad))


# Get the frequency of each physician specialty code
spec_fre_tbl <- dad_coh %>% group_by(doc_spec.dad) %>% summarise(n=n())
# Link it to both crosswalk file and data dictionary
crosswalk_file <- read.csv("R:/working/J Staples working 2019-07-09/Eligible for G78717 based on DOC_SPEC in DAD.csv")
crosswalk_file <- crosswalk_file %>% rename(specialty_in_crosswalk=specialty)
spec_fre_tbl <- spec_fre_tbl %>% left_join(crosswalk_file, by=c('doc_spec.dad' = 'code'))
data_dict <- read_excel('R:/DATA/2018-06-20/docs/data_dictionary_discharge-abstracts-database.xlsx',sheet='providerservicecodes')
data_dict <- data_dict %>% filter(`...4`=='Codes in use in 2001/02') %>%
  rename(Provider_Service_Codes=`Provider Service Codes`, specialty_in_data_dictionary=`...2`) %>%
  mutate(Provider_Service_Codes=as.integer(Provider_Service_Codes)) %>% select(-`...3`,-`...4`)
spec_fre_tbl <- spec_fre_tbl %>% left_join(data_dict, by=c('doc_spec.dad' = 'Provider_Service_Codes'))
# Save the frequency table
write.csv(spec_fre_tbl, "spec_freq_tbl.csv", row.names = FALSE)


# For flow chart numbers:
# Exclude MRP other than pysician specialist
dad_demo %>% filter(!doc_spec.dad %in% crosswalk_file$code) %>% nrow()
# Exclude non-elective admission
dad_demo %>% filter(!admit.dad == 'U') %>% nrow()
# Exclude non-acute care
dad_demo %>% filter(!level.dad == 'A') %>% nrow()
# Exclude length of stay < 5 days
dad_demo %>% filter(los < 5) %>% nrow()
# Exclude pregnancy-related visits
dad_demo %>% filter((str_detect(diagx1.dad, 'O|P'))) %>% nrow()
# Exclude admission categories N('Newborn') and S('Stillborn')
dad_demo %>% filter(sepdisp.dad %in% c(9)) %>% nrow
# Exclude hospitalizations ended in death or discharge to AMA
dad_demo %>% filter(sepdisp.dad %in% c(6, 7, 8, 12)) %>% nrow()
# Exclude hospitalizations ended in discharge to long-term care or other facilities
dad_demo %>% filter(sepdisp.dad %in% c(1, 2, 3)) %>% nrow()
# Exclude hospitalizations that were transferred from/to another hospital
dad_demo %>% filter(!(hospfrom.dad == 6 | hospfrom.dad == 7 | is.na(hospfrom.dad))) %>%
  filter(!(hospto.dad == 6 | hospto.dad == 7 | is.na(hospto.dad))) %>% nrow()
# Exclude age < 18
dad_demo %>% filter(age < 18) %>% nrow()


#############################################################
# Apply exclusion criteria
#############################################################
dad_demo <- dad_demo %>% 
  # Filter MRP with eligible specialty in Item SB2
  filter(doc_spec.dad %in% crosswalk_file$code) %>%
  # Keep non-elective admission 
  filter(admit.dad == 'U') %>%
  # Keep acute care
  filter(level.dad == 'A') %>%
  # Exclude length of stay < 5 days
  filter(!los < 5) %>%
  # exclude pregnancy-related visits from primary diagnostic code
  filter(!(str_detect(diagx1.dad, 'O|P'))) %>%
  # exclude admission categories N('Newborn') and S('Stillborn'), ended in death or discharge to AMA or
  # ended in discharge to long-term care or other facilities
  filter(sepdisp.dad %in% c(4,5)) %>%
  # exclude hospitalizations that were transferred from/to another hospital
  # We keep: blank (not a transfer)
  filter(hospfrom.dad == 6 | hospfrom.dad == 7 | is.na(hospfrom.dad)) %>%
  filter(hospto.dad == 6 | hospto.dad == 7 | is.na(hospto.dad)) %>%
  # Exclude age < 18
  filter(!age < 18) 

# Create a frequency table of sepdisp and los after filter
dad_demo %>% select(sepdisp.dad) %>% group_by(sepdisp.dad) %>% dplyr::summarise(n=n())
dad_demo %>% select(los) %>% group_by(los) %>% dplyr::summarise(n=n())

# Check NAs  
dad_demo %>% filter(is.na(sepdate.dad))
dad_demo %>% filter(is.na(los))

# Check number of rows and number of patients
nrow(dad_demo)
length(unique(dad_demo$studyid.dad))

# Save file
write.csv(dad_demo, "dad_demo.csv", row.names = FALSE)

################################################################
## Merge with Census data (SES, Residential urbanicity)
###############################################################
# Load census data and rename var names
cen <- read.csv("U:/Readmits/Data/Raw data/census_all.csv") %>%
  as.tibble() %>%
  clean_names() %>%
  select(-version, -seqno)

# Add .df suffix to all vars
colnames(cen) <-  paste0(colnames(cen), ".cen")

# Left_join dad_demo with census based on studyid and index visit year, ensure # of rows remains same.
dad_dc <- dad_demo %>%
  left_join(cen, by = c("studyid.dad" = "studyid.cen",
                        "indexyr" = "veryear.cen")) 

# Check number of rows and number of patients
nrow(dad_dc)
length(unique(dad_dc$studyid.dad))

# Save file 
write.csv(dad_dc, "dad_dc.csv", row.names = FALSE)


################################################################
## Merge with MSP-Practitioner
###############################################################
msp.p <- read.csv("U:/Readmits/Data/Raw data/msp_p_all.csv") %>%
  as.tibble %>%
  clean_names() %>%
  rename(physid = pracnumencrypt,
         dobyy  = birthdtyyyy) %>%
  mutate(physid = as.integer(physid),
         datayr = as.integer(datayr)) %>%
  select(-version, -seqno) 

# Add .df suffix to all vars
colnames(msp.p) <-  paste0(colnames(msp.p), ".mrp")

# left_join (without using indexyr as a joining variable leads to 7mil rows)
dad_dcp <- dad_dc %>%
  left_join(msp.p, by=c("respphys.dad" = "physid.mrp",
                        "indexyr" = "datayr.mrp"))

# Check the number of practitioner
length(unique(dad_dcp$respphys.dad))

# Check number of rows and number of patients
nrow(dad_dcp)
length(unique(dad_dcp$studyid.dad))

# Save file 
write.csv(dad_dcp, "dad_dcp.csv", row.names = FALSE)


#######################################################
# Merge with Registry
######################################################
reg <- read.csv("U:/Readmits/Data/Raw data/registry_all.csv") %>%
  as_tibble() %>%
  clean_names() %>%
  select(-version, -seqno, -daysreg) 

# Add .df suffix to all vars
colnames(reg) <-  paste0(colnames(reg), ".reg")

# Left join 
dad_dcpr <- dad_dcp %>%
  left_join(reg, by = c("studyid.dad" = "studyid.reg",
                        "indexyr" = "year.reg")) 

# Check number of rows and number of patients
nrow(dad_dcpr)
length(unique(dad_dcpr$studyid.dad))

# Save file 
write.csv(dad_dcpr, "dad_dcpr.csv", row.names = FALSE)


################################################################
## Merge with Deaths  
###############################################################
deaths <- read.csv("U:/Readmits/Data/Raw data/deaths_all.csv") %>%
  as_tibble() %>%
  clean_names() %>%
  # Combine year, mon, day of death into one variable
  mutate(deathdt = as.Date(with(., ymd(paste(deathdtccyy, deathdtmm, 
                                             deathdtdd, sep="-"))),"%Y%m%d")) %>% 
  # remove the variables we most likely won't need
  select(-deathdtccyy, -deathdtmm, -deathdtdd, -version, -seqno)

deaths %>% sample_n(5)
deaths %>% group_by(native) %>% dplyr::summarise(n = n())

# Add .df suffix to all vars
colnames(deaths) <-  paste0(colnames(deaths), ".vs")

deaths %>% group_by(datayr.vs) %>% dplyr::summarise(n= n())
deaths %>% 
  group_by(studyid.vs) %>% 
  #filter(is.na(deathdt.vs)) %>% 
  dplyr::summarise(n= n()) %>% arrange(desc(n))

# there are 36 duplicate rows: 4 are just repeats; rest have different death date for each row. 
dup <- deaths %>% select(-datayr.vs) %>%
  group_by(studyid.vs) %>% add_count() %>% arrange(desc(n)) 

## TEST
# check ids from dup with ids from cohort
dad_dcpr %>% filter(studyid.dad == '') %>% select(sepdate.dad, addate.dad, studyid.dad)
# Looks like sepdate is either before the last deathdate or before both. Maybe makes sense to only include the latest date.
# First, remove the duplicates with different deathdate (keep the latest one): this removed 14 rows as expected
# Then remove the duplicates that have same deathdate (keeping the first one): this removed 4 as expected
# The opposite order of removal will not be correct as it won't discriminate between the deathdates.
# dup2 <- dup %>% group_by(studyid.vs) %>% slice_max(deathdt.vs)
# dup2 <- dup %>% group_by(studyid.vs) %>% slice_head(n=1)

# Apply to the deaths dataset before merging + check row count.
deaths <- deaths %>% group_by(studyid.vs) %>% slice_max(deathdt.vs)
deaths <- deaths %>% group_by(studyid.vs) %>% slice_head(n=1)

# Left_join based on studyid since VS would only include rows if someone dies.
dad_dcprd <- dad_dcpr %>%
  left_join((deaths %>% select(-datayr.vs)), by = c("studyid.dad" = "studyid.vs"))

dad_dcprd <- dad_dcprd %>% mutate(indexdate = as.Date(sepdate.dad))
deaths %>% group_by(datayr.vs) %>% dplyr::summarise(n= n())

# Check number of rows and number of patients
nrow(dad_dcprd)
length(unique(dad_dcprd$studyid.dad))

# Save file 
write.csv(dad_dcprd, "dad_dcprd.csv", row.names = FALSE)


#######################################################
# Extract the studyIDs
#######################################################
cohort <- dad_dcprd
cohortID <- cohort %>% select(studyid.dad, indexyr, sex, age, sepdate.dad, sepdisp.dad, respphys.dad)
# Save file
write.csv(cohortID, "cohortID_2022-11-18.csv", row.names = FALSE)

################################################################
## Merge MSP data 
################################################################
# Use the helper file to link MSP data year by year (data has been saved, so don't need to run again)
#source('U:/Readmits/My code/Readmits - 2b Data linkage_msp.R')

f <- function(file_name) {
  path <- paste0(file_name,".csv")
  cm <- read.csv(path)
  # Filter out rows with fee code 78717
  cm_78717 <- cm %>% filter(feeitem.msp==78717) %>% 
    select(studyid.dad,sepdate.dad,feeitem.msp) %>%
    mutate(sepdate.dad = as.Date(sepdate.dad)) 
  # There are ties, so always choose the first one
  cm_78717 <- cm_78717 %>% group_by(studyid.dad,sepdate.dad) %>% slice_head(n=1)
  return(cm_78717)
}

cm2012 <- f('coh_msp2012')
cm2013 <- f('coh_msp2013')
cm2014 <- f('coh_msp2014')
cm2015 <- f('coh_msp2015')
cm2016 <- f('coh_msp2016')

# Combine 2012-2016 together: so this file will contain all the hospitalizations with the fee code
cm2012_2016 <- rbind(cm2012,cm2013,cm2014,cm2015,cm2016) 

# Left join to dad_dcprd
cohort <- cohort %>%
  left_join(cm2012_2016, by=c("studyid.dad" = "studyid.dad","sepdate.dad" = "sepdate.dad")) %>%
  mutate(G78717_feecode = case_when(feeitem.msp == 78717 ~ 1, TRUE ~ 0))

write.csv(cohort, "cohort_2022-11-18.csv",row.names = FALSE)
 
