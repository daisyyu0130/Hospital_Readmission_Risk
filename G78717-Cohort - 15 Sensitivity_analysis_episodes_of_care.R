#############################################
# Additional Sensitivity analysis: Link episode of cares
# Author: Daisy Yu
# Date: 2023-08-24
# Updated: 2023-08-24
#############################################

# Libraries
pkgs = c('tidyverse', 'lubridate', 'data.table', 'dplyr', 'readxl', 'janitor')
lapply(pkgs, library, character.only = TRUE)
mydir <- "R:/working/G78717-Cohort/results/"
setwd(mydir)

# Read in the cohort data
dad_coh <- read.csv("R:/working/G78717-Cohort/data/merged data/dad_coh.csv") %>%
  rename(STUDYID = studyid.dad,
         ADDATE = addate.dad,
         SEPDATE = sepdate.dad,
         SEPDISP = sepdisp.dad,
         `HOSPTO*` = hospto.dad,
         `HOSPFROM*` = hospfrom.dad) %>%
  mutate(SEPDISP = as.character(SEPDISP)) %>%
  distinct()

# exclude pregnancy-related visits from primary diagnostic code
dad_coh <- dad_coh %>% filter(!(str_detect(diagx1.dad, 'O|P')))

# To solve the issue of duplicate records
# *duplicate records* are defined as those rows with the same addate but different sepdate OR the same sepdate but different addate

# STEP1: exclude elective admission and los < 5
dad_coh1 <- dad_coh %>% filter(!los < 5) %>% filter(admit.dad == 'U')

# STEP2: for rows with the same STUDYID, ADDATE, SEPDATE and los, randomly select one of them
dad_coh2 <- dad_coh1 %>% 
  group_by(STUDYID, ADDATE, SEPDATE,los) %>%
  slice_head(n=1) %>%
  ungroup()
  
# STEP3: for duplicate rows, select the one with largest los
dad_coh3 <- dad_coh2 %>% 
  group_by(STUDYID, ADDATE) %>%
  filter(los == max(los)) %>%
  ungroup() %>%
  group_by(STUDYID, SEPDATE) %>%
  filter(los == max(los)) %>%
  ungroup()

# TEST: GOOD! Not duplicates
dad_coh3 %>% group_by(STUDYID, SEPDATE) %>% count() %>% filter(n>1) %>% nrow() 
dad_coh3 %>% group_by(STUDYID, ADDATE) %>% count() %>% filter(n>1) %>% nrow() 



########################################
## Link hospitalizations into episodes
########################################
source("R:/working/G78717-Cohort/code/StaplesLab - Link episodes v5.R")
dad_coh <- create_episodes(dad_coh, episodes_of_care = "DT_1day_transfer") %>%
  rename(studyid.dad = STUDYID,
         addate.dad = ADDATE,
         sepdate.dad = SEPDATE,
         sepdisp.dad = SEPDISP,
         hospto.dad = `HOSPTO*`,
         hospfrom.dad = `HOSPFROM*`) 

# Save file
write.csv(dad_coh,file="R:/working/G78717-Cohort/data/merged data/data_episode_of_care/dad_coh.csv",
          row.names = FALSE)

# View(dad_coh %>% group_by(episode_of_care) %>% count())
# 
# dad_coh_num <- dad_coh %>%
#   group_by(episode_of_care) %>%
#   arrange(desc(sepdate.dad)) %>%
#   mutate(last = row_number() == 1,
#          num_hosp = n(),
#          single_hosp = num_hosp == 1) %>%
#   ungroup()


#############################################################
## Merge with demographics data 
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

crosswalk_file <- read.csv("R:/working/J Staples working 2019-07-09/Eligible for G78717 based on DOC_SPEC in DAD.csv") %>%
  rename(specialty_in_crosswalk=specialty)


#############################################################
## Apply exclusion criteria
#############################################################
dad_demo <- dad_demo %>%
  group_by(episode_of_care) %>% 
  arrange(sepdate.dad) %>%
  mutate(
    sepdate.dad_new = max(sepdate.dad),
    addate.dad_new = min(addate.dad),
    sepdisp.dad_new = last(sepdisp.dad),
    age_new = max(age),
    los_new = sum(los),
    admit.dad_new = case_when(any(admit.dad == 'U')~'U',TRUE~'Others'),
    level.dad_new = case_when(any(level.dad == 'A')~'A',TRUE~'Others'),
    doc_spec.dad_new = last(doc_spec.dad)) %>%
  ungroup() %>%
  # Filter MRP with eligible specialty in Item SB2
  filter(doc_spec.dad_new %in% crosswalk_file$code) %>%
  # Keep non-elective admission 
  filter(admit.dad_new == 'U') %>%
  # Keep acute care
  filter(level.dad_new == 'A') %>%
  # Exclude length of stay < 5 days
  filter(!los_new < 5) %>%
  # exclude admission categories N('Newborn') and S('Stillborn'), ended in death or discharge to AMA or
  # ended in discharge to long-term care or other facilities
  filter(sepdisp.dad_new %in% c(4,5)) %>%
  # Exclude age < 18
  filter(!age_new < 18) 
    
# Save file
write.csv(dad_demo,file="R:/working/G78717-Cohort/data/merged data/data_episode_of_care/dad_demo.csv",
          row.names = FALSE)
    
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
    
# Save file 
write.csv(dad_dc, file="R:/working/G78717-Cohort/data/merged data/data_episode_of_care/dad_dc.csv", 
          row.names = FALSE)
    
    
################################################################
## Merge with MSP-Practitioner
################################################################
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
    
# Save file 
write.csv(dad_dcp, file="R:/working/G78717-Cohort/data/merged data/data_episode_of_care/dad_dcp.csv", 
          row.names = FALSE)
    
    
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
    
# Save file 
write.csv(dad_dcpr, file="R:/working/G78717-Cohort/data/merged data/data_episode_of_care/dad_dcpr.csv", 
          row.names = FALSE)
    
    
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

# Add .df suffix to all vars
colnames(deaths) <-  paste0(colnames(deaths), ".vs")

# Apply to the deaths dataset before merging + check row count.
deaths <- deaths %>% group_by(studyid.vs) %>% slice_max(deathdt.vs)
deaths <- deaths %>% group_by(studyid.vs) %>% slice_head(n=1)

# Left_join based on studyid since VS would only include rows if someone dies.
dad_dcprd <- dad_dcpr %>%
  left_join((deaths %>% select(-datayr.vs)), by = c("studyid.dad" = "studyid.vs")) %>%
  mutate(indexdate = as.Date(sepdate.dad))

# Save file 
write.csv(dad_dcprd, file="R:/working/G78717-Cohort/data/merged data/data_episode_of_care/dad_dcprd.csv", 
          row.names = FALSE)
    
    
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
    
cm2012 <- f('R:/working/G78717-Cohort/data/merged data/coh_msp2012')
cm2013 <- f('R:/working/G78717-Cohort/data/merged data/coh_msp2013')
cm2014 <- f('R:/working/G78717-Cohort/data/merged data/coh_msp2014')
cm2015 <- f('R:/working/G78717-Cohort/data/merged data/coh_msp2015')
cm2016 <- f('R:/working/G78717-Cohort/data/merged data/coh_msp2016')
    
# Combine 2012-2016 together: so this file will contain all the hospitalizations with the fee code
cm2012_2016 <- rbind(cm2012,cm2013,cm2014,cm2015,cm2016) 
    
# Left join to dad_dcprd
cohort <- dad_dcprd %>%
  left_join(cm2012_2016, by=c("studyid.dad" = "studyid.dad","sepdate.dad" = "sepdate.dad")) %>%
  mutate(G78717_feecode = case_when(feeitem.msp == 78717 ~ 1, TRUE ~ 0))
    
write.csv(cohort, 
          file="R:/working/G78717-Cohort/data/merged data/data_episode_of_care/cohort_2023-09-06.csv",
          row.names = FALSE)


#######################################################
## Get patient/hosptial-level characteristic variables
######################################################
cohort <- read.csv("R:/working/G78717-Cohort/data/merged data/data_episode_of_care/cohort_2023-09-06.csv") %>%
  mutate(addate.dad = as.Date(addate.dad),
         sepdate.dad = as.Date(sepdate.dad))

# Patient characteristics  
fsa <- read.csv("R:/working/G78717-Cohort/data/Crosswalks/fsa_2016_urbanRural.csv")
patient_char <- cohort %>% 
  left_join(fsa,by=c('pc3.reg'='fsa')) %>%
  mutate(age_cat = case_when(age >= 18 & age <= 49~'18-49 yrs',
                             age >= 50 & age <= 64~'50-64 yrs',
                             age >= 65~'>= 65 yrs',
                             is.na(age)~'Missing'),
         qaippe = case_when(qaippe.cen==1~'1st',
                            qaippe.cen==2~'2nd',
                            qaippe.cen==3~'3rd',
                            qaippe.cen==4~'4th',
                            qaippe.cen==5~'5th',
                            qaippe.cen==9 | is.na(qaippe.cen)~'Missing'),
         fsaType =case_when(fsaType=='popCentreMedium'~'popCentre',
                            fsaType=='popCentreSmall'~'popCentre',
                            fsaType=='ruralArea'~'ruralArea',
                            is.na(fsaType)~'Missing')) %>%
  select(studyid.dad,addate.dad,sepdate.dad,age,age_new,age_cat,sex,qaippe,fsaType,G78717_feecode)

# Details of index hospitalization
hosp_size_xw <- read.table("R:/working/G78717-Cohort/data/crosswalks/G78717-Readmits-Hospital_size-FINAL_v2_encrypted.dat");colnames(hosp_size_xw) <- c('Code',"Category")
md_xw <- read_excel("R:/working/G78717-Cohort/data/crosswalks/G78717-Cohort - MD specialty categorized by PATSERV in DAD v7.xlsx")
hosp_idx_char <- cohort %>% left_join(hosp_size_xw, by=c('hosp.dad'='Code')) %>% 
  left_join(md_xw %>% select(Code, MD_category), by=c('patserv.dad' = 'Code')) %>%
  mutate(fiscal_year = as.integer(str_sub(lubridate::quarter(sepdate.dad, with_year=T,fiscal_start=4),1,4)),
         ambulanc = case_when(ambulanc.dad %in% c("A","G","W","C") ~ '1.Yes',
                              ambulanc.dad == 'N' | is.na(ambulanc.dad) ~ '2.No'),
         hosp_size = case_when(Category == 'Community-Large' ~ '1.Community-Large',
                               Category %in% c('Community-Medium','Likely-Community-Medium ') ~ '2.Community-Medium',
                               Category %in% c('Community-Small','Likely-Community-Small ') ~ '3.Community-Small',
                               Category == 'Teaching' ~ '4.Teaching',
                               Category == 'NotApplicable' ~ '5. NotApplicable',
                               is.na(Category) ~ '6.Missing'),
         icu_stay = ifelse(icudays.dad > 0, 1, 0)
  ) %>%
  select(studyid.dad,addate.dad,sepdate.dad,fiscal_year,ambulanc,hosp_size,MD_category,icu_stay,G78717_feecode)


cohort <- cohort %>% 
  left_join(patient_char, by=c('studyid.dad','addate.dad','sepdate.dad','G78717_feecode','age','age_new','sex')) %>%
  left_join(hosp_idx_char, by=c('studyid.dad', 'addate.dad','sepdate.dad','G78717_feecode')) %>%
  group_by(respphys.dad) %>%
  fill(sex.mrp, .direction='downup') %>%
  fill(gradplc.mrp, .direction='downup') %>%
  fill(gradyr.mrp, .direction='downup') %>%
  ungroup() %>%
  mutate(most_resp_serv = MD_category,
         hhd_income = qaippe,
         popl_dens = fsaType,
         grad_exp = case_when(gradyr.mrp == 0 | is.na(gradyr.mrp) ~ 'Missing',
                              year(addate.dad) - gradyr.mrp <= 14 ~ '<= 14',
                              year(addate.dad) - gradyr.mrp >= 15 ~ ">= 15"),
         grad_plc = case_when(gradplc.mrp %in% c(0,00) | is.na(gradplc.mrp) ~ "Missing",
                              gradplc.mrp %in% c(16) ~ "UBC",
                              gradplc.mrp %in% c(01:15,17:29) ~ "CanExUBC",
                              gradplc.mrp %in% c(30:99) ~ "Foreign"),
         mrp_spec = case_when(spec1.mrp == 00 ~ "General practice",
                              spec1.mrp == 26 ~ "Cardiology",
                              spec1.mrp == 08 ~ "General surgery",
                              spec1.mrp %in% c(5,80)  ~ "Gynecology",
                              spec1.mrp == 15 ~ "Internal medicine",
                              spec1.mrp %in% c(NA, 55, 36) ~ "Missing",
                              spec1.mrp == 10 ~ "Orthopedic surgery",
                              spec1.mrp == 47 ~ "Vascular surgery",
                              spec1.mrp == 03 ~ "Psychiatry",
                              spec1.mrp == 49 ~ "Respirology",
                              spec1.mrp == 13 ~ "Urology",
                              spec1.mrp %in% c(56,59,24,67,53,74,44,51,29,45,14,19) ~ "Other medical",
                              spec1.mrp %in% c(9,7,48,11,12,6,37,40) ~ "Other surgery",
                              TRUE ~ "Other"),
         hosp_size = case_when(hosp_size == "5. NotApplicable" | is.na(hosp_size) ~ 'Missing',
                               hosp_size == "1.Community-Large" ~ "Community-Large",
                               hosp_size == "2.Community-Medium" | hosp_size == "3.Community-Small" ~ "Community-Medium/small",
                               hosp_size == "4.Teaching" ~ "Teaching"),
         mrp_sex = replace_na(sex.mrp, 'Missing'))


  
#######################################################
## Get medical history & medications 
######################################################
# Admissions (DAD)
# dd_dir <- "R:/DATA/2018-06-20/docs/data_dictionary_discharge-abstracts-database.xlsx"
# types <- c("G", "H", "N")
# fileDir <- "R:/DATA/2018-06-20/hospital/"
# # hospital data
# (hosp <- map_dfr(types, function(t){
# 
#   # get data dictionary
#   (dd <- read_xlsx(dd_dir, sheet = paste0("hospital.", t)) %>%
#      filter(!Name %in% "Line Feed") %>%
#      select(col_width = Length, col_name = `Name Abbrev`))
# 
#   # list all data files for version of interest
#   (fileNames <- paste0(fileDir,
#                        list.files("R:/DATA/2018-06-20/hospital/", pattern = t)))
# 
#   # read files
#   map_df(fileNames, function(file){
#     read_fwf(file, col_positions = fwf_widths(widths = dd$col_width, col_names = dd$col_name),
#              col_types = cols(.default = "c")) %>%
#       mutate(fileName = file) %>%
#       select(fileName, STUDYID, ADDATE,
#              starts_with("SEP"), TDAYS,
#              starts_with("DIAG")) %>%
#       filter(STUDYID %in% cohort$studyid.dad) %>%
#       mutate(SEPDATE = as.Date(SEPDATE)) %>%
#       filter(SEPDATE >= "2011-06-01")
#   })
# 
# }))
# # for cases with duplicates for studyid, admission date, discharge date, and LOS
# # group into single row, combining diagnostic codes
# # if different diagnostic codes for given column, e.g. DIAG1 different in two rows
# # take the first one
# hosp <- hosp %>%
#   group_by(STUDYID, ADDATE, SEPDATE, TDAYS, SEPDISP) %>%
#   summarise(across(everything(), function(x) ifelse(all(is.na(x)), NA, (na.omit(x)[1])))) %>%
#   ungroup()
# hosp <- hosp %>%
#   mutate(SEPDATE = ymd(SEPDATE),
#          ADDATE = ymd(ADDATE),
#          tlos = as.period(ADDATE %--% SEPDATE),
#          los.hosp = as.duration(tlos) / ddays(1)) %>%
#   select(-tlos)
# # save file
# saveRDS(hosp,'R:/working/G78717-Cohort/data/merged data/data_episode_of_care/dad.rds')
# 
# # Physician visits (MSP)
# fileDir <- "R:/DATA/2018-06-20/msp/"
# (fileNames <- c(list.files(fileDir, "2011"),
#                 list.files(fileDir, "2012"),
#                 list.files(fileDir, "2013"),
#                 list.files(fileDir, "2014"),
#                 list.files(fileDir, "2015"),
#                 list.files(fileDir, "2016")))
# # read in data dictionary
# dd_dir <- "R:/DATA/2018-06-20/docs/data_dictionary_medical-services-plan-payment-information-file.xlsx"
# dd <- read_xlsx(dd_dir, sheet = "msp.C") %>%
#   filter(!Name %in% "Line Feed") %>%
#   select(col_width = Length, col_name = `Name Abbrev`)
# # number of records in reach file
# file.sizes <- c(68927629,89883596,81069453,105703477,83875479,71585816)
# # read data in chunks
# readRow <- 10000000  ## read 10 million rows at a time
# # create chunks to read in file
# file.cuts <- lapply(file.sizes, function(x) seq(0,x, by = readRow))
# # use for loops - map functions caused memory error
# msp_lst <- list()
# for(i in 1:length(fileNames)){
#   fileName <- fileNames[i]
#   file.cut <- file.cuts[[i]]
#   print(paste(fileName, "with", file.sizes[i], "records"))
# 
#   msp_file_lst <- list()
#   index <- 1
#   getF <- paste0(fileDir, fileName)
# 
#   for(j in 1:length(file.cut)){
#     print(paste("reading chunk", j, "out of", length(file.cut)))
# 
#     msp_chunk <- tryCatch({
#       # read data
#       read_fwf(getF, fwf_widths(widths=dd$col_width, col_names=dd$col_name),
#                col_types=cols(.default="c"),
#                n_max=readRow, skip=file.cut[j]) %>%
#         as_tibble() %>%
#         select(STUDYID, servdate, servloc, feeitem, icd9, contains("icd9")) %>%
#         filter(STUDYID %in% cohort$studyid.dad) %>%
#         distinct() # throw away duplicate rows with same studyid, servicedate, and icd codes
#     }, error=function(e){
#       NULL
#     })
# 
#     if(!is.null(msp_chunk)){
#       msp_file_lst[[index]] <- msp_chunk
#       index <- index + 1
#     }
# 
#   }
# 
#   msp_file <- as_tibble(do.call("rbind", msp_file_lst)) %>% distinct()
#   msp_lst[[i]] <- msp_file
# }
# # bind all years of visits together
# msp <- as_tibble(do.call("rbind", msp_lst)) %>% distinct()
# # save file
# saveRDS(msp,'R:/working/G78717-Cohort/data/merged data/data_episode_of_care/msp.rds')


source('R:/working/G78717-Cohort/code/StaplesLab - get_med_hist_for_tableA.R')
get_med_hist(cohort,'R:/working/G78717-Cohort/data/merged data/data_episode_of_care/')
tableA_med_hist <- read.csv("R:/working/G78717-Cohort/data/merged data/data_episode_of_care/table-A-med-history.csv") %>%
  mutate(sepdate.dad = as.Date(sepdate.dad)) 

get_comorbidity_core <- function(com_list, df) {
  for (i in 1:length(com_list)) {
    name <- com_list[i]
    df[name] <- as.numeric(df[paste0(name,'_dad')] >= 1 | df[paste0(name,'_msp')] >= 2 )
  }
  return(df)
}

med_hist <- cohort %>% select(studyid.dad, sepdate.dad, G78717_feecode) %>%
  mutate(sepdate.dad = as.Date(sepdate.dad)) %>%
  left_join(tableA_med_hist %>% select(-los), by = c('studyid.dad', 'sepdate.dad')) %>%
  mutate(
    # number of hospitalizations >= 1
    num_hosp_great_1 = if_else(num_hosp_prev_year >= 1,1,0),
    # number of clinic visits >= 7
    num_clinic_great_7 = if_else(num_clinic_prev_year >= 7,1,0)
  ) %>% 
  get_comorbidity_core(com_list=sub("_dad.*", "", tableA_med_hist %>% select(ends_with("_dad")) %>% colnames())) %>%
  mutate(
    # Charlson comorbidity score
    cci = if_else(mi*1+chf*1+pvd*1+cvd*1+dem*1+copd*1+rheum*1+pud*1+
                    liver_mild*1+dm_nc*1+dm_compl*2+paraplegia*2+renal*2+cancer*2+liver_modsev*3+mets*6+hiv*6
                  >= 2,1,0)) %>%
  select(-ends_with(c('_dad','_msp'))) 

# Medication history 
source('R:/working/G78717-Cohort/code/StaplesLab - get_medication_for_tableA.R')
get_medication(cohort,'R:/working/G78717-Cohort/data/merged data/data_episode_of_care/')
tableA_medications <- read.csv("R:/working/G78717-Cohort/data/merged data/data_episode_of_care/table-A-medications.csv") %>%
  mutate(addate.dad = as.Date(addate.dad)) 

medications <- cohort %>% 
  select(studyid.dad, addate.dad, G78717_feecode) %>%
  mutate(addate.dad = as.Date(addate.dad)) %>%
  left_join(tableA_medications, by = c('studyid.dad' = 'STUDYID', 'addate.dad')) %>% 
  mutate(across(c(-studyid.dad,-addate.dad,-G78717_feecode,-num_active_med), 
                ~case_when(. >= 1 ~ 1, . == 0 ~ 0)),
         num_med = case_when(num_active_med == 0 ~ '0',
                             num_active_med == 1 ~ '1',
                             num_active_med >= 2 ~ '>= 2')) %>%
  select(-num_active_med)

#######################################################
# Most responsible diagnosis   
#######################################################
dsc_xw <- read_xlsx("R:/DATA/2018-06-20/docs/data_dictionary_discharge-abstracts-database.xlsx", sheet = "diagnosticshortcodes", skip=4)
dsc_freq_tbl <- cohort %>% select(studyid.dad, sepdate.dad, dsc.dad, G78717_feecode) %>%
  right_join(dsc_xw %>% select(DSC_2005, Description) %>% mutate(DSC_2005 = as.integer(DSC_2005)),
             by = c('dsc.dad' = 'DSC_2005')) %>% 
  group_by(dsc.dad, Description) %>% summarise(n=n()) %>% arrange(desc(n)) 

#write.csv(dsc_freq_tbl,file='results/dsc_freq_tbl.csv')

mrs_lst <- dsc_freq_tbl %>% head(n=10) %>% pull(Description)

mrs <- cohort %>% select(studyid.dad, sepdate.dad, dsc.dad, G78717_feecode) %>%
  left_join(dsc_xw %>% select(DSC_2005, Description) %>% mutate(DSC_2005 = as.integer(DSC_2005)),
            by = c('dsc.dad' = 'DSC_2005')) %>%  
  fastDummies::dummy_cols(select_columns = 'Description') %>%
  select(studyid.dad, sepdate.dad, G78717_feecode, ends_with(mrs_lst)) %>%
  mutate(sepdate.dad = as.Date(sepdate.dad))

# combine all them together
cohort_full <- cohort %>% 
  mutate(addate.dad = as.Date(addate.dad), sepdate.dad = as.Date(sepdate.dad)) %>%
  left_join(medications,by=c('studyid.dad','addate.dad','G78717_feecode')) %>%
  left_join(med_hist,by=c('studyid.dad','sepdate.dad','G78717_feecode')) %>%
  left_join(mrs,by=c('studyid.dad','sepdate.dad','G78717_feecode')) %>%
  distinct() 

write.csv(cohort_full, 
          file="R:/working/G78717-Cohort/data/merged data/data_episode_of_care/cohort_full_2023-09-07.csv",
          row.names = FALSE)

#######################################################
# Create all others variables
#######################################################
cohort_full <- read.csv(file="R:/working/G78717-Cohort/data/merged data/data_episode_of_care/cohort_full_2023-09-07.csv")

com <- read_excel('R:/working/G78717-Cohort/documentation/G78711-cohort - variable_name - JAS.xlsx') %>%
  filter(Description =='Comorbidities') %>% pull(`Variable name`)
med <- read_excel('R:/working/G78717-Cohort/documentation/G78711-cohort - variable_name - JAS.xlsx') %>%
  filter(Description =='Medications') %>% pull(`Variable name`)
#med <- colnames(cohort_full)[312:344] 
mrd <- cohort_full %>% dplyr::select(starts_with('Description_')) %>% colnames()
cov <- c("G78717_feecode","sex", "age", "age_cat","hhd_income", "popl_dens","tdays_prev_year",
         "num_hosp_prev_year","num_clinic_prev_year", "cci", "num_med", "fiscal_year", 
         "ambulanc", "hosp_size", "most_resp_serv", "num_med", "icu_stay","los","mrp_sex",
         "grad_exp", "grad_plc", "mrp_spec" ,"surgcase2.dad","entry.dad","alcdays.dad","tdays.dad",
         "deathdt.vs")

cohort_ec <- cohort_full %>%
  select(episode_of_care, studyid.dad, addate.dad, sepdate.dad, 
         ends_with('_new'), one_of(c(cov,com,med,mrd))) %>%
  group_by(episode_of_care) %>%
  arrange(sepdate.dad) %>%
  mutate_at(vars(med), ~ first(.)) %>%
  mutate_at(vars(com), ~ last(.)) %>%
  mutate_at(vars(mrd), ~ last(.)) %>%
  mutate(age_cat_new = last(age_cat),
         num_hosp_prev_year_new = first(num_hosp_prev_year),
         num_clinic_prev_year_new = first(num_clinic_prev_year),
         tdays_prev_year_new = first(tdays_prev_year),
         cci_new = last(cci),
         num_med_new = first(num_med),
         G78717_feecode_new = last(G78717_feecode),
         sex_new = last(sex),
         hhd_income_new = last(hhd_income),
         popl_dens_new = last(popl_dens),
         fiscal_year_new = last(fiscal_year),
         ambulanc_new = first(ambulanc),
         hosp_size_new = last(hosp_size),
         most_resp_serv_new = last(most_resp_serv),
         icu_stay_new = as.numeric(any(icu_stay == 1)),
         mrp_sex_new = last(mrp_sex),
         grad_exp_new = last(grad_exp),
         grad_plc_new = last(grad_plc),
         mrp_spec_new = last(mrp_spec),
         surgcase2.dad_new = as.numeric(any(surgcase2.dad == 'Y')),
         entry.dad_new = first(entry.dad),
         alcdays.dad_new = sum(alcdays.dad),
         tdays.dad_new = sum(tdays.dad),
         deathdt.vs_new = last(deathdt.vs)
      ) %>%
  ungroup() %>%
  select(episode_of_care,studyid.dad,ends_with('_new'), one_of(c(com,med,mrd))) %>%
  group_by(episode_of_care) %>% 
  distinct() %>%
  ungroup() 

  
colnames(cohort_ec)[c(3:34)] <- gsub('_new','',colnames(cohort_ec)[c(3:34)])
cohort_ec <- cohort_ec %>% mutate(addate.dad = as.Date(addate.dad),
                                  sepdate.dad = as.Date(sepdate.dad)) 


#######################################################
# Create response variable
#######################################################
cohort_ec <- cohort_ec %>% select(studyid.dad, sepdate.dad) %>% 
  left_join(cohort_ec %>% select(studyid.dad, addate.dad) %>% mutate(readmit_date = addate.dad), 
            by = c("studyid.dad")) %>% 
  filter(readmit_date >= sepdate.dad & readmit_date <= sepdate.dad + 30) %>%
  group_by(studyid.dad, sepdate.dad) %>% 
  slice_min(readmit_date) %>%
  select(-addate.dad) %>%
  unique() %>% 
  right_join(cohort_ec, by=c('studyid.dad','sepdate.dad')) %>%
  mutate(readmit = case_when(is.na(readmit_date) ~ 0, 
                             TRUE ~ 1),
         death = case_when(is.na(deathdt.vs) ~ 0,
                           as.Date(deathdt.vs) <= sepdate.dad + 30 ~ 1,
                           TRUE ~ 0),
         readmit_or_death = as.numeric(readmit|death)) 


write.csv(cohort_ec, 
          file="R:/working/G78717-Cohort/data/merged data/data_episode_of_care/cohort_ec_2023-09-07.csv",
          row.names = FALSE)

#######################################################
# Get propensity score
#######################################################
cohort_ec <- read.csv(file="R:/working/G78717-Cohort/data/merged data/data_episode_of_care/cohort_ec_2023-09-07.csv")

com <- c('mi','chf', 'pvd', 'cvd', 'dem', 'copd', 'rheum', 'pud', 'liver_mild', 
         'dm_nc', 'dm_compl', 'paraplegia', 'renal', 'liver_modsev', 'cancer', 'mets', 'hiv', 'drugs')
med <- colnames(cohort_ec)[54:86] 
mrd <- cohort_ec %>% dplyr::select(starts_with('Description_')) %>% colnames() 
cov <- c("sex", "age", "hhd_income", "popl_dens", "num_hosp_prev_year", "num_clinic_prev_year", "cci",  # comorbidities + medications
         "num_med", "fiscal_year", "ambulanc", "hosp_size", "most_resp_serv", # most responsible diagnosis
         "icu_stay", "los", "mrp_sex", "grad_exp", "grad_plc", "mrp_spec" )

ps.fit <- glm(G78717_feecode~., 
              data=cohort_ec %>% select(G78717_feecode,one_of(c(cov,com,med,mrd))), 
              family=binomial(link="logit"))

library(WeightIt)
ps_WeightIt <- ps.fit$fitted.values
w_WeightIt <- get_w_from_ps(ps=ps_WeightIt,treat=cohort_ec$G78717_feecode, estimand='ATO')

cohort_ec$ps_val <- ps_WeightIt
cohort_ec$ps_weight <- w_WeightIt1

# Save file
write.csv(cohort_ec, 
          file="R:/working/G78717-Cohort/data/merged data/data_episode_of_care/cohort_ec_2023-09-07.csv",
          row.names=F)


#######################################################
# Perform regression
load('stepAIC_out.RData')
backward$formula

summGLM <- function(glmFit){
  coef <- summary(glmFit)$coef
  
  # robust SE
  library(sandwich)
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
SummTable <- function(data,out_adj,out_unadj){
  out <- cbind(
    # Case and Control
    data %>%
      mutate(exposure = case_when(G78717_feecode==0~"Control",G78717_feecode==1~"Case")) %>%
      group_by(exposure) %>% 
      count() %>%
      pivot_wider(names_from=exposure,values_from=n) %>%
      mutate(prop_case=paste0('(',round(100*Case/(cohort_ec %>%
                                                    mutate(exposure = case_when(G78717_feecode==0~"Control",G78717_feecode==1~"Case")) %>%
                                                    group_by(exposure) %>% 
                                                    count() %>%
                                                    pivot_wider(names_from=exposure,values_from=n) %>% pull(Case)),1),')'),
             prop_control=paste0('(',round(100*Control/(cohort_ec %>%
                                                          mutate(exposure = case_when(G78717_feecode==0~"Control",G78717_feecode==1~"Case")) %>%
                                                          group_by(exposure) %>% 
                                                          count() %>%
                                                          pivot_wider(names_from=exposure,values_from=n) %>% pull(Control)),1),')')),
    # Unadjusted and adjusted OR
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

# readmission or death
out_adj <- glm(backward$formula,data=cohort_ec,family='binomial',weight=ps_weight)
out_unadj <- glm(readmit_or_death~G78717_feecode,data=cohort_ec,family='binomial',weight=ps_weight)
out <- SummTable(cohort_ec %>% filter(readmit_or_death==1),out_adj,out_unadj)

# readmission
out_adj <- glm(update(backward$formula, readmit~.),data=cohort_ec,family='binomial',weight=ps_weight)
out_unadj <- glm(readmit~G78717_feecode,data=cohort_ec,family='binomial',weight=ps_weight)
out <- SummTable(cohort_ec %>% filter(readmit==1),out_adj,out_unadj)

# death
out_adj <- glm(update(backward$formula, death~.),data=cohort_ec,family='binomial',weight=ps_weight)
out_unadj <- glm(death~G78717_feecode,data=cohort_ec,family='binomial',weight=ps_weight)
out <- SummTable(cohort_ec %>% filter(death==1),out_adj,out_unadj)
