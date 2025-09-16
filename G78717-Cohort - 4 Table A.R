#############################################
# Create Table A from baseline patient characteristics
# Author: Daisy Yu
# Date: 2022-09-08
# Updated: 2023-01-25
#############################################

# Libraries
pkgs = c('tidyverse', 'lubridate', 'data.table', 'dplyr','tableone','readxl','xlsx','fastDummies')
lapply(pkgs, library, character.only = TRUE)
setwd("R:/working/G78717-Cohort/")

# Read in the cohort data for Table A
cohort <- read.csv("data/merged data/cohort_2022-11-18.csv")

##############################
## Patient characteristics  ##
##############################
# Read in crosswalk files
fsa <- read.csv("data/Crosswalks/fsa_2016_urbanRural.csv")

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
  select(studyid.dad,sepdate.dad,age,age_cat,sex,qaippe,fsaType,G78717_feecode) 


##############################
##      Medical history     ##
##############################
# Read in crosswalk files
tableA_med_hist <- read.csv("data/merged data/table-A-med-history.csv") 

get_comorbidity_core <- function(com_list, df) {
  for (i in 1:length(com_list)) {
    name <- com_list[i]
    df[name] <- as.numeric(df[paste0(name,'_dad')] >= 1 | df[paste0(name,'_msp')] >= 2 )
  }
  return(df)
}

med_hist <- cohort %>% select(studyid.dad, sepdate.dad, G78717_feecode) %>%
  left_join(tableA_med_hist, by = c('studyid.dad', 'sepdate.dad')) %>%
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


# med_hist <- cohort %>% select(studyid.dad, sepdate.dad, G78717_feecode) %>%
#   left_join(tableA_med_hist, by = c('studyid.dad', 'sepdate.dad')) %>%
#   mutate(
#     # number of hospitalizations >= 1
#     num_hosp_great_1 = if_else(num_hosp_prev_year >= 1,1,0),
#     # number of clinic visits >= 7
#     num_clinic_great_7 = if_else(num_clinic_prev_year >= 7,1,0),
#     # each comorbidity
#     mi = as.numeric(mi_dad >= 1 | mi_msp >= 2),
#     chf = as.numeric(chf_dad >= 1 | chf_msp >= 2),
#     pvd = as.numeric(pvd_dad >= 1 | pvd_msp >= 2),
#     cd = as.numeric(cd_dad >= 1 | cd_msp >= 2),
#     dementia = as.numeric(dementia_dad >= 1 | dementia_msp >= 2),
#     copd = as.numeric(copd_dad >= 1 | copd_msp >= 2),
#     rheumatic_dis = as.numeric(rheumatic_dis_dad >= 1 | rheumatic_dis_msp >= 2),
#     pud = as.numeric(pud_dad >= 1 | pud_msp >= 2),
#     mild_liver = as.numeric(mild_liver_dad >= 1 | mild_liver_msp >= 2),
#     dm_nc = as.numeric(dm_nc_dad >= 1 | dm_nc_msp >= 2),
#     dm_compl = as.numeric(dm_compl_dad >= 1 | dm_compl_msp >= 2),
#     dm_nc_or_compl = as.numeric(dm_nc | dm_compl),
#     ph = as.numeric(ph_dad >= 1 | ph_msp >= 2),
#     renal_dis = as.numeric(renal_dis_dad >= 1 | renal_dis_msp >= 2),
#     mod_sev_liver = as.numeric(mod_sev_liver_dad >= 1 | mod_sev_liver_msp >= 2),
#     mild_mod_sev_liver = as.numeric(mild_liver | mod_sev_liver),
#     cancer = as.numeric(cancer_dad >= 1 | cancer_msp >= 2),
#     mc = as.numeric(mc_dad >= 1 | mc_msp >= 2),
#     cancer_or_mc = as.numeric(cancer | mc),
#     hiv = as.numeric(hiv_dad >= 1 | hiv_msp >= 2),
#     drugs = as.numeric(drugs_dad >= 1 | drugs_msp >= 2),
#     # Charlson comorbidity score
#     cci = if_else(mi*1+chf*1+pvd*1+cd*1+dementia*1+copd*1+rheumatic_dis*1+pud*1+
#                     mild_liver*1+dm_nc*1+dm_compl*2+ph*2+renal_dis*2+cancer*2+mod_sev_liver*3+mc*6+hiv*6
#                    >= 2,1,0)
#     ) %>% 
#   select(-ends_with(c('_dad','_msp'))) 


#################################
##      Medication history     ##
#################################
tableA_medications <- read.csv("data/merged data/table-A-medications.csv") 

medications <- cohort %>% select(studyid.dad, addate.dad, G78717_feecode) %>%
  left_join(tableA_medications, by = c('studyid.dad' = 'STUDYID', 'addate.dad')) %>% 
  mutate(across(c(-studyid.dad,-addate.dad,-G78717_feecode,-num_active_med), 
                ~case_when(. >= 1 ~ 1, . == 0 ~ 0)),
         num_med = case_when(num_active_med == 0 ~ '0',
                             num_active_med == 1 ~ '1',
                             num_active_med >= 2 ~ '>= 2')) %>%
  select(-num_active_med)


######################################
## Details of index hospitalization ##
######################################
# Read in crosswalk files
hosp_size_xw <- read.table("data/crosswalks/G78717-Readmits-Hospital_size-FINAL_v2_encrypted.dat");colnames(hosp_size_xw) <- c('Code',"Category")
md_xw <- read_excel("data/crosswalks/G78717-Cohort - MD specialty categorized by PATSERV in DAD v7.xlsx")

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
         icu_stay = ifelse(icudays.dad > 0, 1, 0),
         los = case_when(los >= 5 & los <= 7 ~ '1.5 to 7 days',
                         los >= 8 & los <= 29 ~ '2.8 to 29 days',
                         los >= 30 ~ '3.>= 30 days')) %>%
  select(studyid.dad,sepdate.dad,fiscal_year,ambulanc,hosp_size,MD_category,icu_stay,los,G78717_feecode)


######################################
##     Most responsible diagnosis   ##
######################################
# Read in crosswalk files
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
  select(studyid.dad, sepdate.dad, G78717_feecode, ends_with(mrs_lst))


######################################
##          Create Table A          ##
######################################
# Combine data sets
tableA <- list(cohort %>% select(studyid.dad,sepdate.dad,addate.dad,G78717_feecode),patient_char,med_hist,hosp_idx_char,mrs) %>% 
  Reduce(function(df1,df2) left_join(df1, df2, by = c('studyid.dad', 'sepdate.dad', 'G78717_feecode')),.) %>%
  left_join(medications, by =c('studyid.dad','addate.dad','G78717_feecode')) 

write.csv(tableA, file='R:/working/G78717-Cohort/data/merged data/cohort_tableA_2023-03-01.csv', row.names = FALSE)

# Vector of variables to summarize
myVars <- colnames(tableA)[!colnames(tableA) %in% c('studyid.dad','addate.dad','sepdate.dad','G78717_feecode')]

# Vector of non-normal variables 
biomarkers <- c('age','num_hosp_prev_year','tdays_prev_year','num_clinic_prev_year','num_distinct_med')

# Vector of categorical variables
catVars <- myVars[!myVars %in% biomarkers]

# Summarize all levels of categorical variables
tab1 <- CreateTableOne(vars = myVars, data = tableA, factorVars = catVars)
tab1_csv <- print(tab1, nonnormal = biomarkers, showAllLevels = T, formatOptions = list(big.mark=','), printToggle = F)

# Group by exposure category (G78717_feecode)
tab2 <- CreateTableOne(vars = myVars, strata = 'G78717_feecode', data = tableA, factorVars = catVars)
tab2_csv <- print(tab2, nonnormal = biomarkers, showAllLevels = T, formatOptions = list(big.mark=','), smd=TRUE, printToggle = F)

# Save tables
write.xlsx(tab1_csv,file="R:/working/G78717-Cohort/results/G78717-Cohort - TableB v1.xlsx",sheetName='sheet1')
write.xlsx(tab2_csv,file="R:/working/G78717-Cohort/results/G78717-Cohort - TableB v1.xlsx",sheetName='sheet2')

# Age-range
max(patient_char$age)-min(patient_char$age)
max(patient_char$age[patient_char$G78717_feecode==1])-min(patient_char$age[patient_char$G78717_feecode==1])
max(patient_char$age[patient_char$G78717_feecode==0])-min(patient_char$age[patient_char$G78717_feecode==0])










