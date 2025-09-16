#############################################
# Additional Sensitivity analysis: Does not exclude discharges to long term care
# Author: Daisy Yu
# Date: 2023-06-28
# Updated: 2023-08-09
#############################################

# Libraries
pkgs = c('tidyverse', 'lubridate', 'data.table','PSweight','dplyr','tableone','readxl','xlsx','fastDummies',
         'car', 'corrplot', 'irr', 'performance','lme4','janitor','sandwich')
lapply(pkgs, library, character.only = TRUE)
mydir <- "R:/working/G78717-Cohort/results/"
setwd(mydir)

# Read in the cohort data
dad_coh <- read.csv("R:/working/G78717-Cohort/data/merged data/dad_coh.csv")

#############################################################
# Merge with demographics data 
#############################################################
demo <- read.csv("R:/working/G78717-Cohort/data/raw data/demo_all.csv") %>% 
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
         sepdate.dad = as.Date(sepdate.dad))

# Create variable for indexyear, convert to integer (census year is integer) 
# Create age variable 
dad_demo <- dad_demo %>% 
  mutate(indexyr = as.integer(format(as.Date(sepdate.dad, format="%Y-%m-%d"), "%Y")),
         age = (as.period(interval(birthdt, sepdate.dad)))@year) 

dad_demo <- dad_demo %>%
  filter(!is.na(sepdate.dad))

#############################################################
# Apply exclusion criteria
#############################################################
crosswalk_file <- read.csv("R:/working/J Staples working 2019-07-09/Eligible for G78717 based on DOC_SPEC in DAD.csv") %>%
   rename(specialty_in_crosswalk=specialty)
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
  # ended in discharge to other facilities
  filter(sepdisp.dad %in% c(2,4,5)) %>%
  # exclude hospitalizations that were transferred from/to another hospital
  # We keep: blank (not a transfer)
  filter(hospfrom.dad == 6 | hospfrom.dad == 7 | is.na(hospfrom.dad)) %>%
  # filter(hospto.dad == 6 | hospto.dad == 7 | is.na(hospto.dad)) %>%
  # Exclude age < 18
  filter(!age < 18) 

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

# deaths %>% sample_n(5)
# deaths %>% group_by(native) %>% dplyr::summarise(n = n())

# Add .df suffix to all vars
colnames(deaths) <-  paste0(colnames(deaths), ".vs")

# Apply to the deaths dataset before merging + check row count.
deaths <- deaths %>% group_by(studyid.vs) %>% slice_max(deathdt.vs)
deaths <- deaths %>% group_by(studyid.vs) %>% slice_head(n=1)

# Left_join based on studyid since VS would only include rows if someone dies.
dad_dcprd <- dad_dcpr %>%
  left_join((deaths %>% select(-datayr.vs)), by = c("studyid.dad" = "studyid.vs"))

dad_dcprd <- dad_dcprd %>% mutate(indexdate = as.Date(sepdate.dad))
#deaths %>% group_by(datayr.vs) %>% dplyr::summarise(n= n())


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
cohort <- dad_dcprd
cohort <- cohort %>%
  left_join(cm2012_2016, by=c("studyid.dad" = "studyid.dad","sepdate.dad" = "sepdate.dad")) %>%
  mutate(G78717_feecode = case_when(feeitem.msp == 78717 ~ 1, TRUE ~ 0))

write.csv(cohort,file='R:/working/G78717-Cohort/data/merged data/data_with_LTC/cohort_with_LTC.csv',row.names=F)

cohort <- read.csv('R:/working/G78717-Cohort/data/merged data/data_with_LTC/cohort_with_LTC.csv')
#######################################################
# Create Table A variables
######################################################
# Patient-level characteristics
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
  select(studyid.dad,sepdate.dad,age,age_cat,sex,qaippe,fsaType,G78717_feecode) 

# Medical history  
source('R:/working/G78717-Cohort/code/StaplesLab - get_med_hist_for_tableA.R')
get_med_hist(cohort,'R:/working/G78717-Cohort/data/merged data/data_with_LTC/')
tableA_med_hist <- read.csv("R:/working/G78717-Cohort/data/merged data/data_with_LTC/table-A-med-history.csv") 

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

# Medication history 
source('R:/working/G78717-Cohort/code/StaplesLab - get_medication_for_tableA.R')
get_medication(cohort,'R:/working/G78717-Cohort/data/merged data/data_with_LTC/')
tableA_medications <- read.csv("R:/working/G78717-Cohort/data/merged data/data_with_LTC/table-A-medications.csv")

medications <- cohort %>% select(studyid.dad, addate.dad, G78717_feecode) %>%
  left_join(tableA_medications, by = c('studyid.dad' = 'STUDYID', 'addate.dad')) %>% 
  mutate(across(c(-studyid.dad,-addate.dad,-G78717_feecode,-num_active_med), 
                ~case_when(. >= 1 ~ 1, . == 0 ~ 0)),
         num_med = case_when(num_active_med == 0 ~ '0',
                             num_active_med == 1 ~ '1',
                             num_active_med >= 2 ~ '>= 2')) %>%
  select(-num_active_med)

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
         icu_stay = ifelse(icudays.dad > 0, 1, 0),
         los = case_when(los >= 5 & los <= 7 ~ '1.5 to 7 days',
                         los >= 8 & los <= 29 ~ '2.8 to 29 days',
                         los >= 30 ~ '3.>= 30 days')) %>%
  select(studyid.dad,sepdate.dad,fiscal_year,ambulanc,hosp_size,MD_category,icu_stay,los,G78717_feecode)


# Most responsible diagnosis   
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

# Combine data sets
cohort_tableA <- list(cohort %>% select(studyid.dad,sepdate.dad,addate.dad,G78717_feecode),patient_char,med_hist %>% select(-los),hosp_idx_char,mrs) %>% 
  Reduce(function(df1,df2) left_join(df1, df2, by = c('studyid.dad', 'sepdate.dad', 'G78717_feecode')),.) %>%
  left_join(medications, by =c('studyid.dad','addate.dad','G78717_feecode')) 
write.csv(cohort_tableA, file='R:/working/G78717-Cohort/data/merged data/data_with_LTC/cohort_tableA.csv', row.names = FALSE)



cohort_tableA <- read.csv('R:/working/G78717-Cohort/data/merged data/data_with_LTC/cohort_tableA.csv')
#######################################################
# Create response variable 
######################################################
hosp <- readRDS('R:/working/G78717-Cohort/data/merged data/dad.rds')

# Get response variable: readmission within 30 days of discharge
# Get response variable: death within 30 days of discharge
cohort_with_response <- cohort %>% select(studyid.dad, sepdate.dad) %>% mutate(sepdate.dad=as.Date(sepdate.dad)) %>%
  left_join(hosp %>% select(STUDYID, ADDATE) %>% mutate(readmit_date=ADDATE), 
            by = c("studyid.dad"='STUDYID')) %>% 
  filter(readmit_date >= sepdate.dad & readmit_date <= sepdate.dad + 30) %>%
  group_by(studyid.dad, sepdate.dad) %>% 
  slice_min(readmit_date) %>%
  select(-ADDATE) %>%
  unique() %>% 
  right_join(cohort %>% mutate(sepdate.dad=as.Date(sepdate.dad)), by=c('studyid.dad','sepdate.dad')) %>%
  mutate(readmit = case_when(is.na(readmit_date) ~ 0, 
                             TRUE ~ 1),
         death = case_when(is.na(deathdt.vs) ~ 0,
                           as.Date(deathdt.vs) <= sepdate.dad + 30 ~ 1,
                           TRUE ~ 0),
         readmit_or_death = as.numeric(readmit|death))


#######################################################
# Get propensity score weights
######################################################
coh <- cohort_with_response %>% dplyr::select(-c(sex, age, los, G78717_feecode)) %>% 
  left_join(cohort_tableA %>%
              mutate(sepdate.dad=as.Date(sepdate.dad)), by=c('studyid.dad',"sepdate.dad","addate.dad")) %>%
  group_by(respphys.dad) %>%
  fill(sex.mrp, .direction='downup') %>%
  fill(gradplc.mrp, .direction='downup') %>%
  fill(gradyr.mrp, .direction='downup') %>%
  ungroup() %>%
  mutate(addate.dad = as.Date(addate.dad), 
         sepdate.dad = as.Date(sepdate.dad),
         most_resp_serv = MD_category,
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

class(coh) <- 'data.frame'

com <- c('mi','chf', 'pvd', 'cvd', 'dem', 'copd', 'rheum', 'pud', 'liver_mild', 'dm_nc', 'dm_compl', 'paraplegia',
         'renal', 'liver_modsev', 'cancer', 'mets', 'hiv', 'drugs')
med <-  colnames(coh)[352:384]
mrd <- coh %>% dplyr::select(starts_with('Description_')) %>% colnames()
cov <- c("sex", "age", "hhd_income", "popl_dens", "num_hosp_prev_year", "num_clinic_prev_year", "cci",  # comorbidities + medications
         "num_med", "fiscal_year", "ambulanc", "hosp_size", "most_resp_serv", # most responsible diagnosis
         "icu_stay", "los", "mrp_sex", "grad_exp", "grad_plc", "mrp_spec" )

covform <- paste0(c(cov,med,com,mrd), collapse = "+" )
ps.formula <- formula(paste("readmit_or_death~", covform))

library(WeightIt)
ps.fit <- glm(ps.formula, data=coh,family=binomial(link="logit"))
ps_WeightIt <- ps.fit$fitted.values
w_WeightIt <- get_w_from_ps(ps=ps_WeightIt,treat=coh$G78717_feecode, estimand='ATO')

coh$ps_val <- ps_WeightIt
coh$ps_weight <- w_WeightIt
write.csv(coh, file='R:/working/G78717-Cohort/data/merged data/data_with_LTC/cohort_for_modelling.csv', row.names=F)

# Get the backward selection result
coh <- read.csv('R:/working/G78717-Cohort/data/merged data/data_with_LTC/cohort_for_modelling.csv') %>%
  mutate(most_resp_serv = as.factor(most_resp_serv),
         surgcase2.dad = case_when(surgcase2.dad=='Y'~'Yes',TRUE~'No'),
         addate.dad = as.Date(addate.dad), 
         sepdate.dad = as.Date(sepdate.dad))

load('stepAIC_out.RData')

# Some useful functions
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
SummTable <- function(data,out_adj,out_unadj){
  out <- cbind(
    # Case and Control
    data %>%
      mutate(exposure = case_when(G78717_feecode==0~"Control",G78717_feecode==1~"Case")) %>%
      group_by(exposure) %>% 
      count() %>%
      pivot_wider(names_from=exposure,values_from=n) %>%
      mutate(prop_case=paste0('(',round(100*Case/(cohort %>%
                                                    mutate(exposure = case_when(G78717_feecode==0~"Control",G78717_feecode==1~"Case")) %>%
                                                    group_by(exposure) %>% 
                                                    count() %>%
                                                    pivot_wider(names_from=exposure,values_from=n) %>% pull(Case)),1),')'),
             prop_control=paste0('(',round(100*Control/(cohort %>%
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

out_adj <- glm(backward$formula,data=coh,family='binomial',weight=ps_weight)
out_unadj <- glm(readmit_or_death~G78717_feecode,data=coh,family='binomial',weight=ps_weight)
out <- SummTable(coh %>% filter(readmit_or_death==1),out_adj,out_unadj)

out_adj <- glm(backward$formula,data=coh1,family='binomial',weight=ps_weight)
out_unadj <- glm(readmit_or_death~G78717_feecode,data=coh1,family='binomial',weight=ps_weight)
out <- SummTable(coh %>% filter(readmit_or_death==1),out_adj,out_unadj)

# Subgroup analysis
coh1 <- coh %>% filter(sepdisp.dad==2)
coh2 <- coh %>% filter(sepdisp.dad %in% c(4,5))
coh3 <- coh2 %>% filter(hospto.dad == 6 | hospto.dad == 7 | is.na(hospto.dad)) # same as the cohort used for main analyses

# Cohort used for main analyses
cohort <- read.csv('R:/working/G78717-Cohort/data/merged data/cohort_for_modelling_2023-03-22.csv') %>%
  mutate(most_resp_serv = as.factor(most_resp_serv),
         surgcase2.dad = case_when(surgcase2.dad=='Y'~'Yes',TRUE~'No'),
         addate.dad = as.Date(addate.dad), 
         sepdate.dad = as.Date(sepdate.dad))

# calculate propensity score weights using only coh3
ps.fit <- glm(ps.formula, data=coh3, family=binomial(link="logit"))
psw <- get_w_from_ps(ps=ps.fit$fitted.values,treat=coh3$G78717_feecode, estimand='ATO')


# results are different
summary(glm(readmit_or_death~G78717_feecode,data=coh3,family='binomial',weight=ps_weight))
summary(glm(readmit_or_death~G78717_feecode,data=cohort,family='binomial',weight=ps_weight))

# results are the same 
summary(glm(readmit_or_death~G78717_feecode,data=coh3,family='binomial'))
summary(glm(backward$formula,data=cohort,family='binomial',weight=ps_weight))


coef_ltc <- summGLM(glm(backward$formula,data=coh,family='binomial',weight=ps_weight))
coef_old <- summGLM(glm(backward$formula,data=cohort,family='binomial',weight=ps_weight))

coef_ltc_no_ps <- summGLM(glm(backward$formula,data=coh,family='binomial'))
coef_old_no_ps <- summGLM(glm(backward$formula,data=cohort,family='binomial'))


Coef <- coef_ltc %>% 
  select(coef,OR.ci,pvalue) %>%
  mutate(sig_or_not = if_else(pvalue<=0.05,1,0)) %>%
  rename(OR.ci_ltc=OR.ci,pvalue_ltc=pvalue,sig_or_not_ltc=sig_or_not) %>%
  left_join(coef_old %>% 
              select(coef,OR.ci,pvalue) %>%
              mutate(sig_or_not = if_else(pvalue<=0.05,1,0)),
            by='coef')

Coef_no_ps <- coef_ltc_no_ps %>% 
  select(coef,OR.ci,pvalue) %>%
  mutate(sig_or_not = if_else(pvalue<=0.05,1,0)) %>%
  rename(OR.ci_ltc=OR.ci,pvalue_ltc=pvalue,sig_or_not_ltc=sig_or_not) %>%
  left_join(coef_old_no_ps %>% 
              select(coef,OR.ci,pvalue) %>%
              mutate(sig_or_not = if_else(pvalue<=0.05,1,0)),
            by='coef')

write.xlsx(x=Coef,file='coef_comparison.xlsx',sheetName='with ps')
write.xlsx(x=Coef_no_ps,file='coef_comparison_no_ps.xlsx',sheetName='without ps')


