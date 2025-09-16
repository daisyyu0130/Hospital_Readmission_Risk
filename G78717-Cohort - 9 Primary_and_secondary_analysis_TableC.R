#############################################
# Secondary Analysis
# Author: Daisy Yu
# Date: 2023-03-30
# Updated: 2023-06-09
#############################################

# Libraries
pkgs = c('tidyverse', 'lubridate', 'data.table','PSweight','dplyr','tableone','readxl','xlsx','fastDummies',
         'car', 'corrplot', 'irr', 'performance','lme4')
lapply(pkgs, library, character.only = TRUE)
mydir <- "R:/working/G78717-Cohort/results"
setwd(mydir)


# Read in the cohort data
cohort <- read.csv('R:/working/G78717-Cohort/data/merged data/cohort_for_modelling_2023-03-22.csv') %>%
  mutate(most_resp_serv = as.factor(most_resp_serv),
         surgcase2.dad = case_when(surgcase2.dad=='Y'~'Yes',TRUE~'No'),
         addate.dad = as.Date(addate.dad), 
         sepdate.dad = as.Date(sepdate.dad))


# Load the backward selection results
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
SummTable <- function(data,out_adj,out_unadj) {
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
                      dplyr::select(OR.ci,pvalue) %>%
                      rename(OR.ci.adj=OR.ci,pvalue.adj=pvalue) %>% 
                      mutate(pvalue.adj=round(pvalue.adj,3)),
                    summGLM(out_unadj) %>% 
                      filter(coef=='G78717_feecode') %>%
                      dplyr::select(OR.ci,pvalue) %>%
                      rename(OR.ci.unadj=OR.ci,pvalue.unadj=pvalue) %>%
                      mutate(pvalue.unadj=round(pvalue.unadj,3))))
  ) %>%
    unite(col='Case',c('Case','prop_case'),sep=' ') %>%
    unite(col='Control',c('Control','prop_control'),sep=' ') %>%
    unite(col='Unadjusted',c('OR.ci.unadj','pvalue.unadj'),sep=', ') %>%
    unite(col='Adjusted',c('OR.ci.adj','pvalue.adj'),sep=', ') %>%
    dplyr::select(Case, Control, Unadjusted, Adjusted)
  
  return(out)
}


# Primary outcome
prim_out_adj <- glm(backward$formula,data=cohort,family='binomial',weight=ps_weight)
prim_out_unadj <- glm(readmit_or_death~G78717_feecode,data=cohort,family='binomial',weight=ps_weight)
prim <- SummTable(cohort %>% filter(readmit_or_death==1),prim_out_adj,prim_out_unadj)


# Subsequent hospital readmission
readmit_out_adj <- glm(update(backward$formula, readmit~.),data=cohort,family='binomial',weight=ps_weight)
readmit_out_unadj <- glm(readmit~G78717_feecode,data=cohort,family='binomial',weight=ps_weight)
readmit <- SummTable(cohort %>% filter(readmit==1),readmit_out_adj,readmit_out_unadj)


# Death
death_out_adj <- glm(update(backward$formula, death~.),data=cohort,family='binomial',weight=ps_weight)
death_out_unadj <- glm(death~G78717_feecode,data=cohort,family='binomial',weight=ps_weight)
death <- SummTable(cohort %>% filter(death==1),death_out_adj,death_out_unadj)


# Subsequent physician clinical visits
msp <- readRDS('R:/working/G78717-Cohort/data/merged data/msp.rds')

coh_clinic_visit <- cohort %>% 
  left_join(msp %>% 
              filter(!servloc %in% c('D','E','F','G','H','I')) %>% 
              dplyr::select(STUDYID, servdate) %>% 
              distinct() %>%
              mutate(servdate = as.Date(as.character(servdate), format = "%Y%m%d")),
            by = c("studyid.dad"="STUDYID")) %>% 
  filter(servdate >= sepdate.dad & servdate <= sepdate.dad + 30) %>% 
  group_by(studyid.dad, addate.dad, sepdate.dad) %>% 
  summarise(num_clinic = n()) %>%
  right_join(cohort, by = c("studyid.dad","addate.dad","sepdate.dad")) %>%
  mutate(num_clinic = case_when(is.na(num_clinic)~0, TRUE~1))

clinic_visit_out_adj <- glm(update(backward$formula,num_clinic~.),data=coh_clinic_visit,family='binomial',weight=ps_weight)
clinic_visit_out_unadj <- glm(num_clinic~G78717_feecode,data=coh_clinic_visit,family='binomial',weight=ps_weight)
clinic_visit <- SummTable(coh_clinic_visit %>% filter(num_clinic==1),clinic_visit_out_adj,clinic_visit_out_unadj)


# Subsequent emergency department visits
dd <- read_xlsx("R:/DATA/2018-06-29/docs/data_dictionary_national-ambulatory-care-reporting-system-201213-onwards.xlsx",
                sheet = "nacrs.A") %>%
  filter(!Name %in% "Line Feed") %>%
  dplyr::select(col_width = Length, col_name = `Name Abbrev`)

nacrs <- read_fwf("R:/DATA/2018-06-29/nacrs/nacrs_rpt.dat.gz",
                  col_positions = fwf_widths(widths = dd$col_width,
                                             col_names = dd$col_name),
                  col_types = cols(.default = "c"))

coh_ed_visit <- cohort %>% 
  dplyr::select(studyid.dad,addate.dad,sepdate.dad) %>%
  left_join(nacrs %>% dplyr::select(NACRS.STUDYID, NACRS.TRIAGEDATETIME),
            by=c('studyid.dad'='NACRS.STUDYID')) %>%
  mutate(NACRS.TRIAGEDATETIME = as.Date(NACRS.TRIAGEDATETIME)) %>%
  filter(NACRS.TRIAGEDATETIME >= sepdate.dad & NACRS.TRIAGEDATETIME <= sepdate.dad + 30) %>%
  right_join(cohort,by=c('studyid.dad','addate.dad','sepdate.dad')) %>%
  group_by(studyid.dad,addate.dad,sepdate.dad) %>%
  slice_head(n=1) %>%
  mutate(ed_vis = case_when(is.na(NACRS.TRIAGEDATETIME)~0, TRUE~1)) 

ed_vis_out_adj <- glm(update(backward$formula, ed_vis~.),data=coh_ed_visit,family='binomial',weight=ps_weight)
ed_vis_out_unadj <- glm(ed_vis~G78717_feecode,data=coh_ed_visit,family='binomial',weight=ps_weight)
ed_vis <- SummTable(coh_ed_visit %>% filter(ed_vis==1),ed_vis_out_adj,ed_vis_out_unadj)


# Prescription fills for selected indicated medications
pnet <- read.csv('R:/working/G78717-COhort/data/raw data/pnet_all.csv') %>%
  mutate(DE.SRV_DATE = as.Date(DE.SRV_DATE)) 
pnet_xw <- read_excel("R:/working/G78717-Cohort/data/crosswalks/G78717-Cohort - PNet XW (from MedGrp.slim) - 2022-11-16.xls")
icd10_cm <- readxl::read_xlsx("U:/Readmits/Data/Crosswalks/MVC-ICD - ICD10CA comorbidity XW - 2022-02-21.xlsx") 
icd10_list <- icd10_cm %>% filter(pmh_name %in% c('mi','chf','cihd')) %>% pull(icd10_code)

coh_ind_med <- cohort %>% 
  # filter subset based on DIAGX1
  filter(diagx1.dad %in% icd10_list) %>%
  dplyr::select(studyid.dad,addate.dad,sepdate.dad) %>%
  # get DINPIN
  left_join(pnet %>% dplyr::select(STUDYID, DE.SRV_DATE, HP.DIN_PIN),by = c("studyid.dad" = "STUDYID")) %>%
  filter(DE.SRV_DATE >= sepdate.dad & DE.SRV_DATE <= sepdate.dad + 30) %>% 
  # use XW to find drug names
  left_join(pnet_xw,by=c('HP.DIN_PIN'='DIN_PIN')) %>%
  # filter indicated medications: specific beta blocking agents (bisoprolol,carvedilol,metoprolol)
  filter(HIVE_DRUG %in% c('BISOPROLOL','CARVEDILOL','METOPROLOL')) %>%
  mutate(ind_med=1) %>%
  right_join(cohort,by=c('studyid.dad','addate.dad','sepdate.dad')) %>%
  group_by(studyid.dad,addate.dad,sepdate.dad) %>%
  slice_head(n=1) %>%
  mutate(ind_med=replace_na(ind_med,0))
  
ind_med_out_adj <- glm(update(backward$formula, ind_med~.),data=coh_ind_med,family='binomial',weight=ps_weight)
ind_med_out_unadj <- glm(ind_med~G78717_feecode,data=coh_ind_med,family='binomial',weight=ps_weight)
ind_med <- SummTable(coh_ind_med %>% filter(ind_med==1),ind_med_out_adj,ind_med_out_unadj)

# Proportion
num <- coh_ind_med  %>% filter(ind_med==1 & G78717_feecode==1 & diagx1.dad %in% icd10_list) %>% nrow()
dem <- coh_ind_med %>% filter(G78717_feecode==1 & diagx1.dad %in% icd10_list) %>% nrow()
print(num/dem)

num <- coh_ind_med  %>% filter(ind_med==1 & G78717_feecode==0 & diagx1.dad %in% icd10_list) %>% nrow()
dem <- coh_ind_med %>% filter(G78717_feecode==0 & diagx1.dad %in% icd10_list) %>% nrow()
print(num/dem)


# Prescription fills for selected contraindicated medications
pnet_PIMS <- read_excel("R:/working/G78717-Cohort/data/crosswalks/G78717-Cohort - PNet PIMS XW (from MedGrp.PIMS.xls) - 2023-06-08.xls") %>%
  filter(G78717_GRP=='PIMs')

# med_list <- toupper(c('Alprazolam','Estazolam','Lorazepam','Oxazepam','Temazepam','Triazolam','Clorazepate',
#               'Chlordiazepoxide','Clonazepam','Diazepam','Flurazepam','Quazepam','Meprobamate','Eszopiclone',
#               'Zolpidem','Zaleplon','Amobarbital','Butabarbital','Butalbital','Mephobarbital',
#               'Pentobarbital','Phenobarbital','Secobarbital','Amitriptyline','Amoxapine','Clomipramine',
#               'Desipramine','Imipramine','Nortriptyline','Paroxetine','Protriptyline','Trimipramine',
#               'Chlorpropamide','Glyburide','Clonidine','Guanabenz','Guanfacine','Methyldopa','Brompheniramine',
#               'Carbinoxamine','Chlorpheniramine','Clemastine','Cyproheptadine','Dexbrompheniramine',
#               'Dexchlorpheniramine','Dimenhydrinate','Diphenhydramine','Doxylamine','Hydroxyzine','Meclizine',
#               'Promethazine','Triprolidine','Atropine','Belladonna alkaloids','Clidinium-Chlordiazepoxide',
#               'Dicyclomine','Hyoscyamine','Propantheline','Scopolamine','Carisoprodol','Chlorzoxazone',
#               'Cyclobenzaprine','Metaxalone','Methocarbamol','Orphenadrine'))

coh_contraind_med <- cohort %>%
  # filter cohort based on age >= 65
  filter(age >= 65) %>% 
  dplyr::select(studyid.dad,addate.dad,sepdate.dad) %>%
  # get DINPIN
  left_join(pnet %>% dplyr::select(STUDYID, DE.SRV_DATE, HP.DIN_PIN),by = c("studyid.dad" = "STUDYID")) %>%
  filter(DE.SRV_DATE >= sepdate.dad & DE.SRV_DATE <= sepdate.dad + 30) %>% 
  # use XW to find drug names
  left_join(pnet_PIMS,by=c('HP.DIN_PIN'='DIN_PIN')) %>%
  # filter contraindicated medications:
  filter(!is.na(G78717_GRP)) %>%
  mutate(contraind_med=1) %>%
  right_join(cohort,by=c('studyid.dad','addate.dad','sepdate.dad')) %>%
  group_by(studyid.dad,addate.dad,sepdate.dad) %>%
  slice_head(n=1) %>%
  mutate(contraind_med=replace_na(contraind_med,0))

contraind_med_out_adj <- glm(update(backward$formula, contraind_med~.),data=coh_contraind_med,family='binomial',weight=ps_weight)
contraind_med_out_unadj <- glm(contraind_med~G78717_feecode,data=coh_contraind_med,family='binomial',weight=ps_weight)
contraind_med <- SummTable(coh_contraind_med %>% filter(contraind_med==1),contraind_med_out_adj,contraind_med_out_unadj)

# Proportion
num <- coh_contraind_med  %>% filter(contraind_med==1 & G78717_feecode==1 & age>=65) %>% nrow()
dem <- coh_contraind_med %>% filter(G78717_feecode==1 & age>=65) %>% nrow()
num/dem

num <- coh_contraind_med  %>% filter(contraind_med==1 & G78717_feecode==0 & age>=65) %>% nrow()
dem <- coh_contraind_med %>% filter(G78717_feecode==0 & age>=65) %>% nrow()
num/dem

# Put all together and create Table C
tableC <- rbind(prim,readmit,clinic_visit,ed_vis,death,ind_med,contraind_med) %>%
  as.data.frame()
tableC$Outcome <- c('Primary outcome','Hospital readmission','Physician clinic visit',
                    'Emergency department visit','Death','Indicated medications',
                    'Contraindicated medications')
tableC <- tableC %>% relocate(Outcome)
    

write.xlsx(tableC,file="R:/working/G78717-Cohort/results/G78717-Cohort - TableC v1.xlsx")



