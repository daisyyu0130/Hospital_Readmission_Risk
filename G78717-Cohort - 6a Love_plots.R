#############################################
# Love plots
# Author: Daisy Yu
# Date: 2023-03-02
# Updated: 2023-04-27
#############################################

# Libraries
pkgs = c('tidyverse', 'lubridate', 'data.table','PSweight','dplyr','tableone','readxl','xlsx','fastDummies')
lapply(pkgs, library, character.only = TRUE)
mydir <- "R:/working/G78717-Cohort/results/"
setwd(mydir)

# Load result of propensity score weighting
load('PSweight.RData')
# Read in cohort data
coh <- read.csv('R:/working/G78717-Cohort/data/merged data/cohort_for_modelling_2023-03-22.csv')

com <- c('mi','chf', 'pvd', 'cvd', 'dem', 'copd', 'rheum', 'pud', 'liver_mild', 'dm_nc', 'dm_compl', 'paraplegia',
         'renal', 'liver_modsev', 'cancer', 'mets', 'hiv', 'drugs')
med <- colnames(coh)[352:384] 
mrd <- coh %>% dplyr::select(starts_with('Description_')) %>% colnames() 
cov <- c("sex", "age", "hhd_income", "popl_dens", "num_hosp_prev_year", "num_clinic_prev_year", "cci",  # comorbidities + medications
         "num_med", "fiscal_year", "ambulanc", "hosp_size", "most_resp_serv", # most responsible diagnosis
         "icu_stay", "los", "mrp_sex", "grad_exp", "grad_plc", "mrp_spec" )

# Save propensity results as a csv file
unweighted <- summary(msstat)$unweighted[,'SMD']
overlap <- summary(msstat)$overlap[,'SMD']
df <- as.data.frame(cbind(unweighted,overlap)) 
View(df)
write.csv(df,file='PSweighted_results.csv')

############# Comorbidity #############
com_df <- df %>% 
  mutate(Name = rownames(df)) %>% 
  filter(rownames(df) %in% com) %>% 
  pivot_longer(cols=c('unweighted','overlap'),names_to = 'Group', values_to = 'SMD')

label <- c('Myocardial Infarction','Congestive Heart Failure','Periphral Vascular Disease',
           'Cerebrovascular Disease', 'Dementia', 'Chronic Pulmonary Disease', 'Rheumatic disease',
           'Peptic Ulcer Disease', 'Mild Liver Disease', 'Diabetes without complications',
           'Diabetes with complications', 'Paraplegia and Hemiplegia', 'Renal Disease',
           'Moderate or Severe Liver Disease', 'Cancer', 'Metastatic Carcinoma', 'HIV/AIDS',
           'Any substance use disorder')

ggplot(data=com_df, aes(x=Name, y=SMD, col=Group)) + 
  geom_point(shape='square',size=4, position = position_dodge(width=0.3)) +
  geom_hline(yintercept = 0.1,linetype='dotted', linewidth=0.25) +
  coord_flip() +
  labs(y="Standardized Mean Differences", x="") + 
  scale_x_discrete(labels = label) + 
  theme_bw(base_size = 13) +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 14),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(margin(0,0,0,0),
                                   hjust=0,
                                   #face=pDat$fontface,
                                   colour = "black"))

ggsave("R:/working/G78717-Cohort/results/ps_com_love plot.tiff", 
       width=14, height=8, units="in", dpi=300,
       compression = "lzw", type='cairo')


############# Medication #############
med_df <- df %>% 
  mutate(Name = rownames(df)) %>% 
  filter(rownames(df) %in% med) %>% 
  pivot_longer(cols=c('unweighted','overlap'),names_to = 'Group', values_to = 'SMD')

#label <- c()

ggplot(data=med_df, aes(x=Name, y=SMD, col=Group)) + 
  geom_point(shape='square',size=4, position = position_dodge(width=0.3)) +
  geom_hline(yintercept = 0.1,linetype='dotted', linewidth=0.25) +
  coord_flip() +
  labs(y="Standardized Mean Differences", x="") + 
  #scale_x_discrete(labels = label) +
  scale_x_discrete() +
  theme_bw(base_size = 13) +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 14),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(margin(0,0,0,0),
                                   hjust=0,
                                   #face=pDat$fontface,
                                   colour = "black"))

ggsave("R:/working/G78717-Cohort/results/ps_med_love plot.tiff", 
       width=14, height=8, units="in", dpi=300,
       compression = "lzw", type='cairo')


############# Most responsible diagnosis #############
mrd_df <- df %>% 
  mutate(Name = rownames(df)) %>% 
  filter(rownames(df) %in% mrd) %>% 
  pivot_longer(cols=c('unweighted','overlap'),names_to = 'Group', values_to = 'SMD')

label <- gsub('\\.',' ', gsub(".*_",'',gsub('.{0,10}$','',mrd)))

ggplot(data=mrd_df, aes(x=Name, y=SMD, col=Group)) + 
  geom_point(shape='square',size=4, position = position_dodge(width=0.3)) +
  geom_hline(yintercept = 0.1,linetype='dotted', linewidth=0.25) +
  coord_flip() +
  labs(y="Standardized Mean Differences", x="") + 
  scale_x_discrete(labels = label) + 
  theme_bw(base_size = 13) +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 14),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(margin(0,0,0,0),
                                   hjust=0,
                                   #face=pDat$fontface,
                                   colour = "black"))

ggsave("R:/working/G78717-Cohort/results/ps_mrd_love plot.tiff", 
       width=14, height=8, units="in", dpi=300,
       compression = "lzw", type='cairo')
