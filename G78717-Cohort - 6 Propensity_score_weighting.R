#############################################
# Propensity score weighting
# Author: Daisy Yu
# Date: 2023-01-25
# Updated: 2023-03-02
#############################################

# Libraries
pkgs = c('tidyverse', 'lubridate', 'data.table','PSweight','dplyr','tableone','readxl','xlsx','fastDummies')
lapply(pkgs, library, character.only = TRUE)
mydir <- "R:/working/G78717-Cohort/data/merged data/"
setwd(mydir)

# Read in the cohort data
cohort_with_response <- read.csv("cohort_with_response_2023-03-01.csv") %>% 
  mutate(addate.dad = as.Date(addate.dad), sepdate.dad = as.Date(sepdate.dad), los = as.numeric(los))
cohort_tableA <- read.csv("cohort_tableA_2023-03-01.csv") %>%
  mutate(addate.dad = as.Date(addate.dad), sepdate.dad = as.Date(sepdate.dad))

# Get variables needed for propensity score
coh <- cohort_with_response %>% dplyr::select(-c(sex, age, los, G78717_feecode)) %>% 
  left_join(cohort_tableA, by=c('studyid.dad',"sepdate.dad","addate.dad")) %>%
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
med <- colnames(coh)[352:384] 
mrd <- coh %>% dplyr::select(starts_with('Description_')) %>% colnames() 
cov <- c("sex", "age", "hhd_income", "popl_dens", "num_hosp_prev_year", "num_clinic_prev_year", "cci",  # comorbidities + medications
         "num_med", "fiscal_year", "ambulanc", "hosp_size", "most_resp_serv", # most responsible diagnosis
         "icu_stay", "los", "mrp_sex", "grad_exp", "grad_plc", "mrp_spec" )

covform <- paste0(c(cov,com,med,mrd), collapse = "+" )
ps.formula <- formula(paste("G78717_feecode~", covform))


# Diagnostic plot
# plot(msstat, type = 'balance', weighted.var = T, threshold = 0.1, metric = 'ASD')
# plot(msstat, type = 'hist', weighted.var = T, threshold = 0.1, metric = 'ASD')
# plot(msstat, type = 'density', weighted.var = T, threshold = 0.1, metric = 'ASD')


####### Comparing different weighting method ######
# Using PSWeight: they use normailzied weights!
msstat <- SumStat(ps.formula = ps.formula, data=coh, weight='overlap', method='glm')
ps_PSweight <- msstat$propensity[,'1']
w_PSweight <- msstat$ps.weights$overlap
summary(msstat, weighted.var = T, metric = 'ASD') 
save(msstat,file="R:/working/G78717-Cohort/results/PSweight.RData")


# By hand 
ps.fit <- glm(ps.formula, data=coh,family=binomial(link="logit"))
ps_hand <- ps.fit$fitted.values
# Overlap weight formula:
# treatment == 1: 1-ps
# treatment == 0: ps
w_hand <- ifelse(coh$G78717_feecode==1, 1-ps_hand, ps_hand)


# Using WeightIt Package: generate the same results as hand calculation!
library(WeightIt)
ps_WeightIt <- ps.fit$fitted.values
w_WeightIt <- get_w_from_ps(ps=ps_WeightIt,treat=coh$G78717_feecode, estimand='ATO')


# Save the data with PS score and weights calculated by WeightIt packages
coh$ps_val <- ps_WeightIt
coh$ps_weight <- w_WeightIt
write.csv(coh, file='cohort_for_modelling_2023-03-22.csv', row.names=F)


# Get some diagnostic plot
# AUC and ROC curve
library(pROC)
roc_object <- roc(cohort$G78717_feecode, pred)
plot(roc_object, type='shape', col='lightblue')
auc(roc_object) # Area under the curve: 0.7748

# confusion matrix
library(caret)
expected_value <- as.factor(ifelse(pred<0.5,0,1))
conf_matrix <- confusionMatrix(data=expected_value,reference=as.factor(cohort$G78717_feecode), positive='1')
conf_matrix 



#################
##### Plots #####
#################
############# Comorbidity #############
unweighted <- summary(msstat)$unweighted[,'SMD']
overlap <- summary(msstat)$overlap[,'SMD']
df <- as.data.frame(cbind(unweighted,overlap)) 
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

############
cov_df <- df %>% 
  mutate(Name = rownames(df)) %>% 
  filter(!rownames(df) %in% com) %>% 
  pivot_longer(cols=c('unweighted','overlap'),names_to = 'Group', values_to = 'SMD')
write.csv(cov_df, file = 'R:/working/G78717-Cohort/data/data_for_love_plot.csv', row.names = FALSE)

cov_df <- read.csv('R:/working/G78717-Cohort/data/data_for_love_plot.csv')
cov_df[cov_df==''] <- NA
cov_df$order = seq(nrow(cov_df),1, by=-1)

forest_plot <- ggplot(cov_df) +
  theme_bw() +
  aes(x=SMD, y=order, col=Group) +
  geom_point() +
  geom_vline(xintercept = 0.1,linetype='dotted', size=0.25) +
  theme(
    axis.text = element_text(size = 10),
    legend.position = "none",
    axis.text.y = element_text(margin(0,0,0,0),
                               hjust=0,
                               #face=cov_df$Style,
                               colour = "black"),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid = element_blank(),
  ) + 
  xlab('Standardized Mean Differences')

table_plot <- ggplot(cov_df) +
  theme_bw() +
  aes(y=order) +
  geom_text(aes(label=gsub('\\s2','',Name),x=0),hjust=0) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank()
  ) 
  

grid::grid.draw(cbind(ggplotGrob(table_plot),ggplotGrob(forest_plot), size='last'))




