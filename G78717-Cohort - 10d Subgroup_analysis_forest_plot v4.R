#############################################
# Subgroup analysis: Forest Plot
# Author: Daisy Yu, John A Staples
# Date: 2023-05-04
# Updated: 2023-09-19
#############################################


# Libraries
pkgs = c('tidyverse', 'lubridate', 'data.table','PSweight','dplyr','tableone','readxl','xlsx','fastDummies',
         'car', 'corrplot', 'irr', 'performance','lme4', 'sandwich')
lapply(pkgs, library, character.only = TRUE)
mydir <- "R:/working/G78717-Cohort/results/"
setwd(mydir)

# Read in the subgroup analysis results
results.subgroups <- read.csv("R:/working/G78717-Cohort/results/G78717-Cohort - results.subgroups - 2023-09-18.csv") %>% 
  as_tibble()

# Attach the whole_cohort value
# Read in the cohort data
load('stepAIC_out.RData')
cohort <- read.csv('R:/working/G78717-Cohort/data/merged data/cohort_for_modelling_2023-03-22.csv') %>%
  mutate(most_resp_serv = as.factor(most_resp_serv),
         surgcase2.dad = case_when(surgcase2.dad=='Y'~'Yes',TRUE~'No'))
prim_out_adj <- glm(backward$formula,data=cohort,family='binomial',weight=ps_weight)

whole_cohort <- as.data.frame(summary(prim_out_adj)$coef)
colnames(whole_cohort) <- c('Estimate','std.error','Z','p.value')
whole_cohort <- whole_cohort[2,] %>%
  mutate(estimate = exp(Estimate),
         se.robust = sqrt(diag(vcovHC(prim_out_adj, type = "HC1")))[2],
         conf.low = exp(Estimate-1.96*se.robust),
         conf.high = exp(Estimate+1.96*se.robust),
         adj = 'adjusted',
         subgroup_name = 'whole_cohort',
         strata_name = 'whole_cohort',
         Case = NA,
         Control = NA,
         rtype = NA) %>%
  dplyr::select(subgroup_name,strata_name,Case,Control,estimate,conf.low,conf.high,p.value,std.error,rtype,se.robust,adj)

rownames(whole_cohort) <- NULL

results.subgroups <- rbind(results.subgroups, whole_cohort)

# remove most responsible service = Others'
results.subgroups <- results.subgroups %>% filter(!(subgroup_name=='most_resp_serv'& strata_name=='Others'))

  

###############################################################################
# Set up labels for the tornado plot

# This can be used to export your groups, modify the names, indenting, bolding etc in Excel, then re-import (this is easier than coding it in R)
# results.subgroups %>%
#  select(subgroup_name, strata_name) %>%
#  write.xlsx("G78717-Cohort - Subgroups tornado plot labels v5.xlsx")

label_data <- read.xlsx("G78717-Cohort - Subgroups tornado plot labels v5 (modified).xlsx",sheetName='Sheet1') %>%
  tibble() %>%
  mutate(label = if_else(indent_nobold == 1, paste0("     ", strata_name_new), strata_name_new),
         fontface = ifelse(indent_nobold == 0, "bold", "plain"))

subgroup_tornado_data <- left_join(label_data,
                                   results.subgroups,
                                   c("subgroup_name", "strata_name")) %>%
  mutate(label = factor(label, levels = rev(label_data$label))) 



###############################################################################
# Create main tornado plot

# Read in Staples Lab ggplot theme
source("R:/working/Reproducible script/Staples Lab - ggplot theme - 2022-04-29.R")


subgroup_tornado_data_main <- subgroup_tornado_data %>% 
  filter(main_forest == 1)

ggplot(dat = subgroup_tornado_data_main) +
  geom_hline(yintercept=1, lty=2) + 
  geom_linerange(aes(x = label, ymin = conf.low, ymax = conf.high, colour = subgroup_name)) + 
  geom_point(aes(x = label, y = estimate, colour = subgroup_name, size = 1/std.error), shape = 15) + 
  coord_flip() + 
  labs(x = "", y = "Odds ratio") +
  scale_y_continuous(trans="log",
                     breaks=c(0.7,0.8,0.9,1.0,1.11,1.25,1.43),
                     limits=c(0.7,1.43)) +
  scale_colour_viridis_d(direction = -1, begin = 0, end = 0.8) +
  theme_stapleslab + 
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_text(face = if_else(rev(subgroup_tornado_data_main$fontface) == "bold", "bold", "plain"),
                                   colour = if_else(rev(subgroup_tornado_data_main$space) == 0, "black", "white"),
                                   margin = margin(0,0,0,0),
                                   hjust = 0)) + 
  guides(size = "none",
         colour = "none") 
ggsave(paste0("G78717-Cohort - Subgroup analysis tornado plot - ", today(), ".pdf"), width=10, height = 19, units="in", dpi = 300)




