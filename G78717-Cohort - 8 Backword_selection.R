#############################################
# Primary Analysis
# Author: Daisy Yu
# Date: 2023-03-30
# Updated: 2023-04-26
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
         surgcase2.dad = case_when(surgcase2.dad=='Y'~'Yes',TRUE~'No'))

variable_name <- read_excel('R:/working/G78717-Cohort/documentation/G78711-cohort - variable_name - JAS.xlsx')

force <- variable_name %>% filter(force==1) %>% pull(`Variable name`)
keep <- variable_name %>% filter(keep==1 & force==0) %>% pull(`Variable name`)
# med <- variable_name %>% filter(Description=='Medications') %>% pull(`Variable name`)
# com <- variable_name %>% filter(Description=='Comorbidities') %>% pull(`Variable name`)
# mrd <- variable_name %>% filter(Description=='Most responsible diagnosis') %>% pull(`Variable name`)

force_variable <- force[!force %in% c('readmit','death','readmit_or_death','ps_weight')]
keep_variable <- keep

##########################################################
# Remove NAs
cohort_new <- cohort %>% 
  dplyr::select(one_of(c(force_variable,keep_variable)),
                readmit_or_death,death,readmit,ps_weight) %>%
  na.omit() 

##########################################################
# Fit global model
# remove most_resp_diag
form_full <- formula(paste("readmit_or_death~", paste0(c(force_variable,keep_variable), collapse = "+" )))
full_model <- glm(form_full,data=cohort_new,family='binomial',weight=ps_weight)

# check VIF
library('MASS')
library('car')
library('corrplot')
vif_full_model <- round(vif(full_model),3)


# EPV global model
nrow(cohort_new)/(length(force_variable) + length(keep_variable)) # EPV = 1657.898, fine!

##########################################################
# Perform variable selection via backward elimination
# smallest possible model with those variable to be forced into the model
form_lower <- formula(paste("readmit_or_death~", paste0(force_variable,collapse = '+')))
lower_model <- glm(form_lower,data=cohort_new,family='binomial',weight=ps_weight)

backward <- stepAIC(object=full_model,scope=list(lower=lower_model),direction='backward',trace=0)
elim_vars <- names(coef(full_model))[!names(coef(full_model)) %in% names(coef(backward))]
save(backward,file='stepAIC_out.RData')

load('stepAIC_out.RData')
# form_final <- out$formula
# fit <- glm(form_final,data=cohort_new,family='binomial',weight=ps_weight)

##########################################################
# Save results
tbl <- as.data.frame(summary(backward)$coef)
colnames(tbl) <- c('Estimate','SE','Z','pval')
tbl <- tbl %>% 
  mutate(variable = rownames(tbl),
         OR = exp(Estimate),
         LL = exp(Estimate-1.96*SE),
         UU = exp(Estimate+1.96*SE)) %>%
  dplyr::select(variable,OR,SE,pval,LL,UU) 
tbl <- rapply(object=tbl,f=round,classes='numeric',how='replace',digits=4)
write.xlsx(tbl, file='final_model.xlsx',row.names = F)
write.xlsx(vif_full_model,'final_model.xlsx', sheet='VIF', append = T)

##########################################################
# Bootstrap
summGLM <- function(glmFit){
  coef <- summary(glmFit)$coef
  ci <- cbind(coef[,1] - qnorm(0.975)*coef[,2], coef[,1] + qnorm(0.975)*coef[,2])
  
  data.frame(rownames(coef), coef, ci) %>% 
    as_tibble %>% 
    rename(coef = 1, est = 2, se = 3, z = 4, pvalue = 5, ciLow = 6, ciHigh = 7) %>% 
    mutate(coef = as.character(coef),
           OR = exp(est)) %>% 
    mutate(OR.ci = paste0(formatC(OR, digits = 2, format = "f"), " (",
                          formatC(exp(ciLow), digits = 2, format = "f"), ", ",
                          formatC(exp(ciHigh), digits = 2, format = "f"), ")"))
}

full_model_summ <- summGLM(full_model)
lower_model_summ <- summGLM(lower_model)
backward_summ <- summGLM(backward)

# create dataframe to store results of bootstrap variable selection
boot_metrics <- tibble(predictors = full_model_summ %>% pull(coef),
                       global_est = full_model_summ %>% pull(est),
                       global_se = full_model_summ %>% pull(se))

boot_metrics <- boot_metrics %>% 
  left_join(backward_summ %>% dplyr::select(coef, est, se),by = c("predictors" = "coef")) %>% 
  rename(selected_est = est, selected_se = se)

# specify number of bootstrap resamples and initiate matrices to store results
bootnum <- 10
boot_est <- boot_se <- matrix(0, ncol = length(full_model_summ %>% pull(coef)), nrow = bootnum,
                              dimnames = list(NULL, full_model_summ %>% pull(coef)))

# repeatedly take bootstrap resamples
# for each resample, peform variable selection via backward elimination
# save chosen variables and their estimates/SE, for variables not chosen set to 0
set.seed(1)
for(i in 1:bootnum){
  # running counter
  print(i)
  df <- cohort_new[sample(1:nrow(cohort_new),replace= T),]
  full_model <- glm(form_full,data=df,family='binomial',weight=ps_weight)
  boot_mod <- stepAIC(object=full_model,scope=list(lower=lower_model),direction='backward',trace=0)
  boot_est[i, names(coef(boot_mod))] <- coef(boot_mod)
  boot_se[i, names(coef(boot_mod))] <- coef(summary(boot_mod))[, "Std. Error"]
}
boot <- list(est=boot_est,se=boot_se)
save(boot,file='boot_result.RData')

# calculated bootstrap metrics as in Heinze et al. (2018) Table 5 
boot_metrics <- boot_metrics %>% 
  mutate(boot_inclusion = apply((boot_est != 0)*1, 2, function(x) sum(x)/length(x) * 100),
         rmsdratio = apply(((t(boot_est) - global_est)^2), 1, function(x) sqrt(mean(x)))/global_se,
         boot_meanratio = apply(boot_est, 2, mean)/global_est,
         boot_rel_bias = (boot_meanratio / (boot_inclusion / 100) - 1)*100,
         boot_median = apply(boot_est, 2, median),
         boot_025_quan = apply(boot_est, 2, function(x) quantile(x, 0.025)),
         boot_975_quan = apply(boot_est, 2, function(x) quantile(x, 0.975)))

# model frequency metrics as in Heinze et al. (2018) Table 6
boot_01 <- cbind(((boot_est != 0)*1)[, 
                                     boot_metrics$predictors[order(boot_metrics$boot_inclusion, decreasing = T)]], 
                 count = rep(1, times = bootnum))

model_freq = tibble(aggregate(count ~., data = boot_01, sum))
model_freq <- model_freq %>% 
  mutate(percent = count / bootnum *100) %>% 
  arrange(desc(percent)) %>% 
  mutate(cum_percent = cumsum(percent))

# create column denoting which variables not included in model
model_freq[,"vars_elim"] <- apply(model_freq[,c(2:(ncol(model_freq) -3))], 1, 
                                  function(x) paste(names(x[x==0]), collapse= " & "))

# select cols, create column denoting which is the final selected model
if(length(elim_vars) == 0){
  model_freq <- model_freq %>% 
    dplyr::select(vars_elim, count, percent, cum_percent) %>% 
    mutate(selected_model = as.numeric(vars_elim == ""))
} else{
  model_freq <- model_freq %>% 
    dplyr::select(vars_elim, count, percent, cum_percent) %>% 
    mutate(selected_model = as.numeric(setequal(sort(trimws(unlist(str_split(vars_elim, "&")))), sort(elim_vars))))
}

write.xlsx(boot_metrics,'final_model.xlsx',
           sheet='Bootstrap metrics as in Heinze et al. (2018) Table 5 ', append = T)
write.xlsx(model_freq,'final_model.xlsx', 
           sheet='Model frequency metrics as in Heinze et al. (2018) Table 6 ', append = T)

