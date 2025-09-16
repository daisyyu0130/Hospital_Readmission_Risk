###########################################################
# Data read-in
# Author: Daisy Yu
# Date: 2022-08-25
# Updated: 2022-09-15
##########################################################

# libraries
library("tidyverse")
library("readxl")

# directory where code is read
code_for_reading_dir <- "R:/working/G78717-Cohort/code"

# set current directory
setwd("R:/working/G78717-Cohort/data/raw data/")

##########################################################
# Read census-geodata 
##########################################################
code_for_reading_file <- "StaplesLab - read-census-geodata.R"
source(paste0(code_for_reading_dir, code_for_reading_file))


##########################################################
# Read DAD-admits
##########################################################
code_for_reading_file <- "StaplesLab - read-DAD-admits.R"
source(paste0(code_for_reading_dir, code_for_reading_file))


##########################################################
# Read death
##########################################################
code_for_reading_file <- "StaplesLab - read-deaths.R"
source(paste0(code_for_reading_dir, code_for_reading_file))


##########################################################
# Read demographics
##########################################################
code_for_reading_file <- "StaplesLab - read-demographics.R"
source(paste0(code_for_reading_dir, code_for_reading_file))


##########################################################
# Read MSP
##########################################################
code_for_reading_file <- "StaplesLab - read-msp.R"
source(paste0(code_for_reading_dir, code_for_reading_file))


##########################################################
# Read MSP - practitioner
##########################################################
code_for_reading_file <- "StaplesLab - read-msp-pract.R"
source(paste0(code_for_reading_dir, code_for_reading_file))


##########################################################
# Read registry
##########################################################
code_for_reading_file <- "StaplesLab - read-registry.R"
source(paste0(code_for_reading_dir, code_for_reading_file))



