#### OPTIONS ####

options(scipen=999)
set.seed(12345)

#### LIBRARIES ####
library(dplyr)
library(magrittr)
library(ggplot2)
library(forcats)
library(lubridate)
library(RQuantLib)
## add other libraries

#### DIRECTORIES ####
working_dir = "/Users/Home/Documents/Documenti Giovanni/UNIMIB/Course Web Marketing & Communication Management/02_scripts"
data_dir = "/Users/Home/Documents/Documenti Giovanni/UNIMIB/Course Web Marketing & Communication Management/01_datasets"

setwd(working_dir)

#### EXECUTION FULL PIPELINE ####

# PIPELINE_scripts <- c(
#   'B01_ingestion.R'
#   , 'C01_preparation_df1.R'
#   , 'C02_preparation_df2.R'
#   , 'C03_preparation_df3.R'
#   , 'C04_preparation_df4.R'
#   , 'C05_preparation_df5.R'
#   , 'C06_preparation_df6.R'
#   , 'C07_preparation_df7.R'
#   ## add other scripts
#   )

# for(i in PIPELINE_scripts){
#   source(i, echo = TRUE)
# }