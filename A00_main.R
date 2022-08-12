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
working_dir = "C:/Users/matte/Università/Web Marketing/DMktg_DSLab_R_Scripts"
data_dir = "C:/Users/matte/Downloads/DMktg_DSLab_data"

setwd(working_dir)

#### EXECUTION FULL PIPELINE ####
PIPELINE_scripts <- c('B01_ingestion.R', 'C01_preparation_df1.R', 'C02_preparation_df2.R', 'C03_preparation_df3.R'
                      , 'C04_preparation_df4.R', 'C05_preparation_df5.R', 'C06_preparation_df6.R', 'C07_preparation_df7.R'
                      , 'D00_churn.R', 'D01_RFM.R', 'D02_CLTV.R', 'D03_FINAL.R')

for(i in PIPELINE_scripts){
   source(i, echo = TRUE)
}
