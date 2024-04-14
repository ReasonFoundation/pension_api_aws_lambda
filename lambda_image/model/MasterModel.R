
rm(list = ls())

# s_time <- Sys.time()

library("readxl")
library(dplyr)
library(tidyr)
library(zoo)
# library(ggplot2)
library(data.table)
library(openxlsx)
library(Rcpp)
# library(memoise)


source("/var/task/model//UtilityFunctions.R")

source("/var/task/model/Inputs.R")

sourceCpp("/var/task/model/future_value_cpp.cpp")

source("/var/task/model/BenefitModel.R")

source("/var/task/model/WorkforceModel.R")
# get_wf_data()


wf_projection <- readRDS("/var/task/model/wf_data.rds")
source("/var/task/model/LiabilityModel.R")

source("/var/task/model/FundingModel.R")


# source("UtilityFunctions.R")
# source("Inputs.R")
# sourceCpp("future_value_cpp.cpp")
# source("BenefitModel.R")
# source("WorkforceModel.R")
# wf_projection <- readRDS("wf_data.rds")
# source("LiabilityModel.R")
# source("FundingModel.R")
# 
# funding <- get_funding_data()
# 
# e_time <- Sys.time()