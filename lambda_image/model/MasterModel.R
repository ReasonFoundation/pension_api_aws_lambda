
# rm(list = ls())

# s_time <- Sys.time()

library("readxl")
library(dplyr)
library(tidyr)
library(zoo)
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

# funding <- memoised_get_funding_data(
#   curr_dist_rate = 0.0755
# )

# funding <- get_funding_data(
#   curr_dist_rate = 0.0755
# )

# e_time <- Sys.time()

