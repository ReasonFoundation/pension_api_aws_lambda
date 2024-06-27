# rm(list = ls())


#Loading required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(zoo)
# library(profvis)
library(data.table)
library(openxlsx)
library(janitor)
library(rio)
library(Rcpp)


source("/var/task/model/utility_functions.R")
source("/var/task/model/MS PERS model inputs.R")
sourceCpp("/var/task/model/future_value_cpp.cpp")

source("/var/task/model/MS PERS data processing.R")
source("/var/task/model/MS PERS benefit model.R")
source("/var/task/model/MS PERS workforce model.R")
source("/var/task/model/MS PERS liability model.R")
source("/var/task/model/MS PERS funding model.R")

# source("./utility_functions/utility_functions.R")
# source("./core_models/MS PERS model inputs.R")
# 
# sourceCpp("./utility_functions/future_value_cpp.cpp")
# 
# source("./core_models/MS PERS data processing.R")
# source("./core_models/MS PERS benefit model.R")
# source("./core_models/MS PERS workforce model.R")
# source("./core_models/MS PERS liability model.R")
# source("./core_models/MS PERS funding model.R")

master_function <- function(
    curr_discount_rate = curr_discount_rate_,
    new_discount_rate = new_discount_rate_,
    cola_for_tier_5 = cola_for_tier_5_,
    tier5_vesting_period =  8,
    retire_refund_ratio = retire_refund_ratio_,
    db_new_ratio = db_new_ratio_,
    er_dc_contr_rate = er_dc_contr_rate_,
    funding_policy = funding_policy_,
    analysis_type = analysis_type_,
    roa_scenario = roa_scenario_,
    stat_new_hire_EE_contr = stat_new_hire_EE_contr_,
    is_fixed_ER_contr_rate = is_fixed_ER_contr_rate_,
    fixed_ER_contr_rate_option = fixed_ER_contr_rate_option_,
    amo_period_curr_hire_new_layer = amo_period_curr_hire_new_layer_,
    amo_period_new_hire = amo_period_new_hire_, 
    amo_method_curr_hire_curr_layer = amo_method_curr_hire_curr_layer_,
    amo_method_curr_hire_new_layer = amo_method_curr_hire_new_layer_,
    amo_method_new_hire = amo_method_new_hire_,
    ee_amo_cost_share_ratio = ee_amo_cost_share_ratio_,
    one_time_er_sup = one_time_er_sup_,
    er_sup_start_year = er_sup_start_year_
) {
  
  tier5_vesting_period_ <<- tier5_vesting_period
  
  # liability_output <- get_liability_data(
  #   curr_dist_rate = curr_discount_rate,
  #   new_dist_rate = new_discount_rate,
  #   cola_for_tier_5 = cola_for_tier_5,
  #   retire_refund_ratio = retire_refund_ratio,
  #   stat_new_hire_EE_contr = stat_new_hire_EE_contr,
  #   db_new_ratio = db_new_ratio
  # )
  
  funding_output <- get_funding_data(
    curr_dist_rate = curr_discount_rate,
    new_dist_rate = new_discount_rate,
    cola_for_tier_5 = cola_for_tier_5,
    retire_refund_ratio = retire_refund_ratio,
    funding_policy = funding_policy,
    analysis_type = analysis_type,
    roa_scenario = roa_scenario,
    stat_new_hire_EE_contr = stat_new_hire_EE_contr,
    is_fixed_ER_contr_rate = is_fixed_ER_contr_rate,
    fixed_ER_contr_rate_option = fixed_ER_contr_rate_option,
    db_new_ratio = db_new_ratio,
    er_dc_contr_rate = er_dc_contr_rate,
    amo_period_curr_hire_new_layer = amo_period_curr_hire_new_layer,
    amo_period_new_hire = amo_period_new_hire,
    amo_method_curr_hire_curr_layer = amo_method_curr_hire_curr_layer,
    amo_method_curr_hire_new_layer = amo_method_curr_hire_new_layer,
    amo_method_new_hire = amo_method_new_hire,
    ee_amo_cost_share_ratio = ee_amo_cost_share_ratio,
    one_time_er_sup = one_time_er_sup,
    er_sup_start_year = er_sup_start_year
  )
  
  target_col <- c("Year", "roa_mva", "total_hire_payroll", "total_hire_aal", "total_hire_ava", "total_hire_actual_mva", "funded_ratio_mva", "total_hire_ual_mva_real",
                  "total_hire_ER_contr_rate", "total_hire_ER_contr_real",
                  "cum_total_hire_ER_contr_real", "total_hire_ER_all_in_cost_real", "one_time_er_supplement"
                  # "curr_hire_ER_contr_rate", "curr_hire_ER_nc_rate", "new_hire_ER_contr_rate", "new_hire_ER_nc_rate",
                  # "new_hire_ER_amo_rate", "new_hire_EE_amo_rate"
  )
  
  target_output <- funding_output[,target_col]
  
  return(target_output)
}
