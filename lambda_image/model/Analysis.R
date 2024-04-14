
# source("../../tmp/model-folder/MasterModel.R")

# base_benefit_data <- get_benefit_data()
# base_liability_data <- get_liability_data()
# base_funding_data <- get_funding_data()

# r <- 0.5
# cf <- rep(9, 10^9)
# log_rcpp_out <- get_npv_rcpp_log(r, cf)
# rcpp_out <- get_npv_rcpp(r, cf)
# print(c(log_rcpp_out, rcpp_out))

# s_fund <- Sys.time()
# det_SQ_assumption <-  get_funding_data(
#   curr_dist_rate = 0.0755,
#   new_dist_rate = 0.0755,
#   cola = 0.03,
#   retire_refund_ratio = 0.6,
#   funding_policy = "StatusQuo",
#   analysis_type = "Deterministic",
#   roa_scenario = "Assumption"
# )
# e_fund <- Sys.time()

# s_fund_ <- Sys.time()
# det_SQ_recur_recession <- get_funding_data(
#   curr_disc_rate = 0.07,
#   new_disc_rate = 0.0755,
#   cola = 0.03,
#   retire_refund_ratio = 0.6,
#   funding_policy = "StatusQuo",
#   analysis_type = "Deterministic",
#   roa_scenario = "RecurringRecession"
# )
# e_fund_ <- Sys.time()
# 
# det_ADC_assumption <-  get_funding_data(
#   curr_disc_rate = 0.0755,
#   new_disc_rate = 0.0755,
#   cola = 0.03,
#   retire_refund_ratio = 0.6,
#   funding_policy = "ADC",
#   analysis_type = "Deterministic",
#   roa_scenario = "Assumption"
# )
# 
# det_ADC_recur_recession <-  memoised_get_funding_data(
#   curr_disc_rate = 0.0755,
#   new_disc_rate = 0.0755,
#   cola = 0.03,
#   retire_refund_ratio = 0.6,
#   funding_policy = "ADC",
#   analysis_type = "Deterministic",
#   roa_scenario = "Recurring Recession"
# )
# 
# 
# 
# file_path = "/Users/anhtu/Documents/Reason Org/Actuarial Modeling Projects/Mississippi PERS/MPERS-Optimized-Models-Steve-Github/Outputs/"
# write.xlsx(det_SQ_assumption, file=paste0(file_path, "det_SQ_assumption.xlsx"))
# write.xlsx(det_SQ_recur_recession, file=paste0(file_path, "det_SQ_recur_recession.xlsx"))
# write.xlsx(det_ADC_assumption, file=paste0(file_path, "det_ADC_assumption.xlsx"))
# write.xlsx(det_ADC_recur_recession, file=paste0(file_path, "det_ADC_recur_recession.xlsx"))















