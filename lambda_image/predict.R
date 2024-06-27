source("/var/task/model/MS PERS master.R") # 2.6s
library(jsonlite)

# download model file from S3 into /tmp folder
# system("aws s3 cp ${S3_MODEL_URI} /tmp/model-folder/ --recursive")

handler <- function(body, ...) {

  data <- fromJSON(txt = body)
  # Load the model source file

  inp_curr_dist_rate <- data$curr_discount_rate
  inp_new_dist_rate <- data$new_discount_rate
  inp_cola_for_tier_5 <- data$cola_for_tier_5
  inp_tier5_vesting_period <- data$tier5_vesting_period
  inp_retire_refund_ratio <- 0.65
  inp_db_new_ratio <- data$db_new_ratio
  inp_er_dc_contr_rate <- data$er_dc_contr_rate
  inp_funding_policy <- data$funding_policy
  inp_analysis_type <- 'Deterministic'
  inp_roa_scenario <- data$roa_scenario
  inp_stat_new_hire_EE_contr <- data$stat_new_hire_EE_contr
  inp_is_fixed_ER_contr_rate <- data$is_fixed_ER_contr_rate
  inp_fixed_ER_contr_rate_option <- data$fixed_ER_contr_rate_option
  inp_amo_period_curr_hire_new_layer <- data$amo_period_curr_hire_new_layer
  inp_amo_period_new_hire <- data$amo_period_new_hire
  inp_amo_method_curr_hire_curr_layer <- data$amo_method_curr_hire_curr_layer
  inp_amo_method_curr_hire_new_layer <- data$amo_method_curr_hire_new_layer
  inp_amo_method_new_hire <- data$amo_method_new_hire
  inp_ee_amo_cost_share_ratio <- data$ee_amo_cost_share_ratio
  inp_one_time_er_sup <- 110
  inp_er_sup_start_year <- 2025

  # Predict with the loaded model using the variables with underscores
  predictions <- master_function(
    curr_discount_rate = as.numeric(inp_curr_dist_rate),
    new_discount_rate = as.numeric(inp_new_dist_rate),
    cola_for_tier_5 = as.numeric(inp_cola_for_tier_5),
    tier5_vesting_period = as.numeric(inp_tier5_vesting_period),
    retire_refund_ratio = as.numeric(inp_retire_refund_ratio),
    db_new_ratio = as.numeric(inp_db_new_ratio),
    er_dc_contr_rate = as.numeric(inp_er_dc_contr_rate),
    funding_policy = inp_funding_policy,
    analysis_type = inp_analysis_type,
    roa_scenario = inp_roa_scenario,
    stat_new_hire_EE_contr = as.numeric(inp_stat_new_hire_EE_contr),
    is_fixed_ER_contr_rate = as.logical(inp_is_fixed_ER_contr_rate),
    fixed_ER_contr_rate_option = inp_fixed_ER_contr_rate_option,
    amo_period_curr_hire_new_layer = as.numeric(inp_amo_period_curr_hire_new_layer),
    amo_period_new_hire = as.numeric(inp_amo_period_new_hire),
    amo_method_curr_hire_curr_layer = inp_amo_method_curr_hire_curr_layer,
    amo_method_curr_hire_new_layer = inp_amo_method_curr_hire_new_layer,
    amo_method_new_hire = inp_amo_method_new_hire,
    ee_amo_cost_share_ratio = as.numeric(inp_ee_amo_cost_share_ratio),
    one_time_er_sup = as.numeric(inp_one_time_er_sup),
    er_sup_start_year = as.numeric(inp_er_sup_start_year)
  )

  # Return response with predictions payload
  return(
    list(
      statusCode = 200,
      headers = list(
        "Content-Type" = "application/json",
        "Access-Control-Allow-Origin" = "*", 
        "Access-Control-Allow-Methods" = "OPTIONS,POST,GET",
        "Access-Control-Allow-Headers" = "Content-Type"
      ),
      body = toJSON(list(predictions = predictions))
    )
  )
}

