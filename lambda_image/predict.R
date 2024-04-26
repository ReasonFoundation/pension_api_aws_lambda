source("/var/task/model/MasterModel.R")
library(jsonlite)

# download model file from S3 into /tmp folder
# system("aws s3 cp ${S3_MODEL_URI} /tmp/model-folder/ --recursive")

handler <- function(body, ...) {

#   rm(list = ls())

  data <- fromJSON(txt = body)
  # Load the model source file

  inp_curr_dist_rate <- data$curr_dist_rate
  inp_new_dist_rate <- data$new_dist_rate
  inp_cola <- data$cola
  inp_retire_refund_ratio <- data$retire_refund_ratio
  inp_funding_policy <- data$funding_policy
  inp_analysis_type <- data$analysis_type
  inp_roa_scenario <- data$roa_scenario
  
  # Predict with the loaded model using the variables with underscores
  predictions <- get_funding_data(
    curr_dist_rate = as.numeric(inp_curr_dist_rate),
    new_dist_rate = as.numeric(inp_new_dist_rate),
    cola = as.numeric(inp_cola),
    retire_refund_ratio = as.numeric(inp_retire_refund_ratio),
    funding_policy = inp_funding_policy,
    analysis_type = inp_analysis_type,
    roa_scenario = inp_roa_scenario
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

