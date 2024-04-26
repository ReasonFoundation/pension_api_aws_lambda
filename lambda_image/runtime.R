library(httr)
library(logger)
log_formatter(formatter_paste)
log_threshold(INFO)

#' Convert a list to a single character, preserving names
#' prettify_list(list("a" = 1, "b" = 2, "c" = 3))
#' # "a=5, b=5, c=5"
prettify_list <- function(x) {
  paste(
    paste(names(x), x, sep = "="),
    collapse = ", "
  )
}

log_debug("Deriving lambda runtime API endpoints from environment variables")
lambda_runtime_api <- Sys.getenv("AWS_LAMBDA_RUNTIME_API")
log_info("Lambda Runtime API:", lambda_runtime_api)

if (Sys.getenv("AWS_LAMBDA_RUNTIME_API") == "") {
  error_message <- "AWS_LAMBDA_RUNTIME_API environment variable undefined"
  log_error(error_message)
  stop(error_message)
}
# next_invocation_endpoint <- paste0(
#   "http://", Sys.getenv("AWS_LAMBDA_RUNTIME_API"), "/2018-06-01/runtime/invocation/next"
# )
# log_info("Next invocation endpoint:", next_invocation_endpoint)

initialisation_error_endpoint <- paste0(
  "http://", Sys.getenv("AWS_LAMBDA_RUNTIME_API"), "/2018-06-01/runtime/init/error"
)

tryCatch(
{
  log_debug("Determining handler from environment variables")
  handler <- Sys.getenv("_HANDLER")
  if (is.null(handler) || handler == "") {
    stop("_HANDLER environment variable undefined")
  }
  log_info("Handler found:", handler)
  handler_split <- strsplit(handler, ".", fixed = TRUE)[[1]]
  log_info("Handler split:", handler_split)
  file_name <- paste0(handler_split[1], ".R")
  log_info("File name:", file_name)
  function_name <- handler_split[2]
  log_info("Using function", function_name, "from", file_name)

#   log_info("Directory:", getwd())
#   log_info("All files in directory:", list.files())

#   log_debug("Checking if", file_name, "exists")
#   log_info("Checking if", file_name, "exists", file.exists(file_name, envir = .GlobalEnv))
  if (!file.exists(file_name)) {
    stop(file_name, " doesn't exist in ", getwd())
  }

  log_info("Attempting to source", file_name)
  source(file_name)
  log_info("Sourced", file_name)

#   log_info("Is function", function_name, "defined?", exists(function_name))
#   log_info("Is function", function_name, "a function?", is.function(eval(parse(text = function_name))))

#   log_info("Check existence of function", function_name, "in environment")
#   if(!exists(function_name, where = .GlobalEnv)) {
#     stop("Function name ", function_name, " isn't defined in the global environment")
#   }


  log_debug("Checking if", function_name, "is defined")
  if (!exists(function_name)) {
    stop("Function name ", function_name, " isn't defined in R")
  }
  log_debug("Checking if", function_name, "is a function")
  if (!is.function(eval(parse(text = function_name)))) {
    stop("Function name ", function_name, " is not a function")
  }
},
  error = function(e) {
    log_error(as.character(e))
    POST(
      url = initialisation_error_endpoint,
      body = list(error_message = as.character(e)),
      encode = "json"
    )
    stop(e)
  }
)

handle_event <- function(event) {
  status_code <- status_code(event)
  log_debug("Status code:", status_code)
  if (status_code != 200) {
    stop("Didn't get status code 200. Status code: ", status_code)
  }
  event_headers <- headers(event)

  # HTTP headers are case-insensitive
  names(event_headers) <- tolower(names(event_headers))
  log_debug("Event headers:", prettify_list(event_headers))

  aws_request_id <- event_headers[["lambda-runtime-aws-request-id"]]
  if (is.null(aws_request_id)) {
    stop("Could not find lambda-runtime-aws-request-id header in event")
  }

  # According to the AWS guide, the below is used by "X-Ray SDK"
  runtime_trace_id <- event_headers[["lambda-runtime-trace-id"]]
  if (!is.null(runtime_trace_id)) {
    Sys.setenv("_X_AMZN_TRACE_ID" = runtime_trace_id)
  }

  unparsed_content <- httr::content(event, "text", encoding = "UTF-8")
  log_debug("Unparsed content:", unparsed_content)
  event_content <- if (unparsed_content == "") {
    list() # If there's no body, then there are no function arguments
  } else {
    jsonlite::fromJSON(unparsed_content)
  }

  result <- do.call(function_name, event_content)
  log_debug("Result:", as.character(result))
  response_endpoint <- paste0(
    "http://", Sys.getenv("AWS_LAMBDA_RUNTIME_API"), "/2018-06-01/runtime/invocation/",
    aws_request_id, "/response"
  )
  POST(
    url = response_endpoint,
    body = result,
    encode = "json"
  )
  rm("aws_request_id") # so we don't report errors to an outdated endpoint
}

log_info("Querying for events")
while (TRUE) {
  tryCatch(
  {
    event <- GET(url = paste0(
  "http://", Sys.getenv("AWS_LAMBDA_RUNTIME_API"), "/2018-06-01/runtime/invocation/next"
))
    log_debug("Event received")
    log_info("About to call handle_event")
    handle_event(event)
    log_info("handle_event called successfully")
  },
    error = function(e) {
      log_error(as.character(e))
      if (exists("aws_request_id")) {
        invocation_error_endpoint <- paste0(
          "http://", Sys.getenv("AWS_LAMBDA_RUNTIME_API"), "/2018-06-01/runtime/invocation/",
          aws_request_id, "/error"
        )
        POST(
          url = invocation_error_endpoint,
          body = list(error_message = as.character(e)),
          encode = "json"
        )
      }
      stop(e)
    }
  )
}
