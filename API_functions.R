listExperiments <- function(count = 20, TOKEN = CONJOINTLY_TOKEN) {
  library(httr)
  library(jsonlite)
  library(data.table)
  
  count <- as.integer(count)
  
  headers <- add_headers(
    `Authorization` = paste("Bearer", TOKEN),
    `Content-Type` = "application/json",
    `Accept` = "application/json",
    `X-Requested-With` = "XMLHttpRequest"
  )
  
  result <- GET(
    paste0("https://api.conjoint.ly/api/experiments?paginate=", count),
    headers
  ) |> content("parsed", encoding = "UTF-8")
  
  experiments <- do.call(rbind, result$data) |> data.table()
  for (i in colnames(experiments)) {
    experiments[[i]][sapply(experiments[[i]], is.null)] <- NA
    experiments[[i]] <- unlist(experiments[[i]])
  }
  experiments
}

getRespondentsJSON <- function(experiment_id, mode = c('simplified', 'full'), TOKEN = CONJOINTLY_TOKEN) {
  
  mode <- match.arg(mode)
  
  headers <- add_headers(
    `Authorization` = paste("Bearer", TOKEN),
    `Content-Type` = "application/json",
    `Accept` = "application/json",
    `X-Requested-With` = "XMLHttpRequest"
  )
  
  # Request export of data as JSON:
  exportRequest <- POST(
    paste0("https://api.conjoint.ly/api/report/", experiment_id, "/analysis/respondents/job"),
    headers,
    body = toJSON(list(
      "command" = "respondents",
      "mode" = mode
    ), auto_unbox = T)
  ) |> content("parsed", encoding = "UTF-8")
  
  # Make sure the export is completed:
  repeat {
    if (exportRequest$data$status %in% c('completed', 'done')) {
      break;
    }
    Sys.sleep(2)
    exportRequest <- GET(
      paste0("https://api.conjoint.ly/api/jobs/", exportRequest$data$id, "/get"),
      headers
    ) |> content("parsed", encoding = "UTF-8")
  }
  
  # Find the location of the file on AWS:
  intermediateFilename <- exportRequest$data$response$result$filename
  awsFileLocationRequest <- GET(
    paste0("https://api.conjoint.ly/api/report/", experiment_id, "/respondents?filename=", intermediateFilename),
    headers
  ) |> content("text", encoding = "UTF-8")
  
  # Download the file from AWS:
  dataRequest <- GET(awsFileLocationRequest) |> 
    content("text", encoding = "UTF-8") |>
    fromJSON()
  
  return(dataRequest)
}

JSONlist2DataTable <- function(JSONlist) {
  if (length(JSONlist$output)) {
    result <- do.call(cbind, JSONlist$output) |> data.table()
    colnames(result) <- unlist(JSONlist$columns)
    for (i in colnames(result)) {
      result[[i]][sapply(result[[i]], is.null)] <- NA
      result[[i]] <- unlist(result[[i]])
    }
  } else {
    result <- matrix(
      nrow = 0,
      ncol = length(result$columns),
      dimnames = list(NULL, unlist(result$columns))
    ) |> data.table()
  }
  result
}