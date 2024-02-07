listExperiments <- function(count = 20, TOKEN = CONJOINTLY_TOKEN) {
  
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

formulateElements <- function() {
  elements <- list(type = "excel", elements = list(
    list(type = "special-entity", payload = list(what = "respondent-info", mode = "simplified")),  # Replace "simplified" with "full" to get full mode
    list(type = "special-entity", payload = list(what = "conjoint-data")),  # This only works for conjoint experiments
    list(type = "special-entity", payload = list(what = "data-dumps"))
  ))
  return(elements)
}

downloadExport <- function(experiment_id, overwrite = FALSE, filename = "data.xlsx", elements = formulateElements(), TOKEN = CONJOINTLY_TOKEN) {

  headers <- add_headers(
    `Authorization` = paste("Bearer", TOKEN),
    `Content-Type` = "application/json",
    `Accept` = "application/json",
    `X-Requested-With` = "XMLHttpRequest"
  )

  cat('Requesting export of data...\n')
  exportRequest <- POST(
    paste0("https://api.conjoint.ly/api/experiments/", experiment_id, "/export/element"),
    headers,
    body = toJSON(elements, auto_unbox = T)
  ) |> content("parsed", encoding = "UTF-8")

  # Make sure the export is completed:
  repeat {
    if (exportRequest$data$status %in% c('completed', 'done')) {
      break;
    }
    Sys.sleep(2)

    cat('Checking if file is ready...\n')
    exportRequest <- GET(
      paste0("https://api.conjoint.ly/api/jobs/", exportRequest$data$id, "/get"),
      headers
    ) |> content("parsed", encoding = "UTF-8")
  }

  cat('Finding the location of the file on AWS...\n')
  intermediateFilename <- exportRequest$data$response$uri
  awsFileLocationRequest <- GET(
    intermediateFilename,
    headers
  )

  cat('Downloading the file from AWS...\n')
  GET(
    awsFileLocationRequest$url,
    write_disk(filename, overwrite = overwrite)
  )

  return(awsFileLocationRequest$url)
}