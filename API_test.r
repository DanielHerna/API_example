#--------------- Load the following libraries ---------------#

library('dplyr') 
library('NbClust') 
library('mclust') 
library('diceR') 
library('fastDummies') 
library('httr')
library('jsonlite')
library('data.table')

#--------------- Load the following custom functions ---------------#
# Please do not change the functions contained in functions.r 
# set your directory the same as functions.r and this script (functions.r must be saved in the same path)

setwd('Your_directory_here')
#setwd('G:/Shared drives/Development (no client data)/On going projects/API')
source('functions.r')

#--------------- Connect with the API ---------------#


CONJOINTLY_TOKEN <- "YOUR_TOKEN" # Get a token from https://run.conjoint.ly/utilities/tokens

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

experiment_id <- Exp_ID # Include the ID of your experiment here  

data <- getRespondentsJSON(experiment_id) |> JSONlist2DataTable()

## ------------------ Clean and select data ----------------

variables_to_include <- c("participant_id",
  "Q2: _ Survey question",
  "Q3: _ Selection",
  "Q4: _ List selection",
  "Q5: _ List selection",
  "Q12: PRODUCT LIKERT",
  "Q14: How likely are you to recommend this company to your friend or colleag"
) # Specify the column names of the variables you intend to incorporate into the cluster analysis.

data <- data %>% select(all_of(variables_to_include)) %>% lapply(as.numeric) %>%
  as.data.frame() 

## ------------------ compute clusters ----------------

id <- data$participant_id # save respondents ID's
data.fixed <- prepare.data(data) # standardize data structure 
data.fixed.pca <- pca(data.fixed) # Perform a PCA analysis on the original dataset

#----- k means solution with consensus clustering 

c.kmeans<-c.number(data.fixed.pca,'kmeans') # suggested number of clusters: the first value in the lower range, 2nd is the upper and the 3rd is the optimal number of cluster

CC.kmeans <- consensus_cluster(data.fixed.pca, nk = c.kmeans[3]:c.kmeans[3], p.item = 0.8,reps = 15,algorithms = 'km') # compute Kmeans using transformed PCA data

# To change the number of clusters (k) please change the nk parameter
# To change the proportion of resample in the consensus algorithm, please change p
# To change number of repetitions consensus algorithm, please change reps
# DONT change algorithms = 'km'

CC.kmeans <- data.frame(apply(CC.kmeans, 2:4, impute_knn, data = data.fixed.pca, seed = 1)) # Impute the K-Nearest Neighbours to the consensus solution
CC.kmeans <- apply(CC.kmeans,1,mode) #replace NAs

#-----  hierarchical solution with consensus clustering 

c.hc<-c.number(data.fixed.pca,'complete') # suggested number of clusters: the first value in the lower range, 2nd is the upper and the 3rd is the optimal number of cluster

CC.hc <- consensus_cluster(data.fixed.pca, nk = c.hc[3]:c.hc[3],p.item = 0.8,reps = 15,algorithms = 'hc')  # compute Kmeans using transformed PCA data

# To change the number of clusters (k) please change the nk parameter
# To change the proportion of resample in the consensus algorithm, please change p
# To change number of repetitions consensus algorithm, please change reps
# DONT change algorithms = 'hc'

CC.hc <- data.frame(apply(CC.hc, 2:4, impute_knn, data = data.fixed.pca, seed = 1)) # Impute the K-Nearest Neighbours to the consensus solution
CC.hc <- apply(CC.hc,1,mode) #replace NAs

#----- GMM solution 

mb = Mclust(data.fixed.pca) # Run Mixture-Gaussian clusters

mb$G # optimal number of cluster, for reference. No need to include it in Mclust
mb$classification # Cluster solution

# Export all the cluster solutions:

export <- data.frame(participant_id = id, KM = CC.kmeans, HC =  CC.hc, GMM = mb$classification)
write.csv(export, 'cluster_solutions.csv', row.names=FALSE) # export a CSV file with the all the clusters membership
