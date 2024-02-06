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
# Or Set working directory to source file location.

setwd('G:/Shared drives/Development (no client data)/On going projects/API')
#setwd('G:/Shared drives/Development (no client data)/On going projects/API')
source('functions.r')   #For clustering
source('API_functions.r') #For API connection and requests

#--------------- Connect with the API ---------------#

CONJOINTLY_TOKEN <- "70|nQZzQDQPwKZFvUfVhixOGrCuVSR1E7ne5zghthAJ" # Paste your token, get a token from https://run.conjoint.ly/utilities/tokens


experimentList <- listExperiments(20)   # Get a list of your experiments
experiment_id <- experimentList$id[1]   # Get first experiment id from list

data <- getRespondentsJSON(experiment_id) |> JSONlist2DataTable()
id <- data$participant_id # save respondents ID's
available_variables <- listVariables(data)

## ------------------ Clean and select data ----------------

# Specify the column names of the variables you intend to incorporate into the cluster analysis.


variables_to_include <- c(available_variables) 
pro_data <- preprocessed_data(data)

## ------------------ compute clusters ----------------

data.fixed <- prepare.data(pro_data) # standardize data structure 
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
