## ------------------ Cluster functions ----------------

#---------- list of available variables to use ---------

listVariables <- function(data){
  data_num <- as.data.frame(apply(data, 2, as.numeric))
  data_num <- data_num[,-1]
  data_num <- data_num[colSums(!is.na(data_num)) > 0]
  return(colnames(data_num))
}

#---------- Prepare, the initial data input ----------

preprocessed_data <- function(data){

  data_pro <- data %>% select(all_of(variables_to_include)) %>% lapply(as.numeric) %>%
  as.data.frame() 

}

#---------- Prepare, code, and clean the data ----------

prepare.data <- function(data){
    
  data.numeric <- select_if(data.fixed,is.numeric)
  data.character <- select_if(data.fixed,is.character)
  data.character.single.fixed <- data.character
  
  # Numerical data
  
  if (ncol(data.numeric)>0) {
    
    data.numeric[is.na(data.numeric)] <- -sample(1000:10000,1) 
    data.numeric <- data.numeric[,colSums(data.numeric) != 0]
    data.numeric <- data.frame(scale(data.numeric)) 
  }
  
  data.fixed <- data.numeric[colSums(!is.na(data.numeric)) > 0]
  return(data.fixed)
}

#---------- Reduce Dimensionality (PCA) ----------

pca <- function(data.fixed){
  
  pca <- prcomp(data.fixed,center = TRUE)
  componetns <- pca$sdev[pca$sdev>=1]
  data.fixed.pca <- pca$x[,1:length(componetns)]
  return(data.fixed.pca)
}

#---------- Determine Optimal number of cluster ----------

c.number <- function(data.clean,method){  
  
  res.clust <- NbClust(data.clean,method = method,index = 'all')
  nc <- res.clust$Best.nc[1,]
  nc <- nc[nc>=2]
  val <- unique(nc)
  optimal <- val[which.max(tabulate(match(nc, val)))]
  sol <- c(min(nc),max(nc),optimal)
  
  return(sol)
}

mode <- function(x) {
  mode <- as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
  return(mode)
}
