## ------------------ Cluster functions ----------------

#---------- Prepare, code, and clean the data ----------

prepare.data <- function(data){
  
  data.fixed <- data%>%select(-participant_id)  
  
  data.numeric <- select_if(data.fixed,is.numeric)
  data.character <- select_if(data.fixed,is.character)
  data.character.single.fixed <- data.character
  
  # Numerical data
  
  if (ncol(data.numeric)>0) {
    
    data.numeric[is.na(data.numeric)] <- -sample(1000:10000,1) 
    data.numeric <- data.numeric[,colSums(data.numeric) != 0]
    data.numeric <- data.frame(scale(data.numeric)) 
  }
  
  # Character data
  
  if (ncol(data.character)>0) {
    
    # Divide Multi and Single choice
    
    data.character.multi <- matrix(NA,nrow = nrow(data.character),ncol = ncol(data.character))
    data.character.single <- matrix(NA,nrow = nrow(data.character),ncol = ncol(data.character))
    
    for (i in 1:ncol(data.character)) {
      
      if (length(unlist(strsplit(data.character[,i], ",")))>nrow(data.character)) {
        
        data.character.multi[,i]<-data.character[,i]
      }
      
      if (length(unlist(strsplit(data.character[,i], ",")))<=nrow(data.character)) {
        
        data.character.single[,i]<-data.character[,i]
      }
      
    }
    
    data.character.multi <- as.matrix(data.character.multi[ , colSums(is.na(data.character.multi))==0])
    data.character.single <- as.matrix(data.character.single[ , colSums(is.na(data.character.single))==0])
    
    # code characters into dummies - multi
    
    if(ncol(data.character.multi)>0){
      
      for (index in 1:ncol(data.character.multi)) {
        
        data.fixed.multi <- data.character.multi[,index]
        All_options <- unique(unlist(strsplit(data.fixed.multi, ",")))
        aux_set<-data.frame(data.fixed.multi) %>% separate(data.fixed.multi, All_options)
        
        row <- nrow(aux_set)
        column <- length(All_options)
        
        coded.data <- matrix(NA,nrow = row,ncol =  column)
        
        for (i in 1:row) {
          
          for (j in 1:length(All_options)) {
            
            coded.data[i,j] <- All_options[j]%in%aux_set[i,]
            
          }
        }
        
        coded.data[coded.data==TRUE] <- 1
        colnames(coded.data) <- paste0('Option: ',All_options,'.',index)
        coded.data <- coded.data[,-1]
        
        data.numeric <- cbind(data.numeric,coded.data) 
        
      }
    }
    
    # code characters into dummies - single
    
    if(ncol(data.character.single)>0){
      
      data.character.single.fixed <- dummy_cols(data.character.single,remove_selected_columns = TRUE,remove_first_dummy = TRUE)
      
    }

  }
  
  data.fixed <- cbind(data.numeric,data.character.single.fixed)
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
