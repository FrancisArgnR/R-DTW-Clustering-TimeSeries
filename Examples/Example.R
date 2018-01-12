
################################### Generate time series

# Inicialization
timeSeries <- list()

# Generate 20 random time series with random length
for(i in 1:20){
  timeSeries[[i]] <- rep(0,as.integer(runif(1,90,110)))
}
# Introduce some noise (Some values to 2, 1 or 0.5)
for(i in 1:20){
  index <- as.integer(runif(1,0,length(timeSeries[[i]])-10)) # Index to introduce the noise
  if(index<( as.integer(length(timeSeries[[i]])/3) )){ # Value of the noise
    value <- 0.5 # Value of the noise
    for(j in index:(index+4-1)){ # 4 values of noise
      timeSeries[[i]][j] <- value
    }
  }else if(index<( as.integer(length(timeSeries[[i]])/3)*2 )) {
    value <- 1 # Value of the noise
    for(j in index:(index+8-1)){ # 8 values of noise
      timeSeries[[i]][j] <- value
    }
  }else{
    value <- 2 # Value of the noise
    for(j in index:(index+12-1)){ # 12 values of noise
      timeSeries[[i]][j] <- value
    }
  }
  #for(j in index:(index+10-1)){
  #  timeSeries[[i]][j] <- value
  #}
}

# Plot the time series
par(mfrow=c(2,10)) 
for(i in 1:20){
  plot(1:(length(timeSeries[[i]])),timeSeries[[i]],type="l",ylim=c(0,2))
}
par(mfrow=c(1,1)) 



################################### DTW+Clustering with package stats

# Compute DTW distances
distMatrix <- dist(timeSeries, method="DTW")

# Hierarchical clustering
hc <- hclust(distMatrix, method="average")
plot(hc, main="")



################################### DTW+Clustering with package TSclust & stats

library(TSclust)

# Compute DTW distances
distMatrix <- diss(timeSeries, METHOD="DTWARP", p=0.05)

# Hierarchical clustering
hc <- hclust(distMatrix)
plot(hc, main="")

# If we want to derive a certain number of clusters from the dendogram (the plot)(the number of cluster should be 3)
rect.hclust(hc, k = 3)
# The cluster of every time serie
cutree(hc, k = 3)

################################### DTW+Clustering with package dtwclust

library(dtwclust)

# Partitional clustering with DTW
pc <- tsclust(timeSeries, type = "partitional", k=3, distance = "dtw_basic", centroid = "pam",  trace = TRUE)
plot(pc)

# Hierarchical clustering wit DTW
pc <- tsclust(timeSeries, type = "hierarchical", k=3, distance = "dtw_basic", centroid = "pam",  trace = TRUE)
plot(pc)
# If we want to derive a certain number of clusters from the dendogram (the plot)(the number of cluster should be 3)
rect.hclust(pc, k = 3)
# The cluster of every time serie
cutree(hc, k = 3)
