
#Jonathan Conrow
#DSSA 5201-091
#Professor Baldwin
#Feb. 6, 2018

# The Significance of K-Means Clustering
#The K means algorithim is a popular yet simple algorithm they helps #to identify underlying structures in a data set. The purpose of it #is to split the data into groups based on the means of each group #which indicates a "summary" of each observation cluster.

#import some libraries I think I may use
library(plyr)
library(tidyverse)
library(ggplot2)

#read in the K Means Data provided to us by baldwin and name the columns
K_data <- read.csv("KMeansData_Conrow.csv", header = FALSE)
#names(K_data) <- c("X", "Y")

#plot the data to get a visual idea of how many clusters may exist
plot( K_data)
# it appears that there may be 7 groupings

#Look at the the data we are working with, there are 2 variables od 873 observations
# I need to split the two variables up into seperate dataframes, variable 1 will be the Training data and variable 2 will be the testing data.

#rm(train.df,test.df)
#train.df <- K_data.df[1]
#test.df <- K_data.df[2]



# Using R Studio's K means function First
# The naitive rstudio K-means clustering with 7 clusters
K_data.7means <- kmeans(K_data, centers = 7)

# To show the centers of each clusters
K_data.7means$centers

# To show the clusters each point was assigned to
K_data.7means$cluster

# To plot the groupings in color
plot( K_data[K_data.7means$cluster == 1, ],
      col = "red",
      xlim=c( min( K_data[]), max( K_data[])),
      ylim=c( min( K_data[]), max( K_data[]))
      )
points( K_data[K_data.7means$cluster == 2, ],
      col = "orange")
points( K_data[K_data.7means$cluster == 3, ],
      col = "yellow")
points( K_data[K_data.7means$cluster == 4, ],
      col = "green")
points( K_data[K_data.7means$cluster == 5, ],
      col = "blue")
points( K_data[K_data.7means$cluster == 6, ],
      col = "black")
points( K_data[K_data.7means$cluster == 7, ],
      col = "purple")


# Now that we have an idea of what the KMeans CLustering in R studio does we can create our own function and make sure it works similarly to the the actual algorithm
## First we must format the centers
#Note: Used enhancedatascience.com for reference

# Since it seems that there should b 5 distinct clusters in my data I will make
#K = 5

#centers initialisation
#centroids=K_data[sample.int(nrow(K_data),K),]

#It is important that a time limit is set to stop the formatting or else a loop will continue
#current_stop_scrit = 10e10

#Must save the vectors where the the centers of each point will be using replicate
#cluster = rep(0,nrow(K_data))

#converged = F

# In order to assign these particular points we must loop through the data
for (i in 1:nrow(K_data))
{
  min_dist = 10e10 
#although this distance is much larger than we need, it will allow    for the loop to iterate through the data appropriately
  # it is important to iterate over the centroids (although they will be adjusted later) created above based on 5 clusters
  for (centroid in 1:nrow(centroids))
  {
    #to calculate the euclidean distance the equation can be denoted like so:
    eucli_dist = sum((centroids[centroid,]-K_data[i,])^2)
    # now set the center of each cluster based on the smallest euclidean distances created in the above line
    if (eucli_dist <= min_dist)
    {
      #these points are now set to the clusters
      cluster[i]=centroid
      min_dist=eucli_dist
    }
  }
}

# Now that we have a better understanding  
#We can put it all together into our very own K means clustering function 
#Making the function. 

#Since I think it should be 7 clusters based on how the original plot looked I will make the function based around this. Also by giving it a stop time it will allow the loop to stop eventually.
K=7
con_means=function(K_data,K,stop_crit = 10e-6)
{
  #initialisation of clusters, like explained above
  centers=K_data[sample.int(nrow(K_data),K),]
  current_stop_scrit=1000
  cluster=rep(0,nrow(K_data))
  # The converged is set to false for now because later in the loop when it becomes true it stops the loop of our function and the "itera" is the iteration through the data 
  converged=F
  itera=1
  while(current_stop_scrit>=stop_crit & converged==F)
  {
    itera=itera+1
    if (current_stop_scrit<=stop_crit)
    {
      # When convereged is turned to true it will stop the loop from continuing
      converged=T
    }
    # The old centers are the same as the centers but they have to be differnernt in the next portion of the loop so we make a new variable called "ol_centers"to keep this data
    ol_centers=centers
    #We must now assign each point to one of these centers
    for (i in 1:nrow(K_data))
    {
      # This minimum distance is much larger to allow us to get all of the points in the data and find the best possible cluster
      min_dist=10e10
      for(center in 1:nrow(centers))
      {
        # The Euclidean distance is the sum of the center coordinates minus the iteration of the data squared. This helps to find the least amount of distastance between the center of a cluster and all the data points around it, which then allows for the groupings to take place.
        eucli_dist=sum((centers[center,]-K_data[i,])^2)
        if(eucli_dist<=min_dist)
        {
          cluster[i]=center
          min_dist=eucli_dist
        }
      }
    }
    #next we iterate through the data once again to apply the groupings to each point to it can belong to a designated cluster by applying the mean Euclidean distance of each center. It is set to stop when the mean of the previous centers and new centers are subtracted and squared is reached 
    for( i in 1:nrow(centers))
    {centers[i,]=apply(K_data[cluster==i,],2,mean)}
    current_stop_scrit=mean((ol_centers-centers)^2)
  }
  # Lastly we return a list of the information made with this funtion, determining which data set goes in which cluster.
  return(list(K_data=data.frame(K_data,cluster),centers=centers))
}


# Using the Function
#Now that the function is complete, I will graph it and see what our clusters look like using ggplot

library(ggplot2)
res=con_means(K_data[1:2],K=7)
res$centers$cluster=1:7
res$K_data$iscenter=F
res$centers$iscenter=T
K_data_plot=rbind(res$centers,res$K_data)
ggplot(K_data_plot,aes(x=V1,y=V2,color=as.factor(cluster),size=iscenter,alpha=iscenter))+geom_point()


# Maybe 7 clusters is not the appropriate number of clusters to use I will try 6 instead
#The reason I dont think 7 is the correct number of clusters is because in the lower left of the graph it seems like it should be 1 cluster but there are two centers.


library(ggplot2)
res=con_means(K_data[1:2],K=6)
res$centers$cluster=1:6
res$K_data$iscenter=F
res$centers$iscenter=T
K_data_plot=rbind(res$centers,res$K_data)
ggplot(K_data_plot,aes(x=V1,y=V2,color=as.factor(cluster),size=iscenter,alpha=iscenter))+geom_point()

#It appears that this looks much better.

# K means clustering Elbow graph
#To find the optimized number of clusters to use for the data, a elbow graph of K Vs K Means function(con_means) will reveal which factor will yield the best results. I had difficulty with this portion, so I referenced a site that explained how to use the elbow method to find the best kvalue using the kmeans funtion


#Elbow method for finding the best number of clusters to use. To compute and plot the elbow method using "Within Sum Squares" for the clusters.

# The max values for K
kmax <- 10
# Since I may have use the "K-data" for other things, I wanted to have a new variable linked to it for the elbow method
datacon <- K_data
# Using sapply to return a matrix/vector of the data ranging from k=1 to k=10 and iterating through the datacon data
wss <- sapply(1:kmax,
              function(k){kmeans(datacon,k,nstart=1, iter.max=873) $tot.withinss})
wss
plot(1:kmax,wss,
     type = "b", pch = 19, frame=TRUE,
     xlab = "Number of Clusters",
     ylab = "Total Within Clusters Sum of Squares")



# Based on the results from the elbow method
#As one can see the graph has a less drastic decrease after 5 clusters which would indicate that 5 clusters would be our optimized number of clusters if the "between sums squared"/"total sums squared" has less of a change.

#-Lets look at what the cluster look like with 5 groups.

library(ggplot2)
res=con_means(K_data[1:2],K=5)
res$centers$cluster=1:5
res$K_data$iscenter=F
res$centers$iscenter=T
K_data_plot=rbind(res$centers,res$K_data)
ggplot(K_data_plot,aes(x=V1,y=V2,color=as.factor(cluster),size=iscenter,alpha=iscenter))+geom_point()


# Honestly, I think 6 clusters makes more sense than 5 based on the differences between the two graphs.

# References
#https://www.r-bloggers.com/finding-optimal-number-of-clusters/

#http://enhancedatascience.com/2017/10/24/machine-learning-explained-kmeans/


