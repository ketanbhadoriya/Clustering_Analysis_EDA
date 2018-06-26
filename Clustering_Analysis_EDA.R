#Start of the Script

###Loading the Data and Required Packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(dendextend)
library(cluster)
library(purrr)

customers_spend <- readRDS("ws_customers.rds")
head(customers_spend)
tail(customers_spend)


###Using Hierarchical Clustering to Perform Market Segmentation

#Calculating euclidean distance between customers

dist_customers <- dist(customers_spend,method="euclidean")


#Generating a complete linkage analysis 
hc_customers <- hclust(dist_customers,method="complete")

#Plotting the dendrogram
plot(hc_customers)

#Create=ing a cluster assignment vector at h = 15000
clust_customers <- cutree(hc_customers,h=15000)

#Generating the segmented customers dataframe
segment_customers <- mutate(customers_spend, cluster = clust_customers)

###Exploring Wholesale Customers Clusters

#Counting the number of customers that fall into each cluster

clusters_size <- count(segment_customers, cluster)
clusters_size

#Colored dendrogram based on the height cutoff

dend_customers <- as.dendrogram(hc_customers)
dend_colored <- color_branches(dend_customers, h = 15000)

#Plotting the colored dendrogram

plot(dend_colored)

#Calculating the mean for each category
str(segment_customers)

final_segmented_customers<- segment_customers %>% 
                                  group_by(cluster) %>% 
                                  summarize("Milk"=as.integer(mean(Milk)),
                                      "Grocery"=as.integer(mean(Grocery)),
                                       "Frozen"=as.integer(mean(Frozen)))

final_segmented_customers$Cluster_Size <- clusters_size$n

final_segmented_customers

###Performing the same analysis using k-Means Clustering

#Determining the "best" value of k using average silhouette width.

#Using map_dbl to run many models with varying value of k

sil_width <- map_dbl(2:10,  function(k){
  model <- pam(x = customers_spend, k = k)
  model$silinfo$avg.width
})

#Generating a data frame containing both k and sil_width

sil_df <- data.frame(
  k = 2:10,
  sil_width = sil_width
)

#Plotting the relationship between k and sil_width

ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10)

# *Comment : From the elbow plot it is noticed that k = 2 has the highest average sillhouette width 
# and is the “best” value of k we will move forward with.*

###Analyzing the wholesale customer 
#data by building and exploring a kmeans model with 2 clusters

#Building a k-means model for the customers_spend with a k of 2

model_customers_k <- kmeans(x=customers_spend,centers=2)

#Extracting the vector of cluster assignments from the model

clust_customers_k <- model_customers_k$cluster

#Build the segment_customers dataframe

segment_customers_k <- mutate(customers_spend, cluster = clust_customers_k)

#Calculating the size of each cluster

clusters_size_k <- count(segment_customers_k, cluster)

# Calculate the mean for each category
str(segment_customers_k)

final_segmented_customers_k<- segment_customers_k %>% 
                                  group_by(cluster) %>% 
                                  summarize("Milk"=as.integer(mean(Milk)),
                                            "Grocery"=as.integer(mean(Grocery)),
                                            "Frozen"=as.integer(mean(Frozen)))

final_segmented_customers_k$Cluster_Size <- clusters_size_k$n

final_segmented_customers_k
final_segmented_customers
