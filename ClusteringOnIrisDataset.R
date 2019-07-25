# 1. Make a copy of the iris data
myIris <- iris

# 2. Remove the species attribute
myIris <- myIris[-5]


# 3. Calculate the 'within group sum of squares' to help decide on an appropriate number of clusters
sumsq <- (nrow(myIris)-1)*sum(apply(myIris,2,var))
for(i in 2:15) sumsq[i] <- sum(kmeans(myIris,centers=i)$withinss)

# 4. Plot the 'within group sum of squares'
plot(1:15,sumsq,type = "b",xlab="Number of Clusters", ylab="Within Groups Sum of Squares")

# 5. Apply clustering to the data, using a k-value of 3
kc <- kmeans(myIris,3)

# 6. Set a color palette for the clustering plot
palette(value=c('blue','black','green'))

# 7. Plot the clusters
axes <- c("Sepal.Length", "Sepal.Width")
plot(myIris[axes],
     col=kc$cluster, pch=16)

# 8. Aggregate the data according to the resulting clusters, returning the mean value for each
aggregate(myIris,by=list(kc$cluster),FUN = mean)

# 9. Append the cluster assignment to the rows in a data frame
myClusters <- data.frame(myIris,kc$cluster)

# 10. Display the data frame
myClusters

# 11. Load the cluster library
library(cluster)

# 12. use the daisy() function to calculate all pair-wise distances (dissimilarities)
scores <- daisy(myIris)

# 13. Use the silhouette() function to calculate the silhouettes and plot them
sil <- silhouette(kc$cluster,scores)
plot(sil)


# 14. Use the hclust() function to hierarchically cluster the data. Use the average agglomeration technique
hc <- hclust(dist(myIris),method="ave")

# 15. Plot the Dendrogram
plot(hc,hang=-1,labels=iris$Species)

