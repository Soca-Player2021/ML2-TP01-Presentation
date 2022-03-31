# K-means clustering is a simple and elegant approach for partitioning a
# data set into K distinct, non-overlapping clusters. To perform K-means
# clustering, we must first specify the desired number of clusters K; then the
# K-means algorithm will assign each observation to exactly one of the K clusters.

rm(list=ls())
set.seed(5082)
library(tidyverse)

# a) Generate a simulated data set with 20 observations in each of three classes (i.e. 60 observations
# total), and 50 variables. Hint: There are a number of functions in R that you can use to
# generate data. One example is the rnorm() function; runif() is another option. Be sure to
# add a mean shift to the observations in each class so that there are three distinct classes.
SimulatedDataset <- rbind(matrix(rnorm(20*50, mean = 1), nrow = 20),
                          matrix(rnorm(20*50, mean = 2), nrow = 20),
                          matrix(rnorm(20*50, mean = 3), nrow = 20))

# b) Perform PCA on the 60 observations and plot the first two principal component score vectors.
# Use a different color to indicate the observations in each of the three classes. If the three
# classes appear separated in this plot, then continue on to part (c). If not, then return to part
# (a) and modify the simulation so that there is greater separation between the three classes.
# Do not continue to part (c) until the three classes show at least some separation in the first
# two principal component score vectors.

SimulatedDataset.pca = prcomp(SimulatedDataset,scale=TRUE)
biplot(SimulatedDataset.pca, scale=0)
summary(SimulatedDataset.pca)
SimulatedDataset.pca = prcomp(SimulatedDataset)$x
plot(SimulatedDataset.pca[,1:2], col=c(rep(1,20), rep(2,20), rep(3,20)), pch=20, cex =2)

# We can see 3 classes are properly clustered shown in different colors.
# The color of each observation indicates the cluster to which it was
# assigned using the K-means clustering algorithm.
# Note that there is no ordering of the clusters, so the cluster coloring is arbitrary.
# This arbitrary decision-making can have consequences:
# One issue, in the case, of K-means clustering, how many clusters should we look for in the data?
# K-means clustering will assign each observation to a cluster. However, sometimes this might not be appropriate.
# For instance, suppose that most of the observations truly belong to a small number of
# (unknown) subgroups, and a small subset of the observations are quite
# different from each other and from all other observations.

# c) Perform K-means clustering of the observations with K = 3. How well do the clusters that
# you obtained in K-means cluster- ing compare to the true class labels? Hint: You can use the
# table() function in R to compare the true class labels to the class labels obtained by clustering.
# Be careful how you interpret the results: K-means clustering will arbitrarily number the
# clusters, so you cannot simply check whether the true class labels and clustering labels are
# the same.
res = kmeans(SimulatedDataset, centers = 3, nstart=25)
true_class = c(rep(1,20), rep(2,20), rep(3,20))
table(res$cluster, true_class)
res$cluster

par(mfrow=c(1,2))
plot(SimulatedDataset, col = (res$cluster +1),
     main="K means Clustering Results with K= 3",
     xlab= "" , ylab="", pch=20, cex =2)

#We see our simulated dataset is perfectly clustered into 3 classes.

# d) Perform K-means clustering with K = 2. Describe your results.
res = kmeans(SimulatedDataset, centers = 2, nstart=25)
res$cluster
par(mfrow=c(1,2))
plot(SimulatedDataset, col = (res$cluster +1),
     main="K means Clustering Results with K= 2",
     xlab= "" , ylab="", pch=20, cex =2)
true = c(rep(1,20), rep(2,20), rep(3,20))
table(res$cluster, true_class)
res$tot.withinss

# We see the second class is not clustered correctly.

# e) Now perform K-means clustering with K = 4, and describe your results.
res = kmeans(SimulatedDataset, centers = 4, nstart=25)
res$cluster
par(mfrow=c(1,2))
plot(SimulatedDataset, col = (res$cluster +1),
     main="K means Clustering Results with K= 4",
     xlab= "" , ylab="", pch=20, cex =2)
true_class = c(rep(1,20), rep(2,20), rep(3,20))
table(res$cluster, true_class)
res$tot.withinss

# The result shows 4 classes instead of 3 because one of the 3 classes was
# split into 2 classes and produced the extra class.

# f)Now perform K-means clustering with K = 3 on the first two principal component score
# vectors, rather than on the raw data. That is, perform K-means clustering on the 60 x 2
# matrix of which the first column is the first principal component score vector, and the second
# column is the second principal component score vector. Comment on the results.
res = kmeans(SimulatedDataset.pca[,1:2], centers = 3, nstart=25)
res$cluster
par(mfrow=c(1,2))
plot(SimulatedDataset, col = (res$cluster +1),
     main="K means Clustering Results with K= 2",
     xlab= "" , ylab="", pch=20, cex =2)
true_class = c(rep(1,20), rep(2,20), rep(3,20))
table(res$cluster, true_class)

# The result remains the same as perfectly clustered into 3 classes.

# g) Using the scale() function, perform K-means clustering with K = 3 on the data after scaling
# each variable to have standard deviation one. How do these results compare to those obtained
# in (b)? Explain.res = kmeans(scale(X), centers = 3)
res = kmeans(scale(SimulatedDataset), centers = 3, nstart=25)
true_class = c(rep(1,20), rep(2,20), rep(3,20))
table(res$cluster, true_class)

# Scaling didn't change the clustering, and 3 classes seem to be the optimal number of clusters.
