---
title: "Clusters Tutorial"
author: "Jeffrey Strickland"
date: "2024-06-13"
output:
  word_document: 
    reference_docx: C:\Users\jeff\Documents\R\book_template.docx
  always_allow_html: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Steps

1. Install and import relevant libraries.

2. Create the data.

3. Use K-means and medoids to cluster data.

4. Use Hierarchical clustering.

5. Use Gaussian Mixed Models to cluster.

6. Compare the accuracy of each method.

## Step 1. Install and import relevant libraries

In this step, we'll import the necessary R libraries.

```{r , message=FALSE}
library("factoextra")
library("mvtnorm")
library("dplyr")
library("ggplot2")
library("fpc")
library("caret")
library("mclust")
library("cluster")

for(pkg in c("mvtnorm", "dplyr", "ggplot2", "fpc", "caret", "mclust", "factoextra", "cluster")){
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

set.seed(42)
```

## Step 2. Create the data

For this tutorial we’ll generate data so that we can be sure of the underlying structure and have labels for our dataset to compare the efficacy of different methods. We generate 3 clusters using normal distributions. Each data point has three features: an x value, a y value, and a class that labels the point. We'll have three classes that we'd like our clustering algorithms to represent as clusters.

```{r}
dataset <- {
  #create our 3 distributions
  cluster1 = data.frame(rmvnorm(40, c(0, 0), diag(2) * c(2, 1))) %>% mutate(class=factor(1))
  cluster2 = data.frame(rmvnorm(100, c(3, 3), diag(2) * c(1, 3))) %>% mutate(class=factor(2))
  cluster3 = data.frame(rmvnorm(60, c(6, 6), diag(2))) %>% mutate(class=factor(3))
  
  #bind them together
  data = bind_rows(cluster1, cluster2, cluster3)
  #set the column names
  names(data) = c("x", "y", "class")
  #return the data
  data
}
```

Since the data is being generated here we don’t need to check for missing values or normalize our features. If our x feature had values between -1000 and 1000 and our y feature had values between -1 and 1, we would want to normalize those so that distance measures could be calculated accurately. In this case though, that’s not necessary.

We can plot our data in a scatter plot using ggplot to see the natural clustering that we’ll try to replicate:

```{r , message=FALSE, warning=FALSE, fig.width=5, fig.height=3, dpi=330, error=FALSE}

dataset %>% ggplot(aes(x=x, y=y, color=class)) +
  geom_point() +
  coord_fixed() +
  scale_shape_manual(values=c(0, 1, 2))
```

We can see in this visualization that the boundaries between clusters are not well defined, which will present an interesting challenge and basis for comparison with our clustering algorithms.

## Step 3. Use K-means and medoids to cluster data

K-means clustering is one of the most commonly used unsupervised machine learning algorithms for partitioning a given data set into a set of k clusters, where k represents the number of groups pre-specified by the analyst. It classifies objects in multiple groups so that objects clustered together are as similar as possible, referred to as high intra-class similarity, and objects from different clusters are as dissimilar as possible, referred to as low inter-class similarity. In k-means, each cluster is represented by its centroid, which corresponds to the mean of points assigned to the cluster.

To check the optimal number of clusters, we can use the fviz_nbclust method.

```{r , message=FALSE, warning=FALSE, fig.width=5, fig.height=3.5, dpi=330, error=FALSE}
fviz_nbclust(dataset, kmeans, method = "wss") + geom_vline(xintercept = 3, linetype = 2)
```

We want to look at the elbow of the plot where the within-cluster sum of squares drops significantly.

Now that we know the optimal number of clusters we can perform a k-means clustering. By default the call to kmeans() will minimize the Euclidean distance between data points to define clusters.

```{r}
k = 3
kmeans_clustering <- kmeans(dataset[c("x", "y")], centers = k, nstart = 20)
```

We can use the fviz_cluster() method to view the results of our K-means clustering.

```{r , message=FALSE, warning=FALSE, fig.width=5, fig.height=3.5, dpi=330, error=FALSE}
fviz_cluster(kmeans_clustering, dataset[c("x", "y")], xlab = FALSE, ylab = FALSE, geom="point")
```

We can check the accuracy of our clustering in a plot:

```{r , message=FALSE, warning=FALSE, fig.width=5, fig.height=3.5, dpi=330, error=FALSE}
kmeans_res <- dataset
kmeans_res['predicted_class'] = factor(kmeans_clustering$cluster)

kmeans_res %>% ggplot(aes(x=x, y=y, shape=class, color=predicted_class)) +
  geom_point() +
  coord_fixed() +
  scale_shape_manual(values=c(0, 1, 2)) +
  scale_shape(solid = TRUE)
```

We see that K-means clustering results match our data quite well but this approach has some trouble with our clusters overlapping. This is to be expected since we generated a tricky dataset to use and the cluster means don’t fully capture the structure of the data. Since we know the labels, we can check the accuracy using a confusion matrix.

```{r}
knn_conf_mat <- confusionMatrix(factor(kmeans_clustering$cluster), factor(dataset$class), mode = "everything", positive="1")
```

This gives us a confusion matrix and an accuracy score.

```{r}
print(knn_conf_mat)
```

Which returns:


**Accuracy** is perhaps the most basic evaluation metric for classification models, although it does not always offer a complete picture of model performance.

**Kappa** is also known as Cohen’s kappa. This is another accuracy metric that indicates the level of agreement between the ground truth and predictions beyond the level of agreement resulting from chance.

For comparison now we can use medoids instead of using centroids with the `pam()` clustering. Unlike the k-means algorithm, using medoids selects a median point to calculate the clusters rather than cluster centroids.

```{r}
pam_k = 3
pam_clustering <- pam(dataset[c("x", "y")], pam_k)
```

By visual inspection, we can see that this performs slightly worse:

```{r , message=FALSE, warning=FALSE, fig.width=5, fig.height=3.5, dpi=330, error=FALSE}
pam_res <- dataset
pam_res['predicted_class'] <- factor(pam_clustering$cluster)

pam_res %>% ggplot(aes(x=x, y=y, shape=class, color=predicted_class)) +
  geom_point() +
  coord_fixed() +
  scale_shape_manual(values=c(0, 1, 2)) + 
  scale_shape(solid = TRUE)
```

Checking the accuracy with a confusion matrix gives us the following results:

```{r}
pam_conf_mat <- confusionMatrix(factor(pam_clustering$cluster), factor(dataset$class), mode = "everything", positive="1")
print(pam_conf_mat)
```

This shows that the medoids approach doesn't work quite as well as the centroids:

### Step 4. Use Hierarchical clustering

We'll look at two types of hierarchical clustering:

    Agglomerative clustering
    Divisive clustering

### Agglomerative clustering

Agglomerative clustering is the most common type of hierarchical clustering used to group objects in clusters based on some similarity metric. This begins by treating each individual data point as a cluster and successively merging upwards until all clusters have been merged into one cluster at the base of the tree. The result is a tree-based representation of the objects called a dendrogram. Hierarchical clustering relies on maximizing the dissimilarity of each cluster but this can also make it more sensitive to outliers when performing segmentation.

One of the most commonly used is the hclust() method of the stats library. First we construct distance matrix of the distances for between each point using a Euclidean distance and then construct the hierarchical clustering using a call to hclust. Each datasets may perform differently with different distance measures but in this case a simple Euclidean distance will work as well as any other distance metric since the data has the same range along all features.

```{r}
distances = dist(dataset[c("x", "y")], method = 'euclidean')
agglomerative_clustering <- hclust(distances, method = 'average')
```

Now we can visualize the resulting dendogram:

```{r , message=FALSE, warning=FALSE, fig.width=5, fig.height=3.5, dpi=330, error=FALSE}
fviz_dend(agglomerative_clustering, cex = 0.5, k = 3, color_labels_by_k = TRUE, show_labels=FALSE)
```

In this case though, the agglomerative clustering doesn’t represent our data especially well as we can see in the dendrogram. We can compare the actual labels to the generated clusters:

```{r , message=FALSE, warning=FALSE, fig.width=5, fig.height=3.5, dpi=330, error=FALSE}
cut_avg <- cutree(agglomerative_clustering, k = 3)
dataset_agg_cluster <- mutate(dataset, cluster = cut_avg)
ggplot(dataset_agg_cluster, aes(x=x, y = y, color = factor(cluster), shape=class)) + geom_point()
```

Again, we’ll check the accuracy and Kappa coefficient with a confusion matrix:

```{r}
ac_conf_mat <- confusionMatrix(factor(dataset_agg_cluster$cluster), factor(dataset_agg_cluster$class), mode = "everything", positive="1")
print(ac_conf_mat)
```

We can see that this has poor accuracy:


### Divisive clustering

Now we’ll look at a divisive clustering approach to our dataset. The inverse of agglomerative clustering is divisive clustering, which can be with the diana() function (DIvisive ANAlysis). This algorithm works in a top-down manner, beginning at the root representing all the data points in a single cluster. At each step of iteration, the most heterogeneous cluster is divided into two. The process is iterated until all objects are in their own cluster.

```{r}
# compute divisive hierarchical clustering
divisive_clustering <- diana(dataset[c("x", "y")])
# Divise coefficient
print(divisive_clustering$dc)
```

Now we can check the generated dendogram:

```{r , message=FALSE, warning=FALSE, fig.width=5, fig.height=3.5, dpi=330, error=FALSE}
#plot using a colors dendrogram to see our clusters with 3 groups
fviz_dend(divisive_clustering, cex = 0.5, k = 3, color_labels_by_k = TRUE, show_labels=FALSE)
```

When we check the confusion matrix, we can see that this structure represents our data much more accurately than the agglomerative clustering:

```{r}
dc_conf_mat <- confusionMatrix(factor(cutree(divisive_clustering, k = 3)), factor(dataset$class), mode = "everything", positive="1")
print(dc_conf_mat)
```

The divisive approach performs much better than the agglomerative approach:

Next, we can look at an approach that should better match our underlying data process.

## Step 5. Use Gaussian Mixed Models to cluster

Model-based clustering, which treats the data as though it is coming from a distribution which is a mixture of two or more clusters. Unlike k-means, the model-based clustering uses a soft assignment, where each data point has a probability of belonging to each cluster. Each cluster is modeled by the normal or Gaussian distribution which is described by the following parameters:

*    µk: mean vector

*    Σk: covariance matrix

*    An associated probability in the mixture. Each point has a probability of belonging to each cluster.

We can create a GMM clustering with the Mclust function in the Mclust package. Our data is roughly normalized but if the dimensions of our features are very different, we’ll want to make sure that we normalize those features in our data preparation.

```{r}
library(mclust)   # for fitting clustering algorithms

# Create a GMM model
dataset_mc <- Mclust(dataset[c('x', 'y')])
summary(dataset_mc)
```

After fitting the model we should look at a summary of the model using the summary() method. We can see that the model inferred the the correct number of clusters:

```{r , message=FALSE, warning=FALSE, fig.width=5, fig.height=4, dpi=330, error=FALSE}
# Plot results
plot(dataset_mc, what = "density")
```

We can look at a confusion matrix to check how well our model fits the data. One aspect of this to note is that the cluster assignment generated by mclust may not match our original labels. To get a correct measure of accuracy, we’ll recode the generated labels so that the cluster membership matches our original labels.

```{r}
mc_recoded <- recode(dataset_mc$classification, '1' = '2', '2' = '1', '3' = '3')
gmm_conf_mat <- confusionMatrix(factor(mc_recoded), factor(dataset$class), mode = "everything", positive="1")
print(gmm_conf_mat)
```

The GMM approach show fairly high accuracy and Kappa values:

Since our original data was constructed from three Gaussian mixtures, we shouldn’t be surprised to see that this model based approach reconstructs it quite well.

Our model does seem to have some trouble where the edges of the distributions representing our different clusters overlap with one another. One helpful feature of the GMM approach to clustering is that we can see the uncertainty of the model by plotting the uncertainty of the model:

```{r , message=FALSE, warning=FALSE, fig.width=5, fig.height=4, dpi=330, error=FALSE}
plot(dataset_mc, what = "uncertainty")
```

This shows us which members of our discovered clusters the model is least certain about. In the graph, larger data points have more uncertainty, smaller have less. The GMM approach performs well with outlier points but we can see that the overlapping areas of the distributions are difficult for our model to identify correctly, but this approach has the advantage of returning the uncertainty for each data point.

## Step 7. Compare the accuracy of each method

Now we can look at the results of each of our clustering methods:

```{r}
comparison <- c(c(knn_conf_mat$overall[[1]], pam_conf_mat$overall[[1]], ac_conf_mat$overall[[1]], dc_conf_mat$overall[[1]], gmm_conf_mat$overall[[1]]),
                c(knn_conf_mat$overall[[2]], pam_conf_mat$overall[[2]], ac_conf_mat$overall[[2]], dc_conf_mat$overall[[2]], gmm_conf_mat$overall[[2]]))

comparison <- matrix(comparison, nrow=2, byrow=TRUE)

colnames(comparison) = c('KNN','Medoids','Agglomerative','Divisive', 'GMM')
rownames(comparison) <- c('Accuracy','Kappa')
```

Now we can generate the table:

```{r}
as.table(comparison)
```

We can see that the GMM approach performs slightly better than the KNN and that, with the data that we’ve generated, hierarchical approaches don’t model the underlying data as well. That is much more a reflection of our data rather than a feature of hierarchical clustering. Finding the right approach to modeling your data will depend on your dataset and what you’re looking to uncover by clustering.
Summary and next steps

In this tutorial, we looked at generating a complex data set with natural clusters and the basics of how different clustering algorithms perform on that data. The data consisted of 3 natural clusters defined by normal distributions. We then looked at how to perform clustering using KNN, Medoids, Hierarchical Clustering, and Gaussian Mixed Models. Finally, we compared the accuracy of these algorithms using a confusion matrix to generate Accuracy and Kappa scores.


