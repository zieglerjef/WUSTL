# Problem 1

# load libraries and .csv files
library(foreign); library(fpc); library(data.table); library(glmnet); library(e1071)
NYTstoryDTM <- read.csv("~/Documents/Git/WUSTL_textAnalysis/NYTstoriesDTM.csv", row.names=NULL)[,-2]

# a) task: use kmeans function to creat
# plot of the kmeans objective function as the 
# number of clusters varies from 2 to N-1

# the data must be scaled for comparison
# we'll just use euclidean distance
euclideanNYTdtm <- as.matrix(dist(NYTstoryDTM[,-1], method = "euclidean"))

# to see which number of clusters is appropriate
# we want to vary the objective function of a range of K
# first, create k cluster points for 2 to N-1
# cluster points represent centroids for cluster j (c_j)
objectiveFunc <- (nrow(euclideanNYTdtm) - 1 * sum(apply(euclideanNYTdtm, 2, var)))
# then assign each object to the group w/ closest centroid
# after all objects have been assigned
# recalculate positions of the cluster centroids.
# repeat until centroids no longer move
for(i in 2:nrow(euclideanNYTdtm) - 1){
  objectiveFunc[i] <- sum(kmeans(euclideanNYTdtm, centers = i)$withinss)
}
# last, plot objectiveFunc
pdf("~/Documents/Git/WUSTL_textAnalysis/HW3kmeansClusters.pdf")
plot(2:nrow(euclideanNYTdtm) - 1, objectiveFunc, pch=19, xlab="Number of clusters", ylab="Within groups sum of squares")
dev.off()

# b) apply K-Means with 6 clusters
# set seed
set.seed(4); NYTclusters6 <- kmeans(euclideanNYTdtm, 6)

# c) task: label each cluster using computer and hand methods
# i) the final cluster centers are computed as the mean for each feature
# within each final cluster
# in other words, they reflect the characteristics of the "exemplar" document for each cluster

# so, if theta_k is the cluster center for cluster k
# mean(theta_{-k}) = (sum_(theta_j)/(K-1))
# or the average of the centers not k
# then Diff_k = theta_k - mean(theta_{-k})
# use the top ten words from Diff_k to label the clusters

# so, first make data frame of cluster centers
clusterCenters <- as.data.frame(NYTclusters6$centers)
# write a function that:
# finds theta_k, which is row k of clusterCenters
# and theta_{-k}, which is rows not-k of cluster centers divided by number of clusters - 1
# to create Diff_k
top10words <- function(k, clusters){
  internalFunc <- function(k, clusters){
    Diff_k <- as.data.frame(centers[k,] - colSums(centers[-(k),])/(max(clusters)-1))
    # finally, take top 10 words
    return(colnames(Diff_k[,order(Diff_k,decreasing=T)][1:10]))
  }
  # set up matrix for top 10 words
  keywords <- matrix(NA, nrow=10, ncol=k)
  # take top 10 words for each cluster
  for (i in 1:k){
    keywords[,i] <- internalFunc(i, clusterCenters)
  }
  return (keywords)
}
# execute top10words function
top10words(6, clusterCenters)

# ii) sample and read texts assigned to each cluster and produce a hand label
# sample 2 stories from cluster 1
exploreClusters <- function(seed, k, nDocs){
  set.seed(seed)
  for(i in sample(which(NYTclusters6$cluster==k),2)){
    suppressWarnings(print(readLines(paste("~/Documents/Git/WUSTL_textAnalysis/NYTstories/",i,".txt", sep=''))))
  }
}

# these articles don't seem to match well, maybe
# they are grouped together cause they are talking 
# about "battles" and competing (on the field and in the courtroom)
exploreClusters(5, 5, 2)

# Problem 2

# c) How does the score change before and after the election?
# How does the score vary across desks?
# first, load data
storyInfo <- read.csv("~/Documents/Git/WUSTL_textAnalysis/storySentimentInfo.csv")
# then create new dataframe w/ word rate for each column
rateDF <- cbind(storyInfo[,c(1:2)], storyInfo[,c(4:5)]/storyInfo[,3][row(storyInfo[,c(4:5)])])
# see if positive word rate
posToneLM <- lm(NposPorter ~ as.factor(electionTiming) + desk,rateDF)
negToneLM <- lm(NnegPorter ~ as.factor(electionTiming) + desk,rateDF)
summary(posToneLM);summary(negToneLM)
# generally, it doesn't seem like there are more positive
# words over the course of the election (day before the election is reference)
# some desks like health and culture are more "upbeat"
# and then the editorial, national, and foreign ("hard news") is slightly 
# more negative

# Problem 3
# run naive bayes w/ leave one out validation
# find random validation observation
# and create training and test set
# first, create dichotomy noting the specified categories
# Business/Financial desk and National Desk
NYTstoryDTM$row.names <- ifelse(as.character(NYTstoryDTM$row.names)=='Business/Financial Desk' |
                                  as.character(NYTstoryDTM$row.names)=='National Desk', 1, 0)
set.seed(1)
randomObservation <- sample(which(NYTstoryDTM$row.names==1), 1)
trainingSet <- NYTstoryDTM[-randomObservation,]
validation <- NYTstoryDTM[randomObservation,]

# run naive bayes classifier
nbResults <- naiveBayes(trainingSet[,1]~., data=trainingSet)
# predict validation observation
default_pred <- predict(nbResults, as.matrix(validation[,-1]))
# predicts 0, which is incorrect

# c) use lasso and ridge
# run cv.glmnet w/ 10 folds and alpha = 1 for lasso, alpha=0 for ridge
lassoModel <- cv.glmnet(x = as.matrix(trainingSet[,-1]), y = trainingSet[,1], alpha = 1,
                       nfolds=10, family="binomial", type.measure="mse")
ridgeModel <- cv.glmnet(x = as.matrix(trainingSet[,-1]), y = trainingSet[,1], alpha = 0,
                        nfolds=10, family="binomial", type.measure="mse")
# predict category for both models
# by seeing if predicted probabilites > 0.5
predictedProbs <- c(1/(1 + exp(-predict(lassoModel, newx = as.matrix(validation[,-1]), s = lassoModel$lambda.min))),
                    1/(1 + exp(-predict(ridgeModel, newx = as.matrix(validation[,-1]), s = ridgeModel$lambda.min))))
# [1] 0.6638116 0.6748607
# both seem to do better than the naive bayes