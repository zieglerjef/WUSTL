# Problem 1

# load libraries and .csv files
library(glmnet); library(data.table); library(randomForest)
load("~/Documents/Git/WUSTL_textAnalysis/StudentDrinking.RData")

# i) fit a linear regression of alcohol on the covariates in the included data
olsModel <- lm(alcohol~X)
# ii) fit a lasso regression of alcohol on the covariates
# alpha = 1 for lasso, alpha=0 for ridge
lassoModel <- cv.glmnet(x = X, y = alcohol, alpha = 1)
# iii  fit a ridge regression of alcohol on the covariates
ridgeModel <- cv.glmnet(x = X, y = alcohol, alpha = 0)
# iv) fit an elastic-net regression of alcohol on the covariates (alpha = 0.5)
elasticModel <- cv.glmnet(x = X, y = alcohol, alpha = 0.5)
# alpha = 0.5 implies a "balance" between the penalty occurred
# under ridge (lamba*sum(beta_j)) and lasso (lamba*sum(abs(beta_j)))

# v) a) task: obtain a matrix of coefficients for the models used in (ii-iv).
lassoLambda <- data.table("maleCoef" = t(as.matrix(coef(lassoModel, s = lassoModel$lambda)))[,3],
                          "lambda"=lassoModel$lambda, "model"="lasso")
ridgeLambda <- data.table("maleCoef" = t(as.matrix(coef(ridgeModel, s = ridgeModel$lambda)))[1:65,3],
                          "lambda"=ridgeModel$lambda[1:65], "model"="ridge")
elasticLambda <- data.table("maleCoef" = t(as.matrix(coef(elasticModel, s = elasticModel$lambda)))[1:65,3],
                            "lambda"=elasticModel$lambda[1:65], "model"="elastic")

# b) plot the coefficient on male against the value of lambda from the models in ii-iv
# and include the coefficient from OLS as a flat line
# merge data
plotDataframe <- merge(lassoLambda, ridgeLambda, by=c("maleCoef", "lambda", "model"), all=T)
plotDataframe <- cbind(olsModel$coefficients[3], merge(elasticLambda, plotDataframe, by=c("maleCoef", "lambda", "model"), all=T))

pdf("~/Documents/Git/WUSTL_textAnalysis/HW4lambda.pdf")
# we can see from the plot that as lambda --> 0, we get closer to the ols coef
# which is displayed as the solid, horizontal black line at 0.94
ggplot(plotDataframe, aes(x = lambda, y = maleCoef, col = model, linetype = model)) +
  geom_line(lwd = 1, alpha=.4) + geom_hline(yintercept=plotDataframe$V1) +
  theme_bw() + scale_x_continuous(limits = c(0, 3))
dev.off()


# Problem 2

set.seed(4) 
# i) create the validation set
alcoholValid <- alcohol[c(1:20)]; xValid <- X[c(1:20),]
alcoholTraining <- alcohol[-c(1:20)]; xTraining <- X[-c(1:20),]
# w/ the training data (all but the first 20 rows) perform 10 fold CV including:
# create 10 folds  
folds <- sample(1:10, length(alcoholTraining), replace=T)
# create vectors to fill with predicted values
olsPredictions <- c(); lassoPredictions <- c(); ridgePredictions <- c(); 
elasticPredictions <- c(); randomPredictions <- c()
# for each fold
for(i in 1:10){
  # find which observations are included in fold and which aren't
  trainingData <- which(folds!=i)
  testData <- which(folds==i)
  # (1) linear regression
  olsTrain <- lm(alcoholTraining[trainingData] ~ xTraining[trainingData, ])
  # get predicted value
  olsPredictions[testData] <- predict(olsTrain, newdata=as.data.frame(xTraining[testData, ]))
  # (2) lasso
  # alpha = 1 for lasso, alpha=0 for ridge
  lassoTrain <- cv.glmnet(y = alcoholTraining[trainingData], x = xTraining[trainingData, ], alpha = 1)
  # get predicted value
  lassoPredictions[testData] <- predict(lassoTrain, newx= xTraining[testData, ], s = lassoTrain$lambda.min) 
  # (3) ridge
  ridgeTrain <- cv.glmnet(y = alcoholTraining[trainingData], x = xTraining[trainingData, ], alpha = 0)
  # get predicted value
  ridgePredictions[testData] <- predict(ridgeTrain, newx= xTraining[testData, ], s = ridgeTrain$lambda.min,type = "class") 
  # (4) elatist-net
  elasticTrain <- cv.glmnet(y = alcoholTraining[trainingData], x = xTraining[trainingData, ], alpha = 0.5)
  # get predicted value
  elasticPredictions[testData] <- predict(elasticTrain, newx= xTraining[testData, ], s = elasticTrain$lambda.min,type = "class") 
  # (5) random forest
  #randomTrain <- randomForest(alcoholTraining[trainingData] ~ xTraining[trainingData, ])
  # get predicted value
  # randomPredictions[testData] <- predict(randomTrain, xTraining[testData,])
}
# 
# obtain weights
modelWeights <- lm(alcoholTraining ~ cbind(olsPredictions, lassoPredictions, ridgePredictions, elasticPredictions) -1)

