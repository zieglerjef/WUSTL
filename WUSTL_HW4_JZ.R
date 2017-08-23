# Problem 1

# load libraries and .csv files
library(glmnet); library(data.table)
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
#
set.seed(4) 
# i) create the validation set
portugalAlcohol <- data.table(alcohol, X)
validationSet <- portugalAlcohol[1:20,]

# w/ the training data (all but the first 20 rows) perform 10 fold CV including:
# (1) linear regression, (2) LASSO, (3) Ridge, (4) Elastic-Net, and (5) Random Forest
# obtain 5 predictions for each observation in the training set, one from each observation

# create 10 folds  
folds <- sample(1:10, nrow(portugalAlcohol[-c(1:20),]), replace=T)
