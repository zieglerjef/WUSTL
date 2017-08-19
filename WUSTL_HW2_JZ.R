# load libraries and .csv files
library(foreign); library(dplyr); library(tidyr); library(matrixStats)

# since I was unable to properly clean the files in python...
# remove weird second column
unigrams <- read.csv("~/Documents/Git/WUSTL_textAnalysis/PRunigrams.csv", row.names=NULL)[,-2]
names(unigrams)[1] <- "senator"
# include check.names=F, otherwise you will get odd variable names
trigrams <- read.csv("~/Documents/Git/WUSTL_textAnalysis/PRtrigrams.csv", row.names=NULL, check.names = FALSE)[,-2]
# then clean up rest of names by removing additional text
# every variable name should just have word1.word2.word3 now
names(trigrams) <- gsub(" u'", "", names(trigrams), fixed = TRUE)
names(trigrams) <- gsub("(u'", "", names(trigrams), fixed = TRUE)
names(trigrams) <- gsub(",", ".", names(trigrams), fixed = TRUE)
names(trigrams) <- gsub("'", "", names(trigrams), fixed = TRUE)
names(trigrams) <- gsub(")", "", names(trigrams), fixed = TRUE); names(trigrams)[1] <- "senator"

### Problem 2

# create function that performs all three measures of word separation:
# 1) linear discriminant analysis,
# 2) standardized mean difference,
# 3) and standardized log odds

wordSeparation <- function(unigramDTM){
  # first, create separate DTMs for each senator
  # and remove 'senator' variable
  sessionsDTM <- unigramDTM[grep('Sessions', unigramDTM$senator),-1]
  shelbyDTM <- unigramDTM[-(grep('Sessions', unigramDTM$senator)),-1]
  
  # need to calculate means and variances for each column
  # in both senator DTMs
  sessionsMeans <- colSums(sessionsDTM) / sum(colSums(sessionsDTM))
  sessionsVariances <- colVars(as.matrix(sessionsDTM))
  shelbyMeans <- colSums(shelbyDTM) / sum(colSums(shelbyDTM))
  shelbyVariances <- colVars(as.matrix(shelbyDTM))

  # 1) linear discriminant analysis
  # which is difference in unigram means between authors over sum of variances across author
  wordSepTable <- data.frame("linearDiscriminant" = cbind((sessionsMeans - shelbyMeans) / (sessionsVariances + shelbyVariances)))

  # 2) standardized mean difference
  # take the differences of means between authors
  # over standard error, difference of means
  # (or in other words standardize by taking the sqrt of the sum of authors variance/n)
  wordSepTable$standMeanDiff <- (sessionsMeans - shelbyMeans) / sqrt((sessionsVariances/sum(colSums(sessionsDTM))) +
                                                                       (shelbyVariances/sum(colSums(shelbyDTM))))
  
  # 3) and standardized log odds
  # we need to first take each column sum + alpha (which = 1)
  # over the total n of the author DTM + sum of alphas (which = # of cols - 1)
  piSessions <- (colSums(sessionsDTM) + 1) / (sum(colSums(sessionsDTM)) + ncol(sessionsDTM)-1)
  piShelby <- (colSums(shelbyDTM) + 1) / (sum(colSums(shelbyDTM)) + ncol(shelbyDTM)-1)  
  # to get the log odds ratio
  # take log (pi1/1-p1) - log(pi2/1-pi2)
  logOdds <- log(piSessions/(1-piSessions)) - log(piShelby / (1-piShelby))
  # finally standardize the log odds ratio
  # by taking the sqrt of the variance of the ratio
  # ~ 1/(x1+1) + 1/(x2+1) 
  wordSepTable$standLogOdds <- logOdds/sqrt(var(logOdds))

  # return table of different word separation measures
  return(wordSepTable)
}

# execute function separately for senator, and then by type (unigram, trigram) 
unigramWordSep <- wordSeparation(unigrams); unigramWordSep$grams <- rownames(unigramWordSep)
trigramWordSep <- wordSeparation(trigrams); trigramWordSep$grams <- rownames(trigramWordSep)

# part 2: create plots

# take random sample of 10 observations (unigrams and trigrams)
set.seed(4); plotUnigrams <- sample_n(unigramWordSep, 15); plotTrigrams <- sample_n(trigramWordSep, 15)
pdf("~/Documents/Git/WUSTL_textAnalysis/HW2wordDistanceUnigrams.pdf")
par(mfrow=c(1,3))
# plot 1: linear discriminant
plot(plotUnigrams$linearDiscriminant, pch='', xaxt="n", xlab="", ylab="Weight", main="Linear Discriminant")
text(plotUnigrams$linearDiscriminant, label=plotUnigrams$grams, cex=.9)

# plot 2: standardized mean difference
plot(plotUnigrams$standMeanDiff, pch='', xaxt="n", xlab="", ylab="Weight", main="Standardized Mean Difference")
text(plotUnigrams$standMeanDiff, label=plotUnigrams$grams, cex=.9)

# plot 2: standardized mean difference
plot(plotUnigrams$standLogOdds, pch='', xaxt="n", xlab="", ylab="Weight", main="Standardized Log Odds")
text(plotUnigrams$standLogOdds, label=plotUnigrams$grams, cex=.9)
dev.off()

# same for trigrams
pdf("~/Documents/Git/WUSTL_textAnalysis/HW2wordDistanceTrigrams.pdf")
par(mfrow=c(1,3))
# plot 1: linear discriminant
plot(plotTrigrams$linearDiscriminant, pch='', xaxt="n", xlab="", ylab="Weight", main="Linear Discriminant")
text(plotTrigrams$linearDiscriminant, label=plotTrigrams$grams, cex=.7)

# plot 2: standardized mean difference
plot(plotTrigrams$standMeanDiff, pch='', xaxt="n", xlab="", ylab="Weight", main="Standardized Mean Difference")
text(plotTrigrams$standMeanDiff, label=plotTrigrams$grams, cex=.7)

# plot 2: standardized mean difference
plot(plotTrigrams$standLogOdds, pch='', xaxt="n", xlab="", ylab="Weight", main="Standardized Log Odds")
text(plotTrigrams$standLogOdds, label=plotTrigrams$grams, cex=.7)
dev.off()

### Problem 3

# sample 100 documents from Shelby and Sessions
shelbySample <- rbind(trigrams[sample(which(trigrams$senator=="Shelby"), 100, replace=F),], 
                      trigrams[sample(which(trigrams$senator=="Sessions"), 100, replace=F),])

library(tm)
library(RTextTools)
library(lsa)
library(cluster)
library(fpc)


d <- dist(dtm.m, method = "euclidean")

