# Problem 1

# load libraries and .csv files
library(rjson); library(stm); library(tm); library(stringr)
NYTjson <- fromJSON(file="~/Documents/Git/WUSTL_textAnalysis/nyt_ac.json")

# fit a model with 8 topics that conditions on the desk of origin
# prep documents for STM
stmData <- as.data.frame(cbind(sapply(lapply(NYTjson, `[[`, c('meta', 'dsk')), paste0, collapse=""),
                               sapply(lapply(NYTjson, `[[`, c('body', 'body_text')), paste0, collapse="")))
names(stmData) <- c("desk", "documents"); stmData$documents <- as.character(stmData$documents)
# create a function to only take first 25 words to preview docs
# visualize labels
makeShortDoc <- function(x) {
  ul = unlist(strsplit(x, split = "\\s+"))[1:25]
  paste(ul,collapse=" ")
}
stmData$shortdoc <- unlist(lapply(stmData$documents, makeShortDoc))

#stmData$desk <- as.factor(stmData$desk)
processedSTM <- textProcessor(stmData$documents, metadata = stmData)
outSTM <- prepDocuments(processedSTM$documents, processedSTM$vocab, processedSTM$meta)

# fit STM w/ 8 topics
# using the default settings found in the help package
stmModel <- stm(documents = outSTM$documents, vocab = outSTM$vocab, K = 8,
    prevalence =~desk, max.em.its = 75, data = outSTM$meta,
    init.type = "Spectral")

# d) label each topic
labelTopics(stmModel, c(1:8))

# create visualization of topics by previewing docs
pdf("~/Documents/Git/WUSTL_textAnalysis/HW5topicReview.pdf")
par(mfrow = c(2, 4), mar = c(.5, .5, 1, .5))
for(i in 1:8){
  plotQuote(findThoughts(stmModel, texts = stmData$shortdoc, n = 2, topics = i)$docs[[1]],
            width = 30, main = paste0("Topic", i, sep=" "))
}
dev.off()

# e) run vanilla LDA
# using the default settings found in the help package
ldaModel <- stm(documents = outSTM$documents, vocab = outSTM$vocab, K = 8,
                max.em.its = 75, data = outSTM$meta, init.type = "Spectral")
# plot LDA and STM topic proportions
pdf("~/Documents/Git/WUSTL_textAnalysis/HW5topicProportionsSTM.pdf")
plot(stmModel, type = "hist", xlim = c(0, .5), labeltype="frex")
dev.off()

pdf("~/Documents/Git/WUSTL_textAnalysis/HW5topicProportionsLDA.pdf")
plot(ldaModel, type = "hist", xlim = c(0, .5), labeltype="frex")
dev.off()
# not much difference between the two estimated topic proportions

# Problem 2

# we'll make our DTM using the tm package primarily
# create a corpus and transform data
# read in files
# make corpus
machText <- Corpus(DirSource("~/Documents/Git/WUSTL_textAnalysis/MachText"),
              readerControl = list(language = "en"))
# remove punctuation and capitalization
machText <- tm_map(machText, removePunctuation)
machText <- tm_map(machText, content_transformer(tolower))
# we'll also remove stop words
machText <- tm_map(machText, removeWords, stopwords("english"))
# apply porter stemmer
machText <- tm_map(machText, stemDocument)
# create DTM of machTexts
machDTM <- DocumentTermMatrix(machText)
# reduce DTM to  top 500 unigrams
machDTM <- machDTM[,-c(501:2367)]
# normalize by term frequaency - i.e. divide count of each word 
# in document by total number of words in document
machDTM <- normalize(machDTM$j)

# reduce DTM to  top 500 unigrams
machDTM <- machDTM[,-c(501:2367)]
# show top 500 unigrams
# machDTM$dimnames$Terms
# see what DTM looks like
machDTM <- machDTM/rowSums(as.matrix(machDTM))
inspect(machDTM)

# Problem 3

# 2) apply the function prcomp 
# scale data to ensure each column has unit variance
machPCA <- prcomp(machDTM, scale = T)

# a ) reate a plot of variance explained by each additional principal component
pdf("~/Documents/Git/WUSTL_textAnalysis/HW5screePlot.pdf")
plot(machPCA, type = "l")
dev.off()
# the "elbow rule" doesn't really apply, and we don't want to introduce
# more error for better fit at a certain point
# so we'll include 10 components

pdf("~/Documents/Git/WUSTL_textAnalysis/HW5pcaEmbedding.pdf")
plot(machPCA$rotation, pch='',
     xlab="1st Principal Component",ylab="2nd Principal Component")
text(machPCA$rotation, labels=str_extract(machDTM$dimnames$Docs, "[[:digit:]]+"),cex= 0.7)
dev.off()

# c) looking at figure 4, it appears that the first two components
# are orthogonal to each other and that most documents tend 
# toward zero along both dimensions
# find extreme docs (13, 112, 134; and 76)
# first component seems to be advice to the ruler
# w/ examples that use Romans frequently
# second compotent is discussing protecting a ruler's state