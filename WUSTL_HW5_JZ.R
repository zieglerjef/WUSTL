# Problem 1

# load libraries and .csv files
library(rjson); library(stm)
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