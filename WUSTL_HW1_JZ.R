# load libraries and .csv files
library(foreign)
statementInfo <- read.csv("~/Documents/Git/WUSTL_textAnalysis/statementInfo.csv")

# task: create a visualization comparing
# overall positive and negative word rate for Obama, Romney, and Lehrer
# create new dataframe w/ word rate for each column
rateDF <- cbind(statementInfo[,c(1:2)], statementInfo[,c(4:11)]/statementInfo[,3][row(statementInfo[,c(4:11)])])

# create and save boxplots of positive and negative unstemmed word rate, by speaker
# positive unstemmed word rate
pdf("~/Documents/Git/WUSTL_textAnalysis/HW1wordRatePlot.pdf")
par(mfrow=c(1,2))
boxplot(NposWords ~ speaker, data = rateDF, xlab = "Speaker", ylab = "Proportion of a given statement",
        main = "Postive Unstemmed Word Rate")
# negative unstemmed word rate
boxplot(NnegWords ~ speaker, data = rateDF, xlab = "Speaker", ylab = "Proportion of a given statement",
        main = "Negative Unstemmed Word Rate")
dev.off()

# now let's look for trends in each speaker’s statements
# Do you notice:
# i) Trends in the measured tone?

# set up simple linear regression with rate of unstemmed positive words
# as statements increase (as time goes on in the debate)
# does Romney get more positive or negative, in comparison to Obama?
# we'll use the moderator as the reference category
trendToneLM <- lm(NposWords ~ statementNumber*speaker,
                  rateDF[rateDF$speaker=="ROMNEY"|rateDF$speaker=="OBAMA",])
summary(trendToneLM)
# generally, it doesn't seem like Romney is neg compared to Obama especially over time
# interaction interpretation: as debate goes on
# Romney gets slightly less positive in comparison to Obama (not reliable)

# ii) Response to the other candidate’s tone (examining who spoke previously)?

# first load library and then create lagged variable for previous speaker
library(DataCombine)
rateDF$previousSpeaker <- slide(rateDF, Var = "speaker", slideBy = -1)[,11]
# run regression now with previous speaker
# assume that moderator influences tone of respondents
# but we don't care if moderator is influenced by respondents
previousSpeakerLM <- lm(NposWords ~ statementNumber*previousSpeaker,
                  rateDF[rateDF$speaker=="ROMNEY"|rateDF$speaker=="OBAMA",])
summary(previousSpeakerLM)
# again doesn't appear to be much relationship

# iii) Overall interesting patterns?
# run same regression, but assume that moderator 
# doesn't influence tone of respondents
# still don't care how moderator is influenced by respondents
previousSpeakerNoModLM <- lm(NposWords ~ statementNumber*previousSpeaker,
                        rateDF[rateDF$previousSpeaker=="2" |
                                rateDF$previousSpeaker=="3" &
                                rateDF$speaker=="ROMNEY" |
                                 rateDF$speaker=="OBAMA",])
summary(previousSpeakerNoModLM) 
# not much of a relationship still...
