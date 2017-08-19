### Text as data: HW 2 (Summer 2017)

### Problem 1

# import libraries
from urllib import urlopen
import re
import os
import csv
import nltk
from nltk.corpus import stopwords
import collections

# load all press releases from Shelby and Sessions into nested dictionaries
# first, designate folder where press releases are stored in gitHub
PRfolder = 'Downloads/GrimmerSenatePressReleases-master/raw/'

# create empty list within each dictionary key
# to be filled with press releases and their associated info
pressReleases = {}
pressReleases['day']= []
pressReleases['month']= []
pressReleases['year']= []
pressReleases['senator']= []
pressReleases['text'] = []
# iterate over both Sessions' and Shelby's PRs 
for senator in [PRfolder + 'Sessions', PRfolder + 'Shelby']:
	# for each senator, iterate over each press release
    for PR in os.listdir(senator):
        # and append pressReleases with relevant info
        # since the relevant info is located in the file name
        # formatted: DayMonthYearAuthorNumber.txt
        # so, the first two elements of the file name are the days
        pressReleases['day'].append(PR[:2])
        # then the month is the next three elements
        pressReleases['month'].append(PR[2:5])
        # then the year is the next four
        pressReleases['year'].append(PR[5:9])
        # find the characters that precede the file extension .txt
        # can't just take elements since there are extra numbers in file names
        pressReleases['senator'].append(re.sub('[0-9]+.txt', '', PR[9:]))
        # open press release by file name and read in text as string
        pressReleases['text'].append(open(senator + '/' + PR, 'r').read())
        
### Problem 2 through 5

# create function to use Porter stemmer
def porterStem(unstemmedList):
	return [nltk.stem.PorterStemmer().stem(words) for words in unstemmedList]

# create new lists for unigrams and trigrams to be filled with tokens
pressReleases['unigramTokens']= []
pressReleases['trigramTokens']= []

# load a set of stop words from nlkt 
# with the other stop word additions
stopWords = stopwords.words('english') + ['shelby', 'sessions', 'richard', 'jeff', 'email', 'press', 'room', 'member', 'senate']
# apply Porter stemmer to stop words
stopWords = porterStem(stopWords)

# edit the text of pressReleases
for PR in range(0,len(pressReleases['text'])):
	# remove capitalization 
	textTokens = pressReleases['text'][PR].lower()
	# discard punctuation by removing non-word characters
	textTokens = re.sub('\W', ' ', textTokens)
	# and apply Porter stem to tokenized PRs
	textTokens = porterStem(nltk.word_tokenize(textTokens))
	# remove stop words
	textTokens = [x for x in textTokens if x not in stopWords]
	# then append unigramTokens and trigramTokens 
	pressReleases['unigramTokens'].append(textTokens)
	trigramTokens = nltk.trigrams(textTokens)
	# create list to be filled with trigrams
	trigramList = []
	# iterate over all trigram tokens and append into list
	for i in trigramTokens:
		trigramList.append(i)
	pressReleases['trigramTokens'].append(trigramList)

# Use a python dictionary to count the number of times each unigram is used
# and a second dictionary to count the number of times each trigram is used. 
# These should be counts over the whole corpus (that is, both senators’ 
# press releases).

# create empty dictionaries to be filled with counts of across-document frequency
# for unigrams and trigrams
unigramDict = {}
trigramDict = {}

# for each press release
for PR in range(0,len(pressReleases['text'])):
	# add counts to totals
    for word in pressReleases['unigramTokens'][PR]:
        if word not in unigramDict:
            unigramDict[word] = 1
        else:
            unigramDict[word] += 1
    # add counts to totals
    for word in pressReleases['trigramTokens'][PR]:
        if word not in trigramDict:
            trigramDict[word] = 1
        else:
            trigramDict[word] += 1

# sort unigrams and trigrams into new lists
mostNunigrams = []
mostNtrigrams = []

# create function to take the most used words
def extractTopN(topsList, mostNgrams):
	# loop over dictionary and append new list
	# by value, rather than key
	# then sort list
	return sorted(topsList, key=topsList.get, reverse=True)

# extract the most used unigrams
mostNunigrams = extractTopN(unigramDict, mostNunigrams)
# take only the top 1000
mostNunigrams = mostNunigrams[:1000]
# extract the most used trigrams
mostNtrigrams = extractTopN(trigramDict, mostNtrigrams)
# take only the top 500
mostNtrigrams = mostNtrigrams[:500]

# create DTM matrix and write it to .csv
# task: we need to check whether each of the top 1000 words
# is in each press release, and count their frequency
# will iterate over each press release
# first, open up .csv writer
with open('Documents/Git/WUSTL_textAnalysis/PRunigrams.csv', 'wb') as f:
    w = csv.writer(f)
    # create header to be written to .csv as variable names
    # 1st column is senator name, preceding columns 
    # represent top 1000 unigrams
    csvHeader = mostNunigrams
    csvHeader.insert(0,'senator')
    # write header first
    w.writerow(csvHeader)
    # then, we need to create counts of all the words 
	# in each document (NOT across authors or 
	# documents like the previous problem)
	# so, for each press release
    for PR in range(0,len(pressReleases['text'])):
    # create a clear row
	rowEntry = []
	# and give each unigram count in that press release
	for unigram in mostNunigrams:
		rowEntry.append(pressReleases['unigramTokens'][PR].count(unigram))
	rowEntry.insert(0,pressReleases['senator'][PR])
	# write row
	w.writerow(rowEntry)
    	
# now do for trigrams as well	
with open('Documents/Git/WUSTL_textAnalysis/PRtrigrams.csv', 'wb') as f:
    w = csv.writer(f)
    csvHeader = mostNtrigrams
    csvHeader.insert(0,'senator')
    w.writerow(csvHeader)
    for PR in range(0,len(pressReleases['text'])):
	rowEntry = []
	for trigram in mostNtrigrams:
		rowEntry.append(pressReleases['trigramTokens'][PR].count(trigram))
	rowEntry.insert(0,pressReleases['senator'][PR])
	w.writerow(rowEntry)