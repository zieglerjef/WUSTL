### Text as data: HW 3 (Summer 2017)

### Problem 1

# import libraries
import json
import nltk
from nltk.corpus import stopwords
import re
import os
from urllib import urlopen

# a) download {nyt_ac.json} from gitHub
with open('Documents/Git/WUSTL_textAnalysis/nyt_ac.json') as data_file:
    NYTjson = json.load(data_file)
    
# b) inspect the structure of the data
type(NYTjson)
# shows that json object is a list
# check the first elements as example
NYTjson[0].keys()
# two initial dictionaries (body, meta)
NYTjson[0]['body'].keys()
# within body, info like title and text

# c) write each story into new .txt file 
for story in range(0,len(NYTjson)):
	# give title based on order
	with open('Documents/Git/WUSTL_textAnalysis/NYTstories/' +
	str(story) + '.txt', 'w') as file:
		# since we know where the relevant info is stored
		# use dump to write json as string by key
		file.write(json.dumps(NYTjson[story]['body']))

# check that files wrote properly
os.listdir('Documents/Git/WUSTL_textAnalysis/NYTstories/')

# d) task: Using NYTjson file, and create a DTM
# of the 1000 most used terms
# be sure to: 
# discard word order
# remove stop words
# apply porter stemmer

# create function to use Porter stemmer
def porterStem(unstemmedList):
	return [nltk.stem.PorterStemmer().stem(words) for words in unstemmedList]

# load a set of stop words from nlkt 
# with the other stop word additions
stopWords = stopwords.words('english')
# apply Porter stemmer to stop words
stopWords = porterStem(stopWords)

# edit the text of pressReleases
for story in range(0,len(NYTjson)):
	# create key in each NYT story with cleaned text
	NYTjson[story]['body']['cleanText'] = []
	# remove capitalization 
	textTokens = NYTjson[story]['body']['body_text'].lower()
	# discard punctuation by removing non-word characters
	textTokens = re.sub('\W', ' ', textTokens)
	# and apply Porter stem to tokenized story
	textTokens = porterStem(nltk.word_tokenize(textTokens))
	# remove stop words
	textTokens = [x for x in textTokens if x not in stopWords]
	# then append unigram tokens 
	NYTjson[story]['body']['cleanText'] = textTokens
	
# now that we have cleaned text, need to create DTM
# create empty dictionaries to be filled with counts of unigrams
unigramDict = {}

# for each story
for story in range(0,len(NYTjson)):
	# add counts to totals
    for word in NYTjson[story]['body']['cleanText']:
        if word not in unigramDict:
            unigramDict[word] = 1
        else:
            unigramDict[word] += 1
    

# sort unigrams and trigrams into new lists
mostNunigrams = []

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

# now that we have the most 1000 used unigrams
# create DTM matrix and write it to .csv
# task: we need to check whether each of the top 1000 words
# is in each NYT story, and count their frequency
# will iterate over each NYT story
# first, open up .csv writer
with open('Documents/Git/WUSTL_textAnalysis/NYTstoriesDTM.csv', 'wb') as f:
    w = csv.writer(f)
    # create header to be written to .csv as variable names
    # 1st column is desk name, preceding columns 
    # represent top 1000 unigrams
    csvHeader = mostNunigrams
    csvHeader.insert(0,'desk')
    # write header first
    w.writerow(csvHeader)
    # then, we need to create counts of all the words 
	# in each document (NOT across authors or 
	# documents like the previous problem)
	# so, for each story
    for story in range(0,len(NYTjson)):
    # create a clear row
	rowEntry = []
	# and give each unigram count in that story
	for unigram in mostNunigrams:
		rowEntry.append(NYTjson[story]['body']['cleanText'].count(unigram))
	rowEntry.insert(0, NYTjson[story]['meta']['dsk'])
	# write row
	w.writerow(rowEntry)

# Problem 2

# download list of positive and negative stop words
# create function to load sentimental dictionaries
def loadWords(type, stemmer):
	# open url specifying positive or negative dictionary
	url = urlopen('http://www.unc.edu/~ncaren/haphazard/' + type + '.txt').read()
	# since they are in .txt files, we need to split each word 
	# create the unstemmed dictionary
	unstemmedDict = url.split('\n')
	# determine which stemmer should be used
	# (1) Porter
	if stemmer=='Porter':
		# for each word in dictionary, stem
		stemmedDict = [nltk.stem.PorterStemmer().stem(word) for word in unstemmedDict]
	# (2) Snowball
	elif stemmer=='Snowball':
		# for each word in dictionary, stem
		stemmedDict = [nltk.stem.SnowballStemmer('english').stem(word) for word in unstemmedDict]
	# (3) Lancaster
	elif stemmer=='Lancaster':
		stemmedDict = [nltk.stem.LancasterStemmer().stem(word) for word in unstemmedDict]
	else:
		stemmedDict = unstemmedDict
	# return both stemmed and unstemmed dictionaries
	return [unstemmedDict, stemmedDict]

# get basic positive and negative, porter stemmed dictionaries
positiveWords = loadWords('positive', stemmer='Porter').pop(1)
negativeWords = loadWords('negative', stemmer='Porter').pop(1)

# create function that will easily check how many words are in 
# corresponding dictionary list
def wordCount(inputStatement, dictionaries):
	return len([x for x in inputStatement if x in dictionaries])
# create function to pull necessary info from each statement
def storyInfo(story, documentContent):
	# first, need to discard punctuation
	removedPunctuation = re.sub('\W', ' ', str(NYTjson[story]['body']['cleanText']))
	# capitalization
	removedCaps = removedPunctuation.lower()
	# and tokenization
	reducedStatements = nltk.word_tokenize(removedCaps)
    
    # append documentContent with relevant info
	documentContent.append({
	# add to statementIter
	'electionTiming': NYTjson[story]['meta']['publication_day_of_month'],
	'desk': NYTjson[story]['meta']['dsk'],
	# record the number of ___ in statements w/ no punctuation, caps,
	# and reduced tokens: 
	# non-stop words so that we can look at rates in analysis
	'NonstopWords': len([x for x in reducedStatements if x not in stopWords]),
   	# number of words in each positive and negative using:
   	# (1) Porter stem
   	'NposPorter': wordCount([nltk.stem.PorterStemmer().stem(y) for y in reducedStatements], positiveWords),
   	'NnegPorter': wordCount([nltk.stem.PorterStemmer().stem(y) for y in reducedStatements], negativeWords),})
   	
# create empty list to fill with statement info	
storyCharacteristics = []
# execute story info extraction on each story
for i in range(0,len(NYTjson)):
	# execute statementInfo function for each statement
	storyInfo(i, storyCharacteristics)

# with data now assigned to dictionary
# write content to .csv to analyze in R
with open('Documents/Git/WUSTL_textAnalysis/storySentimentInfo.csv', 'wb') as f:
    w = csv.DictWriter(f, fieldnames=('electionTiming', 'desk', 'NonstopWords',
    'NposPorter', 'NnegPorter'))
    w.writeheader()
    for item in storyCharacteristics:
    	w.writerow(item)