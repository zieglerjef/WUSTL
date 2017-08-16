### Text as data: HW 1 (Summer 2017)

### Problem 1

# import libraries
from bs4 import BeautifulSoup
from urllib import urlopen
import re
import os
import csv

# load .html file in gitHub folder (folder location will differ by user)
# find statements within <p> in HTML
pageText = BeautifulSoup(open('Documents/Git/WUSTL_textAnalysis/Debate1.html')).findAll('p')

# we know that there are three speakers
# Speakers: FORMER GOV. MITT ROMNEY, R-MASS; PRESIDENT BARACK OBAMA;
# JIM LEHRER, MODERATOR
# but even if we didn't know which names to search for, it appears 
# they are labeled by all caps
# which is how we'll identify who is speaking and speaker changes

# create empty vector to be filled with statements
statements = []
# prior speaker is set to NULL, but will be filled with the most
# recent speaker
priorSpeaker = ''

# iterate over each text block (excluding the introduction and ending)
for i in pageText[6:477]:
	# first, convert all <p> from bs4 object to strings to be searched 
	# and get rid of HTML in strings
	# '\' will appear, but it's just to escape the apostrophes
    cleanedStatements = re.sub(re.compile('<.*?>'), '', str(i))
    # then check if there is a fully capitalized word at the beginning
    # of each statement
    speakerLabelled = re.search('^[A-Z]+:', cleanedStatements)
    # and if there is...
    if speakerLabelled:
    	# record who the current speaker is (by checking which portion 
    	# of the string matched the regex))
    	currentSpeaker = speakerLabelled.group()
    	# if the current speaker matches the prior speaker, add cleaned
    	# statement to the last full statement that was added
    	# and remove current speaker from every other statement 
    	# except the first
        if currentSpeaker == priorSpeaker:
        	# since no index is specified, .pop() removes and returns
        	# the last item in the list
        	# Note: there is an extra space added because otherwise
        	# append will crunch words together
            statements.append(statements.pop() + " " +
            cleanedStatements.replace(currentSpeaker, ''))
        # if the current speaker is different than prior speaker,
        # add cleaned statement on its own
        else: 
            statements.append(cleanedStatements)
            # and reset prior speaker to the most recently recorded speaker
            priorSpeaker = speakerLabelled.group(0)
    # if there is no speaker listed (does not match regex search),
    # add cleaned statement to the last full statement that was added
    else:
    	statements.append(statements.pop() + " " + cleanedStatements)

# show example output
print statements[0:5]

### Problem 2

# create function to load sentimental dictionaries
def loadWords(type, stemmer):
	# open url specifying positive or negative dictionary
	url = urlopen('http://www.unc.edu/~ncaren/haphazard/' + type + '.txt').read()
	# since they are in .txt files, we need to split each word 
	# create the unstemmed dictionary
	unstemmedDict = url.split('\n')
	# determine which stemmer should be used
	# (1) Porter
	if stemmer=="Porter":
		# for each word in dictionary, stem
		stemmedDict = [nltk.stem.PorterStemmer().stem(word) for word in unstemmedDict]
	# (2) Snowball
	elif stemmer=="Snowball":
		# for each word in dictionary, stem
		stemmedDict = [nltk.stem.SnowballStemmer('english').stem(word) for word in unstemmedDict]
	# (3) Lancaster
	elif stemmer=="Lancaster":
		stemmedDict = [nltk.stem.LancasterStemmer().stem(word) for word in unstemmedDict]
	else:
		stemmedDict = unstemmedDict
	# return both stemmed and unstemmed dictionaries
	# by using set() instead of keeping them as lists
	# removes duplicates
	return [unstemmedDict, set(stemmedDict)]

# get basic positive and negative, unstemmed dictionaries
positiveWords = loadWords('positive', stemmer="None").pop(0)
negativeWords = loadWords('negative', stemmer="None").pop(0)

# run dictionary acquisition and stemming function for all stemmers
# (1) Porter
stemmedPositivePorter = loadWords('positive', stemmer="Porter").pop(1)
stemmedNegativePorter = loadWords('negative', stemmer="Porter").pop(1)

# (2) Snowball
stemmedPositiveSnowball = loadWords('positive', stemmer="Snowball").pop(1)
stemmedNegativeSnowball = loadWords('negative', stemmer="Snowball").pop(1)

# (3) Lancaster
stemmedPositiveLancaster = loadWords('positive', stemmer="Lancaster").pop(1)
stemmedNegativeLancaster = loadWords('negative', stemmer="Lancaster").pop(1)

# create function that will easily check how many words are in 
# corresponding dictionary list
def wordCount(inputStatement, dictionaries):
	return len([x for x in inputStatement if x in dictionaries])
# create function to pull necessary info from each statement
def statementInfo(statement, documentContent, count):
	# first, need to discard punctuation
	removedPunctuation = re.sub("\W", " ", i)
	# capitalization
	removedCaps = removedPunctuation.lower()
	# and tokenization
	reducedStatements = nltk.word_tokenize(removedCaps)
    
    # append documentContent with relevant info
	documentContent.append({
	# add to statementIter
	"statementNumber":  count,
	"speaker":re.search('^[A-Z]+', statement).group(),
	# record the number of ___ in statements w/ no punctuation, caps,
	# and reduced tokens: 
	# non-stop words
	#"NstopWords": len([x for x in reducedStatements if x not in stop_words]),
	# number of positive words
	"NposWords": wordCount(reducedStatements, positiveWords),
   	# number of negative words
   	"NnegWords": wordCount(reducedStatements, negativeWords),
   	# number of words in each positive and negative using:
   	# (1) Porter stem
   	"NposPorter": wordCount([nltk.stem.PorterStemmer().stem(y) for y in reducedStatements], stemmedPositivePorter),
   	"NnegPorter": wordCount([nltk.stem.PorterStemmer().stem(y) for y in reducedStatements], stemmedNegativePorter),
   	# (2) Snowball stem
   	"NposSnowball": wordCount([nltk.stem.SnowballStemmer('english').stem(y) for y in reducedStatements], stemmedPositiveSnowball),
   	"NnegSnowball": wordCount([nltk.stem.SnowballStemmer('english').stem(y) for y in reducedStatements], stemmedNegativeSnowball),
   	# (3) Lancaster stem
   	"NposLancaster": wordCount([nltk.stem.LancasterStemmer().stem(y) for y in reducedStatements], stemmedPositiveLancaster),
   	"NnegLancaster": wordCount([nltk.stem.LancasterStemmer().stem(y) for y in reducedStatements], stemmedNegativeLancaster)})
   	
# create empty list to fill with statement info	
statementCharacteristics = []
# begin document iterations at 0
statementIter = 0
for i in statements:
	# execute statementInfo function for each statement
	# begin document iterations at 1
	statementIter +=1
	statementInfo(i, statementCharacteristics, count=statementIter)

# with data now assigned to dictionary
# write content to .csv
with open('Documents/Git/WUSTL_textAnalysis/statmentInfo.csv', 'wb') as f:
    w = csv.DictWriter(f, fieldnames=("statementNumber", "speaker",
    "NposWords", "NnegWords", "NposPorter", "NnegPorter",
    "NposSnowball", "NnegSnowball", "NposLancaster", "NnegLancaster"))
    w.writeheader()
    for item in statementCharacteristics:
    	w.writerow(item)
