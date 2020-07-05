#SENTIMENT ANALYSIS

#Download positive and negative words from:  
#https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html


#Identify the positive and negative word files.
pos <- "positive-words.txt"
neg <- "negative-words.txt"

#Read in words from both files, seperating each word.
p <- scan(pos, character(0), sep = "\n")
n <- scan(neg, character(0), sep = "\n")

#Examine first 10 words in the positive words list.
head(p, 10) #Contains header information.

#Remove header information from both lists.
p <- p[-1:-34]
n <- n[-1:-34]

#Examine first 10 words in both the positive 
#and negative words lists.
head(p, 10)
head(n, 10)

#Calculate the total number of words. Use data imported  
#and sorted with the file 'Analyzing Unstructured Data (Natural Language Text).R'.
totalWords <- sum(wordCounts)

#Create a vector of the target document's words.
words <- names(wordCounts)

#Produce a vector containing indices of positively word matches.
matched <- match(words, p, nomatch = 0)

#Examine first 10 indices from the matching positive words vector. 
head(matched, 30)

#Get a count of all the matching positive words.
mCounts <- wordCounts[which(matched != 0)]
length(mCounts)

#Create a seperate list of positive words and a sum of their counts.
mWords <- names(mCounts)
nPos <- sum(mCounts)


#Do the same for negative words as done for positive words.
matched <- match(words, n, nomatch = 0)
nCounts <- wordCounts[which(matched != 0)]
nWords <- names(nCounts)
nNeg <- sum(nCounts)

#Calculate the percentage of words that are positive or negative
totalWords <- length(words)
ratioPos <- nPos/totalWords
ratioPos

ratioNeg <- nNeg/totalWords
ratioNeg



