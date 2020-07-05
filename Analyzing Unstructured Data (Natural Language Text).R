###################Required Functions######################################

#This function tests whether a package has already been download
#and installed prior to attempting to do so.
EnsurePackage <- function(x)
{
  #Get copy of th package name.
  x <- as.character(x)
  
  if(!require(x, character.only = TRUE)){
    install.packages(pkgs = x, repos = "http://cran.r-project.org")
    require(x, character.only = TRUE)
  }
}


###########################################################################

#READING IN TEXT FILES

#Identify the target text file.
sbaFile <- "sba.txt"

#Read data with 'scan' function.
sba <- scan(sbaFile, character(0), sep = "\n")

#Examine the first 3 rows of data.
head(sba, 3)


#Read data with 'readLines' function.
sba <- readLines(sbaFile)

#Examine the first 3 rows of data.
head(sba, 3)

#Read data directly from a web page.
EnsurePackage("XML") 

sbaLocation <- URLencode(
  "http://www.historyplace.com/speeches/anthony.htm")

doc.html <- htmlTreeParse(sbaLocation, useInternal = TRUE)

sba <- unlist(xpathApply(doc.html, '//p', xmlValue))
head(sba, 3)

#USING THE TEXT MINING PACKAGE

#Load Package for NLP.
EnsurePackage("tm")

#Add the (sba) text file to a 'text file vector'.
words.vec <- VectorSource(sba)

#Add the text file vector to the tm custom class / 'corpus'. 
words.corpus <- Corpus(words.vec)

#Display corpus details.
words.corpus

#Transform data: Make all text lower case and remove punctuation,
#numbers, and 'stopwords'.
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))

#Conduct a statistical analysis of the corpus via a term-document matrix.
tdm <- TermDocumentMatrix(words.corpus)

#Get a summary of the term-document's contents.
inspect(tdm)

#CREATING WORD CLOUDS

#Load Package for building word clouds.
EnsurePackage("wordcloud")

#Convert text back to a plain data matrix
m <- as.matrix(tdm)

#Create a list of row and column sums for each word.
wordCounts <- rowSums(m)

#Sort the list so the highest frequency is first.
wordCounts <- sort(wordCounts, decreasing = TRUE)

#Look at the top terms in the list (appears most frequently).
head(wordCounts)

#Create a dataframe from the term list and frequency list.
cloudFrame <- data.frame(word = names(wordCounts), freq = wordCounts)

#Generate a word cloud using the newly created dataframe.
wordcloud(cloudFrame$word, cloudFrame$freq)

#Re-generate the word cloud:
#1. Remove unimportant words by setting a minimum word frequency.
#2. Set a maximum number of words displayed.
#3. Add color. (Darker words appear most frequently.)
wordcloud(names(wordCounts), 
          wordCounts, min.freq = 2,
           max.words = 50, rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))



