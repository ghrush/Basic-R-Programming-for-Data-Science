#SAMPLING IN R

###########Recreate Functions from Other Files###################

#Numberize() - Get rid of commas and other junk and 
#converts to numbers
#Assumes that the inputVector is a list of data that
#can be treated as character strings
Numberize <- function(inputVector) {
  
  #Get rid of commas
  inputVector <- gsub(",", "", inputVector)
  
  #Get rid of spaces.
  inputVector <- gsub(" ", "",  inputVector)
  
  return(as.numeric(inputVector))
}


#Read in the census data set
readCensus <- function() {
  
  #Identify location of census data.
  urlToRead <- "https://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"
  
  #Copy census data from census website into a dataframe.
  testFrame <- read.csv(url(urlToRead)) #Will contain errors.
  
  #REMOVING ROWS AND COLUMNS
  
  #Remove the first eight rows/observations from the dataframe.
  testFrame <- testFrame[-1:-8,]
  
  #Remove the last five columns by saving the first four columns. (The previous 
  #line revealed that the last 5 columns contains 'NA' values.)
  testFrame <- testFrame[,1:5]
  
  #Create a new column and fill the column with the data from the first column. 
  #(Create a copy of column 1 with a new name.)
  testFrame$stateName <- testFrame[,1]
  
  #Remove the original first column.
  testFrame <-testFrame[,-1]
  
  #Remove the last five rows/observations from the dataframe.
  testFrame <- testFrame[-52:-58,]
  
  #Remove all periods from state names within the stateName column.
  testFrame$stateName <- gsub("\\.", "", testFrame$stateName)
  
  #Remove any spaces that may exist in each of the new columns.  
  #And, convert the cell's type to numeric.
  testFrame$april10census <- Numberize(testFrame$X)
  testFrame$april10base <- Numberize(testFrame$X.1)
  testFrame$july10pop <- Numberize(testFrame$X.2)
  testFrame$july11pop <- Numberize(testFrame$X.3)
  
  #Remove the original (4) columns from the dataframe.
  testFrame <- testFrame[, -1:-4]
  
  #Remove/reset the row names/numbering initially created by the read.csv function.
  rownames(testFrame) <- NULL
  
  #Send back the results.
  return(testFrame)
}

###############################################################

#Retrieve the census data via function call.
USstatePops <- readCensus()

#Draw a random sample from the April 2010 census data (second column 
#of the census dataframe).
sample(USstatePops$april10census, size = 8, replace = TRUE)

#Calculate and display the mean of a random sample of 8 states.
mean(sample(USstatePops$april10census, size = 8, replace = TRUE))

#REPEATING OUR SAMPLING

#(Automatically) calculate the average of a random sample of 8 states - four times.
replicate(4, mean(sample(USstatePops$april10census, size = 8, replace = TRUE)), simplify = TRUE)

#Calculate the average of all 400 of the sample means. (Summarizing the whole sampling distribution in a single average.)
mean(replicate(400, mean(sample(USstatePops$april10census, size = 8, replace = TRUE)), simplify = TRUE))

#Calculate the average of all 4000 of the sample means. (Summarizing the whole sampling distribution in a single average.)
mean(replicate(4000, mean(sample(USstatePops$april10census, size = 8, replace = TRUE)), simplify = TRUE))

#View the distribution of means using a histogram.  (Do not summarize the sample distribution.)
hist(replicate(4000, mean(sample(USstatePops$april10census, size = 8, replace = TRUE)), simplify = TRUE))

#LAW OF LARGE NUMBERS AND THE CENTRAL LIMIT THEOREM

#Calculate the average of all 100 of the sample means (using an increased sample size).  
#(Summarizing the whole sampling distribution in a single average.)
mean(replicate(100, mean(sample(USstatePops$april10census, size = 100, replace = TRUE)), simplify = TRUE))

#Calculate the average of all 100 of the sample means (using a larger sample size).  
#(Summarizing the whole sampling distribution in a single average.)
mean(replicate(100, mean(sample(USstatePops$april10census, size = 120, replace = TRUE)), simplify = TRUE))

#Save the distribution of sample means.
SampleMeans <- replicate(10000, mean(sample(USstatePops$april10census, size = 5, replace = TRUE)), simplify = TRUE)

#Verify that all (10000) of the sample means were saved.
length(SampleMeans)

#Calculate the average of all 10000 of the sample means.
mean(SampleMeans)

#Examine the summarized results of data contained in the list of sample means.
#(Look at the frequency distribution.)
summary(SampleMeans)

#Examine the frequency distribution with the quantile function.
#This function allows us to control the cuts.
quantile(SampleMeans,  probs = c(0.25, .50, .75))

#Adjust the cutoffs (2.5% mark and 97.5% mark).
quantile(SampleMeans,  probs = c(0.025, .975))

#COMPARE TWO SAMPLES

MysterySample <- c(3706690, 159358, 106405, 55519, 53883)
mean(MysterySample)

#Adjust the cutoffs (.5% mark and 99.5% mark) to make the 
#criterion more stringent.
quantile(SampleMeans,  probs = c(0.005, .995))

#Examine the standard deviation of the distribution of 
#sampling means (standard error of the mean).
sd(SampleMeans)

#Look at the standard error.
sd(USstatePops$april10census)/sqrt(5)

#Short cut to calculating cut points.
stdError <- sd(USstatePops$april10census)/sqrt(5)                 #2.5% cut point.
CutPoint975 <- mean(USstatePops$april10census) + ( 2 * stdError)  #97.5% cut point.
