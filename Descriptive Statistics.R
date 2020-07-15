#UNDERSTANDING DESCRIPTIVE STATISTICS

###########Recreate Dataframe From Other Files####################

#Create a vector of family member names.
myFamilyNames <- c("Dad", "Mom", "Sis", "Bro", "Dog")

#Check the vector's contents. 
myFamilyNames

#Create a vector of family ages.
myFamilyAges <- c(43, 42, 12, 8, 5)

#Access the second element in the vector/list.
myFamilyAges[2]

#Create a vector of family genders.
myFamilyGenders <- c("Male", "Female", "Female", "Male", "Female")

#Create a vector of family weights.
myFamilyWeights <- c(188, 136, 83, 61, 44)

#Build a dataframe to house family data.
myFamily <- data.frame(myFamilyNames, myFamilyAges,
                       myFamilyGenders, myFamilyWeights)

#View of the contents of the myFamily dataframe.
myFamily

###########################################################


#Use the built-in 'var' function to calculate the variance of family ages.
var(myFamily$myFamilyAges)

#Use the built-in 'sd' function to calculate the standard deviation of family ages.
sd(myFamily$myFamilyAges)

#Test both functions using adhoc vectors.
var(c(43, 42, 12, 8, 5))
sd(c(43, 42, 12, 8, 5))

#USING DESCRIPTIVE STATISTICS

#Create functions for repeatedly importing census data.

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


#Retrieve the census data via function call.
USstatePops <- readCensus()

#View the first three rows of the April 2010 census data (secound column).
USstatePops$april10census[1:3]


#Examine some descriptive statistics for the April 2010 census data (secound column of the dataframe).
mean(USstatePops$april10census)   #Measure of central tendency.
median(USstatePops$april10census) #Measure of central tendency.
#mode(USstatePops$april10census)   #Measure of central tendency. ***Produces error. Must use mfv function
mfv(USstatePops$april10census)   #Measure of central tendency.
range(USstatePops$april10census)  #Measure of dispersion.
var(USstatePops$april10census)    #Measure of dispersion.
sd(USstatePops$april10census)     #Measure of dispersion.

#USING HISTOGRAMS TO UNDERSTAND A DISTRIBUTION

#Create a histogram of the the April 2010 census data (secound column of the dataframe).
hist(USstatePops$april10census)

#Create a histogram of the the April 2010 census data with more breaks (cells for the histogram).
hist(USstatePops$april10census, breaks = 20)

#Create a histogram for a random dataset that fits the normal distribution.
#The rnorm generates the random dataset. 
hist(rnorm(51, 6043834, 6823984))

#Recreate the previous histogram with a title and x-access lables. 
hist(rnorm(51, 6043834, 6823984),
     main = "Example of Normal Distribution",
     xlab = "Distribution with a Mean of 6,043,834 and standard deviation of 6,823,984")

