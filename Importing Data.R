#ACCESSING EXCEL DATA

#Import the general-purpose data manipulation 
#package for importing spreadsheet files.
install.packages("gdata")
library("gdata")

#Read U.S. Census data from an Excel spreadsheet stored 
#online at the U.S. census website and store the data
#within a dataframe. <<- Does not work.  Down load 1st.
#testFrame <- read.xls("https://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.xls",
#                       sheet = 1, verbose = TRUE) #Will contain errors.

#Identify location of census data.
targetFile <- "https://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.xls"

#Download the file to a temporary directory.
download.file(targetFile, 
              destfile="nst-est2011-01.xls", method="curl")

#Read the file from the temporary directory.
testFrame <- read.xls("nst-est2011-01.xls", sheet = 1, verbose = TRUE) #Data is not clean.

#Examine the structure of the newly created dataframe.
str(testFrame)

#Remove the first eight rows/observations from the dataframe.
testFrame <- testFrame[-1:-8,]

#Remove the last five columns by saving the first four columns. (The previous 
#line revealed that the last 5 columns contains 'NA' values.)
testFrame <- testFrame[,1:5]

#Remove the last five rows/observations from the dataframe.
testFrame <- testFrame[-52:-57,]

#Create a new column and fill the column with the data from the first column. 
#(Create a copy of column 1 with a new name.)
testFrame$stateName <- testFrame[,1]

#Remove the original first column.
testFrame <-testFrame[,-1]

#Remove all periods from state names within the stateName column.
testFrame$stateName <- gsub("\\.", "", testFrame$stateName)

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
###############################################################

#Remove any spaces that may exist in each of the new columns.  
#And, convert the cell's type to numeric.
testFrame$april10census <- Numberize(testFrame$X)
testFrame$april10base <- Numberize(testFrame$X.1)
testFrame$july10pop <- Numberize(testFrame$X.2)
testFrame$july11pop <- Numberize(testFrame$X.3)

#Remove the original (4) columns from the dataframe.
testFrame <- testFrame[, -1:-4]

#Remove/reset the row names/numbering for the dataframe.
rownames(testFrame) <- NULL

#ACCESSING A DATABASE

#Establish a linkk / connection between R and the database.
con <- dbConnect(dbDriver("MySQL"), username = "root", password = "0neN@ut1c@$h1rt", dbname = "test")

#List the database tables in the database that are accessible.
dbListTables(con)

#Create a database table using the data in the dataframe created earlier.
dbWriteTable(con, "census", testFrame, overwrite = TRUE)

#List the database tables in the database are accessible.
#The newly created database table should appear in the list.
dbListTables(con)

#Return a list of states (along with their populations) that have a population
#less than a million.
dbGetQuery(con, "SELECT stateName, july11pop FROM census WHERE july11pop < 1000000")

#COMPARING SQL AND R FOR ACCESSING A DATA SET

#Run a SQL function on the dataframe created earlier
#that calculates the average population.
sqldf("SELECT avg(april10base) FROM testFrame")

#Calculate the average population using the mean function
#in order to compare the results.
mean(testFrame$april10base)

#Return a list of states (along with their populations) that have a population
#less than a million.
sqldf("SELECT statename FROM testFrame WHERE july11pop < 1000000")

#Remove D.C. from the list of states.
testFrame <- testFrame[testFrame$stateName != "District of Columbia",]

#Remove/reset the row names/numbering for the dataframe.
rownames(testFrame) <- NULL

#Add a column/attribute for regions.
testFrame$region <- state.region

#Determine the population for each region using SQL.
sqldf("SELECT AVG(july11pop) FROM testFrame GROUP BY region")

#Determine the population for each region using R function.
tapply(testFrame$april10base, testFrame$region, mean)

#Store the region mean for each state.
regionMean <- tapply(testFrame$april10base, testFrame$region, mean) #Store Results
regionNames <- names(regionMean) #Store the region names.

#Test one method of getting the indexes of the states within the "Northeast" region.
which(regionNames[1] == testFrame$region)

#Test a secound method of getting the indexes of the states within the "Northeast" region.
which(regionNames[regionNames == "Northeast"]  == testFrame$region)

#Set the region mean population values for each state
#using one method.
testFrame$regionMean <- 0 #Set initial region mean population.
testFrame$regionMean[which(regionNames[1] == testFrame$region)] <- regionMean[1]
testFrame$regionMean[which(regionNames[2] == testFrame$region)] <- regionMean[2]
testFrame$regionMean[which(regionNames[3] == testFrame$region)] <- regionMean[3]
testFrame$regionMean[which(regionNames[4] == testFrame$region)] <- regionMean[4]

#Set the region mean population values for each state
#using another method (loop).
for(x in 1:4){
  indexes <- which(regionNames[x] == testFrame$region)
  testFrame$regionMean[indexes] <- regionMean[x]
}

#ACCESSING JSON DATA

#Import the general-purpose data manipulation 
#package for importing spreadsheet files.
install.packages("RCurl")
install.packages("RJSONIO")
library("RCurl")
library("RJSONIO")

#This helper function takes the supplied address and turns it into an URL.
MakeGeoURL <- function(address)
{
  #Initialize the beginning part of the URL into a string.
  root <- "http://maps.google.com/maps/api/geocode/"
  
  #Glue together the separate parts of the string.
  url <- paste(root, "json?address=", address, "sensor=false", sep = "")
  
  #Convert the string to a URL before returning to calling function/user.
  return(URLencode(url))
}


#This function requests data from the Google API.
addr2latlng <- function(address)
{
  #Develop the formatted URL.
  url <- MakeGeoURL(address)
  
  #Send request to Google.
  apiResult <- getURL(url)
  
  #Convert response from JSON to a R object.
  geoStruct <- fromJSON(apiResult, simplify = FALSE)
  lat <- NA
  lng <- NA
  
  try(lat <- geoStruct$results[[1]]$geometry$location$lat)
  try(lng <- geoStruct$results[[1]]$geometry$location$lng)
  return(c(lat, lng))
}


testData <- addr2latlng("1600 Pennsylvania Avenue, Washington, DC")
str(testData)


#Read a large JSON data set from Citi Bike.
bikeURL <- "https://feeds.citibikenyc.com/stations/stations.json"
apiResult <- getURL(bikeURL)
results <- fromJSON(apiResult)
length(results)

#View first item in the list returned from Citi Bike...time stamp.
when <- results[[1]]

#View second item in the list returned from Citi Bike...list of stations.
stations <- results[[2]]

#View type of information available for each station.
str(stations[[1]])


#Get the total number of rows in the list of stations.
numRows <- length(stations)

#Get the name of all the attributes at each station.
nameList <- names(stations[[1]])

#Create a dataframe from the data.
dfStations <- data.frame(matrix(unlist(stations), nrow= numRows, byrow = T), stringsAsFactors = FALSE)

#Provide appropriate names for each column.
names(dfStations) <- nameList

#Fix mis-typed columns. Convert from character to numeric.
dfStations$availableDocks <- as.numeric(dfStations$availableDocks)
dfStations$availableBikes <- as.numeric(dfStations$availableBikes)
dfStations$totalDocks <- as.numeric(dfStations$totalDocks)
