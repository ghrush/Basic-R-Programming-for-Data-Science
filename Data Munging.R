#READING A CSV TEXT FILE

#Identify location of census data.
urlToRead <- "https://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"

#Copy census data from census website into a dataframe.
testFrame <- read.csv(url(urlToRead)) #Will contain errors.

#Examine the structure of the newly created dataframe.
str(testFrame)

#REMOVING ROWS AND COLUMNS

#Remove the first eight rows/observations from the dataframe.
testFrame <- testFrame[-1:-8,]

#Examine the summarized results of data contained in the testFrame dataframe.
summary(testFrame)

#Remove the last five columns by saving the first four columns. (The previous 
#line revealed that the last 5 columns contains 'NA' values.)
testFrame <- testFrame[,1:5]

#Examine the last five rows of the dataframe.
tail(testFrame, 5)

#Remove the last five rows/observations from the dataframe.
testFrame <- testFrame[-52:-58,]

#RENAMING ROWS AND COLUMNS

#Create a new column and fill the column with the data from the first column. 
#(Create a copy of column 1 with a new name.)
testFrame$stateName <- testFrame[,1]

#Examine the column names of the dataframe.
colnames(testFrame)


cnames <- colnames(testFrame) #Get a copy of the column names.
cnames[1] <- "newName" #Change a name of the 1st column.
cnames #Verify the change.

colnames(testFrame) <- cnames #Replace all of the column names.
colnames(testFrame) #Display the dataframes' (newly replaced) column names.

#Remove the newly created first column from the dataframe.
testFrame <- testFrame[,-1]

#CLEANING UP THE ELEMENTS

#Remove all periods from state names within the stateName column.
testFrame$stateName <- gsub("\\.", "", testFrame$stateName)


#Create new columns based on the existing columns containing population data.
#During the creation process, remove the commas from the values being copied.  
#(Replace commas with blanks.)  Note: the new column's type will be character. 
testFrame$april10census <- gsub(",", "", testFrame$X)
testFrame$april10base <- gsub(",", "", testFrame$X.1)
testFrame$july10pop <- gsub(",", "", testFrame$X.2)
testFrame$july11pop <- gsub(",", "", testFrame$X.3)

#Remove any spaces that may exist in each of the new columns.  
#And, convert the cell's type to numeric.
testFrame$april10census <- as.numeric(gsub(" ", "", testFrame$april10census))
testFrame$april10base <- as.numeric(gsub(" ", "", testFrame$april10base)) 
testFrame$july10pop <- as.numeric(gsub(" ", "", testFrame$july10pop)) 
testFrame$july11pop <- as.numeric(gsub(" ", "", testFrame$july11pop)) 

#Remove the (4) columns from the dataframe.
testFrame <- testFrame[, -1:-4]

#Verify the data types for the census data are numeric by 
#examining the structure of the dataframe.
str(testFrame)

#Examine the first five rows of the dataframe.
head(testFrame, 5)

#Remove/reset the row names/numbering initially created by the read.csv function.
rownames(testFrame) <- NULL

#Verify that the row names/numbers were rest.
head(testFrame, 5)

#SORTING DATAFRAMES

#Create a new dataframe containing the census information
#with rows sorted based on the July population value.
sortedStates <- testFrame[order(testFrame$july11pop),]

#Examine the first five rows of the sorted dataframe.
head(sortedStates, 5)


#Resort the list...placing the largest population first.
sortedStates <- testFrame[order(-testFrame$july11pop),]
