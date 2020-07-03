#BASIC PLOTS IN R

###########Recreate Functions from Chapter 9###################

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
dfStates <- readCensus()

#Show histogram depicting state population.
hist(dfStates$july11pop)

#Show bar plot/chart depicting state population.
barplot(dfStates$july11pop, names.arg = dfStates$stateName, las=2)

#USING GGPLOT2

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


#Down load package the specified package, if it is not already downloaded.
EnsurePackage("ggplot2")

#Display a ggplot2 histogram of the population data.
g <- ggplot(dfStates, aes(x = july11pop)) #define the data and set the 'x' axis.
g <- g + geom_histogram(binwidth = 5000000, color = "black", fill = "white") #define visualization and aesthetics
g <- g + ggtitle("states population histogram") #Define Annotations
g

#Display a ggplot2 boxplot of the population data.
ggplot(dfStates, aes(x = factor(0), july11pop)) +
  geom_boxplot()

#Calculate the annual population change for each state.
dfStates$popChange <- dfStates$july11pop - dfStates$july10pop

#Create a new column in the dataframe that denotes whether the annual 
#population change was positive or negative.
dfStates$increasePop <- ifelse(dfStates$popChange < 0, "positive", "negative")

#Display a ggplot2 boxplot of the population data.  Seperate 
#states with negative and positive annual population growth.
g <- ggplot(dfStates, aes(x = factor(increasePop), july11pop))
g <- g + geom_boxplot() + coord_flip()
g <- g+ ggtitle('Population grouped by positive or negative change')
g

#Display a ggplot2 line chart of the population data.
g <- ggplot(dfStates, aes(x = reorder(stateName, july11pop), y = july11pop, group =1))
g <- g + geom_line()
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g

#Display a ggplot2 bar chart of the population data.
g <- ggplot(dfStates, aes(x = reorder(stateName, july11pop), y = july11pop, group =1))
g <- g + geom_col()
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g


#MORE ADVANCED GGPLOT2 VISUALIZATIONS

#Create a bar chart where bar color represents the percent change in population.
dfStates$percentChange <- dfStates$popChange / dfStates$july10pop * 100
g <- ggplot(dfStates, aes(x = reorder(stateName, july11pop), y = july11pop, fill = percentChange))
g <- g + geom_col()
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g

#Create a scatter plot where each point represents a state.
g <- ggplot(dfStates, aes(x = popChange, y = percentChange))
g <- g + geom_point(aes(size = july11pop, color = july11pop))
g

#Add state names to plot.
g + geom_text(aes(label = stateName), size = 4)

#Re-position of state names to increase readability.
g + geom_text(aes(label = stateName), size = 4, hjust = 1, vjust = 1)

#Create a scatter plot where each point represents a state.
#Only display states with a percentage change of at least 1%
#and a population of at least 100K.
minPerChage <- 1
minPopChange <- 100000
dfStates$keystate <- dfStates$popChange > minPopChange & dfStates$percentChange > minPerChage
minLabel <- format(min(dfStates$july11pop), big.mark = ",", trim = TRUE)
maxLabel <- format(max(dfStates$july11pop), big.mark = ",", trim = TRUE)
medianLabel <- format(median(dfStates$july11pop), big.mark = ",", trim = TRUE)

g <- ggplot(dfStates, aes(x = popChange, y = percentChange))
g <- g + geom_point(aes(size = july11pop, color = july11pop, shape = keystate))
g <- g + geom_text(data = dfStates[dfStates$popChange > minPopChange & dfStates$percentChange > minPerChage,],
                   aes(label = stateName, hjust =1, vjust = -1))
g + scale_color_continuous(name = "Pop",
                           breaks = with(dfStates,
                                         c(min(july11pop), median(july11pop),
                                           max(july11pop))),
                           labels = c(minLabel, medianLabel, maxLabel),
                           low = "white", high = "black")


