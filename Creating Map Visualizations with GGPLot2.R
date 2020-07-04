#CREATING MAP VISUALIZATIONS WITH GGPLOT2

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

###########################################################################

#Down load the specified packages, if  not already downloaded.
EnsurePackage("ggmap")
EnsurePackage("maps")
EnsurePackage("mapproj")
EnsurePackage("curl")

#Get a map of the U.S. with ggmap.
us <- map_data("state")

#Create dataframe to hold state names.
dummyDF <- data.frame(state.name, stringsAsFactors = FALSE)

#Create a second column in the dataframe for state names
#in lower case.  (ggplot requires names in lower case.)
dummyDF$state <- tolower(dummyDF$state.name)

#Create a map of the U.S. with a white background outlined in black.
#Expand limits based on longitude and latitude values.
#Prevent distortion.
map.simple <- ggplot(dummyDF, aes(map_id = state))
map.simple <- map.simple + geom_map(map = us, fill= "white", color = "black")
map.simple <- map.simple + expand_limits(x = us$long, y = us$lat)
map.simple <- map.simple + coord_map() + ggtitle("Basic map of continental USA")
map.simple

#Retrieve the census data.
dfStates <- readCensus()

#Add column to the dataframe that contains lower case state names.
dfStates$state <- tolower(dfStates$stateName)

#Create a map that will fill color based on July 2011 population.
map.popColor <- ggplot(dfStates, aes(map_id = state))
map.popColor <- map.popColor + geom_map(map = us, aes(fill = july11pop))
map.popColor <- map.popColor + expand_limits(x = us$long, y = us$lat)
map.popColor <- map.popColor + coord_map() + ggtitle("state population")
map.popColor

#SHOWING POINTS ON A MAP

#Hard code a specific point/location (in Texas) on the map.
map.simple + geom_point(aes(x = -100, y = 30))


#Set your Google API key.
register_google(key = "YourAPIKey")

#Verify API key exists.
has_google_key()

#US a point on the map using a logical location 
#(i.e., the address not the coordinates).
latlon <- geocode("syracuse, ny")

#View latitude and longitude data.
latlon

#Use the latitude and longitude data to draw a point on the map.
map.popColor + geom_point(aes(x = latlon$lon, y = latlon$lat), 
                          color = "darkred", size = 3)

#Add 2 additional points to the map.
df.latlon <- data.frame(latlon) #Save 1st location
latlon <- geocode("colorado") #Get 2nd location.
df.latlon[2,] <- latlon #Save 2nd location.
df.latlon[3,] <- geocode("denver, colorado") #Get 3rd location.
df.latlon$state <- "?"  #Add dummy column because R requires it.
map.simple + geom_point(data = df.latlon, aes(x = lon, y = lat),
                        alpha = .5, color = "darkred", size = 3)

#A MAP VISUALIZATION EXAMPLE
EnsurePackage("RColorBrewer")

#Capture U.S. company data in a dataframe.
urlFile = "http://www.opendata500.com/us/download/us_companies.csv"
od.companies <- read.csv(url(urlFile))

#Examine the newly created dataframe.
str(od.companies)

#Clean up data:
#Remove columns that do not contain city data.
od.companies <- od.companies[od.companies$city != "",] 

#Change the type of state names to character. 
od.companies$state <- as.character(od.companies$state)

#Remove D.C from data set.
od.companies <- od.companies[od.companies$state != "DC",] 

#Change states mislabeled as KA to KS.
od.companies$state <- ifelse(od.companies$state == "KA", "KS", od.companies$state)

#Create a new colmn composed of the concatenated city and state. 
od.companies$cityState <- paste(od.companies$city, od.companies$state)

#Use the new column to obtain the coordinates for each
#location.  Then, save the results in a new column.
od.companies$geoCode <- geocode(od.companies$cityState)

#Show locations on the map.
map.simple + geom_point(data = od.companies, 
                        aes(x = geoCode$lon, y = geoCode$lat))


#Find and display the outlier that skewed the map 
#with the following code:
bad <- od.companies[od.companies$geoCode$lon > 0,]
bad$cityState

#Remove the identified record from the dataset.
od.companies <- od.companies[od.companies$geoCode$lon < 0, ]
map.simple + geom_point(data = od.companies,
                        aes(x = geoCode$lon, y = geoCode$lat), shape = 1)

#Map companies with smaller number of employees to lighter circles.
od.companies$sizes <- factor(od.companies$full_time_employees, 
                             levels = c("1-10", "11-50", "51-200", "201-500",
                             "501-1,000", "1,001-5,000", "5,001-10,000", "10,001+"))
myColors <- brewer.pal(9, "Reds")
names(myColors) <- levels(od.companies$sizes)
myColors[1:3]

map.popColor + geom_point(data = od.companies,
                          aes(x = geoCode$lon, y =geoCode$lat,
                              color = sizes)) + 
  scale_colour_manual(name = "sizeOfCompany", values = myColors) +
  ggtitle("Open Data Company Analysis")



