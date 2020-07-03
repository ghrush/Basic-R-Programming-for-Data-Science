#CREATING FUNCTIONS IN R

#Create a function that returns the given parameter/argument.
MyMode <- function(myVector)
{
  #Send back the function's results.
  return(myVector)
}


#Create a vector.
tinyData <- c(1, 2, 1, 2, 3, 3, 3, 4, 5, 4, 5)

#Display the values stored in the tinyData vector.
tinyData

#Make function call with the new vector as a parameter.
MyMode(tinyData)

#Enhance the function so that it returns the unique values
#identified in the supplied parameter/argument/vector.
MyMode <- function(myVector)
{
  #Obtain vector with duplicates removed.
  uniqueValues <- unique(myVector)
  
  #Send back the results.
  return(uniqueValues)
}

#Make function call with vector as a parameter.
MyMode(tinyData)

#Further enhance the function so that it returns a count 
#of the unique values identified in the supplied 
#parameter/argument/vector.
MyMode <- function(myVector)
{
  #Obtain vector with duplicates removed.
  uniqueValues <- unique(myVector)
  
  #Obtain a vector containing the number of times
  #each integer occurs.
  uniqueCounts <- tabulate(myVector)
  
  #Send back the results.
  return(uniqueCounts)
}

#Make function call with vector as a parameter.
MyMode(tinyData)

#Modify the function so that it returns a count 
#of the unique values identified in the supplied 
#parameter/argument/vector.
MyMode <- function(myVector)
{
  #Obtain vector with duplicates removed.
  uniqueValues <- unique(myVector)
  
  #Obtain a vector containing the number of times
  #each integer occurs.
  uniqueCounts <- tabulate(myVector)
  
  #Use 'which.max' function to determine the location/index 
  #of the  numeric value that occurs most frequently in the
  #uniqueCounts vector.  Then, send back the results.
  return(uniqueValues[which.max(uniqueCounts)])
}


#TESTING FUNCTIONS

#Display the values stored in the tinyData vector.
tinyData

#Call function  with the vector as a parameter.
MyMode(tinyData)

#Add additional values to the tinyData vector.
tinyData <- c(tinyData, 5, 5, 5)

#Display the values stored in the tinyData vector.
tinyData

#Call function  with the vector as a parameter.
MyMode(tinyData)

#Try to break the function by creating two modes:

#Add additional values to the tinyData vector
#so there are two modes.
tinyData <- c(tinyData, 1, 1, 1)

#Display the values stored in the tinyData vector.
tinyData

#Call function  with the vector as a parameter.
MyMode(tinyData) #The function fails. The which.max 
                 #function only returns the first 
                 #mode it finds.

#Add additional values to the tinyData vector
#so it skips over values contained in the vector.
tinyData <- c(tinyData, 9, 9, 9, 9, 9, 9, 9)

#Call function  with the vector as a parameter.
MyMode(tinyData) #The function fails. The tabulate 
#function adds zeros to cover values not contained in the
#vector between 5 and 9.  Thus, it creates a new mode and 
#the mode function failes.

#Further enhance the function so that it returns a count 
#of the unique values identified in the supplied 
#parameter/argument/vector.
MyMode <- function(myVector)
{
  #Obtain vector with duplicates removed.
  uniqueValues <- unique(myVector)
  
  #Obtain a vector containing the number of times
  #each integer occurs.  The match function eleminates the zeros
  #because there are no zero values in the original vector.
  uniqueCounts <- tabulate(match(myVector, uniqueValues))
  
  #Use 'which.max' function to determine the location/index 
  #of the  numeric value that occurs most frequently in the
  #uniqueCounts vector.  Then, send back the results.
  return(uniqueValues[which.max(uniqueCounts)])
}

#Call function  with the vector as a parameter.
MyMode(tinyData)

#INSTALLING A PACKAGE TO ACCESS A FUNCTION

#Use the mfv function from the modeest package to get the mode of the vector.
mfv(tinyData)

#Create a new vector with multiple modes.
multiData <- c(1, 5, 7, 7, 9, 9, 10)

#Compare the results from both functions.
mfv(multiData)
MyMode(multiData)





