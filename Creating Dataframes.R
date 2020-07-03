#CREATING DATAFRAMES

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

#EXPLORING DATAFRAMES

#Examine the structure of the myFamily dataframe.
str(myFamily)

#Examine the summarized results of data contained in the myFamily dataframe.
summary(myFamily)

#Examine the first two rows of the myFamily dataframe.
head(myFamily, 2)

#Examine the first two rows of the myFamily dataframe.
tail(myFamily, 2)

#ACCESSING COLUMNS IN A DATAFRAME

#Display the myFamilyAges vector in the dataframe.
myFamily$myFamilyAges

#Add an additional age to the myFamilyAges vector.
myFamilyAges <- c(myFamilyAges, 11)

#View the modified vector.
myFamilyAges

#Verify that dataframe remains unchanged.  (Changing the vector after the dataframe is
#created does not change the dataframe.)
myFamily$myFamilyAges

#Verify that attempting to modify the length of a single vector within a dataframe
#will cause an error.  (The dataframe must be balanced.)
myFamily$myFamilyAges <- c(myFamily$myFamilyAges, 11)

