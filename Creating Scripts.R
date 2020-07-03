#CREATING R SCRIPTS

#Create a new vector.
tinyData <- c(1, 2, 1, 2, 3, 3, 3, 4, 5, 4, 5)

#Calculate the minimum and average values contained 
#in the vector. Then, calculate the sum of the values.
min(tinyData)
mean(tinyData)
sum(tinyData)

#Create a new vector based on the previous vector.  
#Add 5 to each value currently in the vector.
biggerData <- tinyData + 5

#Create a dataframe based on the two previous vectors.
df <- data.frame(tinyData, biggerData)

#Change the default names of the columns for the 
#new dataframe.  The defualt names will be the vector names.
colnames(df) <- c("small", "big")
