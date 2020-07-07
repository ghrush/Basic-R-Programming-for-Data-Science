#CAR MAINTENANCE

#Define vectors.
oilChanges <- c(3, 5, 2, 3, 1, 4, 6, 4, 3, 2, 0, 
                10, 7, 8) #Oil changes over 3 years.
                          #An independent variable.
repairs <- c(300, 300, 500, 400, 700, 420, 100, 
             290, 475, 620, 600, 0, 200, 50)  #Total cost of repairs. 
                                              #The dependent variable.
miles <- c(20100, 23200, 19200, 22100, 18400, 
           23400, 17900, 19900, 20100, 24100, 
           18200, 19600, 20800, 19700) #Miles driven by each car.
                                        #An independent variable.

#Combine vectors into a dataframe.
oil <- data.frame(oilChanges, repairs, miles)

#Examine the new dataframe.
View(oil)


#Before building a model, do some exploratory analysis.  Look for general 
#trends between each of the dependent variables and the independent variable.

#Plot oil changes -vs- repairs.
plot(oil$oilChanges, oil$repairs)

#Plot miles -vs- repairs.
plot(oil$miles, oil$repairs)

#Create model based on pattern relationship identified when comparing
#oil changes -vs- repairs.
model1 <- lm(formula = repairs ~ oilChanges, data = oil)

#Examine the model.
summary(model1)

#Plot oil changes -vs- repairs with a trend line.
plot(oil$oilChanges, oil$repairs)
abline(model1)

#Create model using oil changes and miles to
#predict repairs (multiple linear regression). Is it a better predictor?
m <- lm(formula = repairs ~ oilChanges + miles, data = oil)

#Examine the model.
summary(m)


#Add another column to the data set that shows the cost of oil changes.
oil$oilChangeCost <- oil$oilChanges * 350

#Add another column to the data set that shows the total cost per car.
#(Each row in the data set represents an individual vehicle.)
oil$totalCost <- oil$oilChangeCost + oil$repairs

#Create model based to predict total costs based on oil changes.
m <- lm(formula = totalCost ~ oilChanges, data = oil)

#Plot oil changes -vs- repairs with a trend line.
plot(oil$oilChanges, oil$totalCost)
abline(m)

#Calculate the total repair costs for zero (0) oil changes.
test = data.frame(oilChanges = 0)
predict(m, test, type= "response")

#Calculate the total repair costs for zero (0) oil changes.
test = data.frame(oilChanges = 5)
predict(m, test, type= "response")

#Calculate the total repair costs for zero (0) oil changes.
test = data.frame(oilChanges = 10)
predict(m, test, type= "response")

#Plot the relationship using ggplot2
ggplot(oil, aes(x = oilChanges, y = totalCost)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")
