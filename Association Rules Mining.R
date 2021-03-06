#ASSOCIATION RULES MINING

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

###########################################################################

#Include the association rules mining package.
EnsurePackage("arules")

#Get the groceries data set.
data("Groceries")

#Examine the data set
summary(Groceries)

#Produce a bar graph that shows the relative frequency of occurrence 
#of different items in the matrix.
itemFrequencyPlot(Groceries, support = 0.1)


#Show items with lower support levels and adjust the size of the labels.
itemFrequencyPlot(Groceries, support = 0.05, cex.names = 0.5)


#Starting with a different data set....

#Get the census income data set.
data(AdultUCI)

#Copy the data set so as not to compromise its integrity.
AdultUCI.t <- AdultUCI


#Modify the data set by changing integer values in the 
#data set to factors.
AdultUCI.t$age <- as.factor(AdultUCI.t$age)
AdultUCI.t$fnlwgt <- as.factor(AdultUCI.t$fnlwgt)
AdultUCI.t$'education-num' <- as.factor(AdultUCI.t$'education-num')
AdultUCI.t$'capital-gain' <- as.factor(AdultUCI.t$'capital-gain')
AdultUCI.t$'capital-loss' <- as.factor(AdultUCI.t$'capital-loss')
AdultUCI.t$'hours-per-week' <- as.factor(AdultUCI.t$'hours-per-week')

#Convert the data frame into a transaction data set.
AdultUCI.trans <- as(AdultUCI.t, "transactions")

#Produce a bar graph that shows the relative frequency of occurrence 
#of different items in the matrix.
itemFrequencyPlot(AdultUCI.trans, support = 0.2, cex.names = 1.1)

#EXPLORING HOW THE ASSOCIATION RULES ALGORITHM WORKS.

#Save ruleset generated by apriori.
ruleset <- apriori(Groceries, parameter = list(support = 0.01, confidence = 0.5))


#Review summary of the results.
summary(ruleset)

#Get detailed contents of the ruleset.
inspect(ruleset)

#Include package to help visualize apriori results.
EnsurePackage("arulesViz")

#Plot rule identified earlier.
plot(ruleset)

#Create ruleset based on a lower support and confidence level.
ruleset <- apriori(Groceries, parameter = list(support = 0.005, confidence = 0.35))

#Plot rule identified earlier.
plot(ruleset)


#Make a subset of the larger set of rule...choosing only those rules that have a lift higher than 3.5.
goodrules <- ruleset[quality(ruleset)$lift > 3.5]

#Get detailed contents of the subset ruleset.
inspect(goodrules)
