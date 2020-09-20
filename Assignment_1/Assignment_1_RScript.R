#Data Mining Assignment 1, Dylan Buchheim

library(ggplot2)

#Data Sets
auto_mpg <-read.csv("Auto_mpg_raw.csv")
cereals <- read.csv("cereals.csv")

#Question 1 
#Identify each column in the data set as nominal, ordinal, interval, ratio.
# - miles.per.gallon, ratio
# - cylinders, ratio
# - displacement, ratio
# - horsepower, ratio
# - weight, ratio
# - acceleration, ratio
# - model.year, ordinal
# - origin, nominal
# - car.name, nominal

#-------------------------------------------------

#Question 2
#Histograms for every column
hist(auto_mpg$Miles.per.gallon, xlab = "Miles Per Gallon", main = "Histogram for Miles Per Gallon")
hist(auto_mpg$Cylinders, xlab = "Miles Per Gallon", main = "Histogram for Cylinders")
hist(auto_mpg$Displacement, xlab = "Miles Per Gallon", main = "Histogram for Displacement")
hist(auto_mpg$Horsepower, xlab = "Miles Per Gallon", main = "Histogram for Horsepower")
hist(auto_mpg$Weight, xlab = "Miles Per Gallon", main = "Histogram for Weight")
hist(auto_mpg$Acceleration, xlab = "Miles Per Gallon", main = "Histogram for Acceleration")
hist(auto_mpg$Model.year, xlab = "Miles Per Gallon", main = "Histogram for Year")
hist(auto_mpg$Origin, xlab = "Miles Per Gallon", main = "Histogram for Origin")

#There seems to be a sentinal value of 1000 that represents missing data in the miles.per.gallon histogram.
#I will set the misleading value to NA and generate a new histogram.
auto_mpg$Miles.per.gallon <- ifelse(test = auto_mpg$Miles.per.gallon == 1000, yes= NA, no= auto_mpg$Miles.per.gallon)
hist(auto_mpg$Miles.per.gallon, xlab = "Miles Per Gallon", main = "Histogram for Miles Per Gallon Adjusted")

#There are a few misleading values in the horsepower column, there are zeros instead of NA.
#I will set the misleading value to NA and generate a new histogram.
auto_mpg$Horsepower <- ifelse(test = auto_mpg$Horsepower == 0, yes= NA, no= auto_mpg$Horsepower)
hist(auto_mpg$Horsepower, xlab = "Horsepower", main = "Histogram for Horsepower Adjusted")

#Writing the updated table to a csv
write.csv(auto_mpg, file = "Auto_mpg_adjust.csv", row.names = FALSE)

#-------------------------------------------------

#Question 3
#Using Z-Values to identify outliers in the adjusted data.
auto_mpg_adjust <- read.csv("Auto_mpg_adjust.csv")
auto_mpg_adjust$Miles.per.gallon.z <- scale(x=auto_mpg_adjust$Miles.per.gallon)

mpg_outliers <- auto_mpg_adjust[which(auto_mpg_adjust$Miles.per.gallon.z > 3 | auto_mpg_adjust$Miles.per.gallon.z < -3),]
print(mpg_outliers)
#There are no outliers in the Miles Per Gallon Attribute.

#-------------------------------------------------

#Question 4
#Part a, a bar graph and normalized bar graph of Manuf with a Type overlay.
ggplot(cereals, aes(Manuf)) + geom_bar(aes(fill=Type)) + ggtitle("Bar Graph of Manufacturers with Type Overlay")
ggplot(cereals, aes(Manuf)) + geom_bar(aes(fill=Type), position="fill") + ggtitle("Normalized Bar Graph of Manufacturers with Type Overlay")

#Part b, contingency tables of Manuf and Type
ContingencyTable.v1 <- table(cereals$Manuf,cereals$Type)
ContingencyTable.v2 <- addmargins(A=ContingencyTable.v1,FUN= list(total=sum), quiet= TRUE)
ContingencyTable.rnd <- round(prop.table(ContingencyTable.v1,margin = 2)*100, 1)
print(ContingencyTable.v1)
print(ContingencyTable.v2)
print(ContingencyTable.rnd)

#Part c, histograms of calories with manuf overlay
ggplot(cereals, aes(Calories)) + geom_histogram(aes(fill=Manuf), binwidth = 10)
ggplot(cereals, aes(Calories)) + geom_histogram(aes(fill=Manuf), position="fill", binwidth = 5)

#Part d, binned calories  bar graph.
cereals$Calories_binned <- cut(x=cereals$Calories,breaks=c(0,90.1,110.1,200),right=FALSE, labels=c("0 to 90","91 to 110","Over 110"))
ggplot(cereals, aes(Calories_binned)) + geom_bar(aes(fill=Manuf))


