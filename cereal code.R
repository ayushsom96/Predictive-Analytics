#Read the data from the excel file

cereals_data  = read.csv(file.choose())

#Step1 - Data Cleaning

complete.cases(cereals_data) #chk for missing data


#Step2 - Exploratory

#Load the required library

library(data.table)
library(ggplot2)


summary(cereals_data)

#Exploratory Analysis using some visualization

#1. For categorical data like type, mfr use the bar plot
mfr_counts  = table(cereals_data$mfr)
mfr_viz = barplot(mfr_counts, main="MFR in Cereals", xlab= "mfr", ylab="freq")

type_counts = table(cereals_data$type)
type_viz = barplot(type_counts, main="Type in Cereals", xlab= "type", ylab="freq")

#For the remaining numerical data, we use the histograms

#Goal 1  - to understand the effect of calories and rating. Hence, draw corresponding histograms
rating_hist = hist(cereals_data$rating, col="green", xlab = "Rating", main="Rating Distribution")
calories_hist = hist(cereals_data$calories, col = "red", xlab = "calories", main= "Calories Distribution")

#Understanding the relation between rating and calories


n=c("calories","sugars","rating")
b=subset(cereals_data,select = n)
pairs(b)


#MOdel Training

model1<-lm(rating~calories+sodium+potass+carbo+sugars,data = cereals_data)
summary(model1)

#decisiontree
library(party)
cereal_ctree = ctree(rating~calories+sodium+fiber+carbo,data=cereals_data) 
plot(cereal_ctree)
print(cereal_ctree)
plot(cereal_ctree,type="simple")
