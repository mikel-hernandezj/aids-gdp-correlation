library(corrplot)
library(plyr)
library(dplyr)
library(ggplot2)

#Read datasets with the data for the moment
dataset <- read.csv("final_dataset.csv", header = TRUE)
dataset

#Summary of dataset
head(dataset)
tail(dataset)
sapply(dataset, class)
dim(dataset)

summary(dataset)
sapply(dataset[,5:13], skewness)
sapply(dataset[,5:13], sd)

library(corrplot)
#Correlations of numeric variables
num_cols <- unlist(lapply(dataset, is.numeric)) #identify numeric columns
correlations <- cor(dataset[,num_cols]) #get the correlations of the numeric columns
print(correlations) # display the correlation matrix
corrplot(correlations, method="circle") #create correlation plot

#histograms of each attributes
par(mfrow=c(2,5))
for(i in 5:13) {
  hist(dataset[,i], main=names(dataset)[i])
}

#density plots
par(mfrow=c(2,5))
for(i in 5:13) {
  plot(density(dataset[,i]), main=names(dataset)[i])
}

#boxplots
par(mfrow=c(2,5))
for(i in 5:13) {
  boxplot(dataset[,i], main=names(dataset)[i])
}

#scatterplots
pairs(dataset[,5:13])

#get countries we have in the data
unique(as.character(dataset$country_name))
#get countries of each continent
unique(select(filter(dataset, continent=='Europe'),country_name))
years <- c(2010,2011,2012,2013,2014,2015,2016,2017,2018)

#get data of a specific continent
dscontinent <- filter(dataset, continent == 'Oceania')

population <- vector(mode='numeric', length=9)
deathrate <- vector(mode='numeric', length=9)
gdp <- vector(mode='numeric', length=9)
active_HIV_cases <- vector(mode='numeric', length=9)
aids_deaths <- vector(mode='numeric', length=9)

for (i in 1:length(years)) {
  dsyear <- filter(dscontinent, year==years[i])
  population[i] <- mean(dsyear$population)
  deathrate[i] <- mean(dsyear$deathrate)
  gdp[i] <- mean(dsyear$gdp)
  active_HIV_cases[i] <- mean(dsyear$active_HIV_cases_children + dsyear$active_HIV_cases_female_adults + dsyear$active_HIV_cases_male_adults)
  aids_deaths[i] <- mean(dsyear$aids_deaths_children + dsyear$aids_deaths_female_adults + dsyear$aids_deaths_male_adults)
}

years <- as.character(years)
#Plot the population evolution over the years
d <- data.frame(years,population)
p1 <- ggplot(d, aes(x=years, y=population, group = 1)) + geom_point() + geom_line()

#Plot the deathrate evolution over the years
d <- data.frame(years,deathrate)
p2 <- ggplot(d, aes(x=years, y=deathrate, group=1)) + geom_point() + geom_line()

#Plot the gdp evolution over the years
d <- data.frame(years,gdp)
p3 <- ggplot(d, aes(x=years, y=gdp, group=1)) + geom_point() + geom_line()

#Plot the total active hiv cases evolution over the years
d <- data.frame(years,active_HIV_cases)
p4 <- ggplot(d, aes(x=years, y=active_HIV_cases, group=1)) + geom_point() + geom_line()

#Plot the total aids related deaths evolution over the years
d <- data.frame(years,aids_deaths)
p5 <- ggplot(d, aes(x=years, y=aids_deaths, group=1)) + geom_point() + geom_line()

#Show all plots
library(gridExtra)
grid.arrange(p1,p2,p3,p4,p5, nrow = 2)
