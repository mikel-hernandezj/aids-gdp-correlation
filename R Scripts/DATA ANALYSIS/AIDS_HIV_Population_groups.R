library(plyr)

#Read the dataset with the information for analysis
dataset <- read.csv("final_dataset.csv", header = TRUE)
dataset

summary(dataset)

#Save the data of the 2010 for each country
ds2010 <- dataset[c(1:217),c(1:13)]

#Save the data of the 2011 for each country
ds2011 <- dataset[c(218:434),c(1:13)]

#Save the data of the 2012 for each country
ds2012 <- dataset[c(435:651),c(1:13)]

#Save the data of the 2013 for each country
ds2013 <- dataset[c(652:868),c(1:13)]

#Save the data of the 2014 for each country
ds2014 <- dataset[c(869:1085),c(1:13)]

#Save the data of the 2015 for each country
ds2015 <- dataset[c(1086:1302),c(1:13)]

#Save the data of the 2016 for each country
ds2016 <- dataset[c(1303:1519),c(1:13)]

#Save the data of the 2017 for each country 
ds2017 <- dataset[c(1520:1736),c(1:13)]

#Save the data of the 2018 for each country
ds2018 <- dataset[c(1737:1953),c(1:13)]


# Create a 9-row, 10-column matrix with the data of all the world
headings <- list(NULL, c("year","population","deathrate","gdp","active_HIV_cases_children","active_HIV_cases_male_adults","active_HIV_cases_female_adults","aids_deaths_children","aids_deaths_male_adults","aids_deaths_female_adults"))
dsglobal <- matrix(nrow=9, ncol=10, byrow=TRUE, dimnames=headings)

dsglobal[1,1] <- "2010"
dsglobal[2,1] <- "2011"
dsglobal[3,1] <- "2012"
dsglobal[4,1] <- "2013"
dsglobal[5,1] <- "2014"
dsglobal[6,1] <- "2015"
dsglobal[7,1] <- "2016"
dsglobal[8,1] <- "2017"
dsglobal[9,1] <- "2018"

dsglobal[1,2] <- mean(ds2010$population)
dsglobal[2,2] <- mean(ds2011$population)
dsglobal[3,2] <- mean(ds2012$population)
dsglobal[4,2] <- mean(ds2013$population)
dsglobal[5,2] <- mean(ds2014$population)
dsglobal[6,2] <- mean(ds2015$population)
dsglobal[7,2] <- mean(ds2016$population)
dsglobal[8,2] <- mean(ds2017$population)
dsglobal[9,2] <- mean(ds2018$population)

dsglobal[1,3] <- mean(ds2010$deathrate)
dsglobal[2,3] <- mean(ds2011$deathrate)
dsglobal[3,3] <- mean(ds2012$deathrate)
dsglobal[4,3] <- mean(ds2013$deathrate)
dsglobal[5,3] <- mean(ds2014$deathrate)
dsglobal[6,3] <- mean(ds2015$deathrate)
dsglobal[7,3] <- mean(ds2016$deathrate)
dsglobal[8,3] <- mean(ds2017$deathrate)
dsglobal[9,3] <- mean(ds2018$deathrate)

dsglobal[1,4] <- mean(ds2010$gdp)
dsglobal[2,4] <- mean(ds2011$gdp)
dsglobal[3,4] <- mean(ds2012$gdp)
dsglobal[4,4] <- mean(ds2013$gdp)
dsglobal[5,4] <- mean(ds2014$gdp)
dsglobal[6,4] <- mean(ds2015$gdp)
dsglobal[7,4] <- mean(ds2016$gdp)
dsglobal[8,4] <- mean(ds2017$gdp)
dsglobal[9,4] <- mean(ds2018$gdp)

dsglobal[1,5] <- mean(ds2010$active_HIV_cases_children)
dsglobal[2,5] <- mean(ds2011$active_HIV_cases_children)
dsglobal[3,5] <- mean(ds2012$active_HIV_cases_children)
dsglobal[4,5] <- mean(ds2013$active_HIV_cases_children)
dsglobal[5,5] <- mean(ds2014$active_HIV_cases_children)
dsglobal[6,5] <- mean(ds2015$active_HIV_cases_children)
dsglobal[7,5] <- mean(ds2016$active_HIV_cases_children)
dsglobal[8,5] <- mean(ds2017$active_HIV_cases_children)
dsglobal[9,5] <- mean(ds2018$active_HIV_cases_children)

dsglobal[1,6] <- mean(ds2010$active_HIV_cases_male_adults)
dsglobal[2,6] <- mean(ds2011$active_HIV_cases_male_adults)
dsglobal[3,6] <- mean(ds2012$active_HIV_cases_male_adults)
dsglobal[4,6] <- mean(ds2013$active_HIV_cases_male_adults)
dsglobal[5,6] <- mean(ds2014$active_HIV_cases_male_adults)
dsglobal[6,6] <- mean(ds2015$active_HIV_cases_male_adults)
dsglobal[7,6] <- mean(ds2016$active_HIV_cases_male_adults)
dsglobal[8,6] <- mean(ds2017$active_HIV_cases_male_adults)
dsglobal[9,6] <- mean(ds2018$active_HIV_cases_male_adults)

dsglobal[1,7] <- mean(ds2010$active_HIV_cases_female_adults)
dsglobal[2,7] <- mean(ds2011$active_HIV_cases_female_adults)
dsglobal[3,7] <- mean(ds2012$active_HIV_cases_female_adults)
dsglobal[4,7] <- mean(ds2013$active_HIV_cases_female_adults)
dsglobal[5,7] <- mean(ds2014$active_HIV_cases_female_adults)
dsglobal[6,7] <- mean(ds2015$active_HIV_cases_female_adults)
dsglobal[7,7] <- mean(ds2016$active_HIV_cases_female_adults)
dsglobal[8,7] <- mean(ds2017$active_HIV_cases_female_adults)
dsglobal[9,7] <- mean(ds2018$active_HIV_cases_female_adults)

dsglobal[1,8] <- mean(ds2010$aids_deaths_children)
dsglobal[2,8] <- mean(ds2011$aids_deaths_children)
dsglobal[3,8] <- mean(ds2012$aids_deaths_children)
dsglobal[4,8] <- mean(ds2013$aids_deaths_children)
dsglobal[5,8] <- mean(ds2014$aids_deaths_children)
dsglobal[6,8] <- mean(ds2015$aids_deaths_children)
dsglobal[7,8] <- mean(ds2016$aids_deaths_children)
dsglobal[8,8] <- mean(ds2017$aids_deaths_children)
dsglobal[9,8] <- mean(ds2018$aids_deaths_children)

dsglobal[1,9] <- mean(ds2010$aids_deaths_male_adults)
dsglobal[2,9] <- mean(ds2011$aids_deaths_male_adults)
dsglobal[3,9] <- mean(ds2012$aids_deaths_male_adults)
dsglobal[4,9] <- mean(ds2013$aids_deaths_male_adults)
dsglobal[5,9] <- mean(ds2014$aids_deaths_male_adults)
dsglobal[6,9] <- mean(ds2015$aids_deaths_male_adults)
dsglobal[7,9] <- mean(ds2016$aids_deaths_male_adults)
dsglobal[8,9] <- mean(ds2017$aids_deaths_male_adults)
dsglobal[9,9] <- mean(ds2018$aids_deaths_male_adults)

dsglobal[1,10] <- mean(ds2010$aids_deaths_female_adults)
dsglobal[2,10] <- mean(ds2011$aids_deaths_female_adults)
dsglobal[3,10] <- mean(ds2012$aids_deaths_female_adults)
dsglobal[4,10] <- mean(ds2013$aids_deaths_female_adults)
dsglobal[5,10] <- mean(ds2014$aids_deaths_female_adults)
dsglobal[6,10] <- mean(ds2015$aids_deaths_female_adults)
dsglobal[7,10] <- mean(ds2016$aids_deaths_female_adults)
dsglobal[8,10] <- mean(ds2017$aids_deaths_female_adults)
dsglobal[9,10] <- mean(ds2018$aids_deaths_female_adults)

dsglobal <- data.frame(dsglobal)

convert_to_numeric <- function(values) {
  #Convert the values to characters (there are factors)
  values <- as.character(values)
 
  converted <- as.numeric(values)
}

d <- dim(dsglobal)
for (i in 2:d[2]) {
  dsglobal[,i] = convert_to_numeric(dsglobal[,i])
}
dsglobal

#ANOVA test between the population groups and deaths
library(ggplot2)
 
headings2 <- list(NULL, c("deaths","groups"))
group_deaths <- matrix(nrow=5859, ncol=2, byrow=TRUE, dimnames=headings2)

group_deaths[1:1953,1]<-dataset$aids_deaths_children
group_deaths[1:1953,2]<-"aids_deaths_children"
group_deaths[1954:3906,1]<-dataset$aids_deaths_male_adults
group_deaths[1956:3906,2]<-"aids_deaths_male_adults"
group_deaths[3907:5859,1]<-dataset$aids_deaths_female_adults
group_deaths[3907:5859,2]<-"aids_deaths_female_adults"

group_deaths <- data.frame(group_deaths)

convert_to_numeric <- function(values) {
  #Convert the values to characters (there are factors)
  values <- as.character(values)
  
  converted <- as.numeric(values)
}

#Convert the first column into numbers
group_deaths[,1] = convert_to_numeric(group_deaths[,1])

group_deaths


ggplot(group_deaths, aes(x=groups, y=deaths, fill = groups)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_classic()

anova_one_way <- aov(deaths~groups, data = group_deaths)
summary(anova_one_way)

TukeyHSD(anova_one_way)
plot(TukeyHSD(anova_one_way))

#ANOVA test between the population groups and active cases
headings3 <- list(NULL, c("active","groups"))
group_active <- matrix(nrow=5859, ncol=2, byrow=TRUE, dimnames=headings3)

group_active[1:1953,1]<-dataset$active_HIV_cases_children
group_active[1:1953,2]<-"active_HIV_cases_children"
group_active[1954:3906,1]<-dataset$active_HIV_cases_male_adults
group_active[1956:3906,2]<-"active_HIV_cases_male_adults"
group_active[3907:5859,1]<-dataset$active_HIV_cases_female_adults
group_active[3907:5859,2]<-"active_HIV_cases_female_adults"

group_active <- data.frame(group_active)

convert_to_numeric <- function(values) {
  #Convert the values to characters (there are factors)
  values <- as.character(values)
  
  converted <- as.numeric(values)
}

#Convert the first column into numbers
group_active[,1] = convert_to_numeric(group_active[,1])

group_active


ggplot(group_active, aes(x=groups, y=active, fill = groups)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_classic()

anova_one_way2 <- aov(active~groups, data = group_active)
summary(anova_one_way2)

TukeyHSD(anova_one_way2)
plot(TukeyHSD(anova_one_way2))

#Deaths bellow the 15 years old
global_deaths_children <-(sum(dataset$aids_deaths_children))/9
global_deaths_males <-(sum(dataset$aids_deaths_male_adults))/9
global_deaths_females <-(sum(dataset$aids_deaths_female_adults))/9

global_deaths_per_year <- global_deaths_children+global_deaths_females+global_deaths_males

per_children <- global_deaths_children*100/global_deaths_per_year
per_male <- global_deaths_males*100/global_deaths_per_year
per_female <- global_deaths_females*100/global_deaths_per_year
