library(plyr)
library(dplyr)

#Read the dataset with the information for analysis
dataset <- read.csv("final_dataset.csv", header = TRUE)
dataset

summary(dataset)

#Transform the dataset for the analysis
countries <- unique(as.character(dataset$country_name))
headings <- list(NULL, c('country','continent','gdp','active_HIV_cases', 'aids_deaths'))
dsglobal <- matrix(nrow=length(countries), ncol=5, byrow=TRUE, dimnames=headings)

for (i in 1:length(countries)) {
  df_country <- filter(dataset, country_name == countries[i])
  dsglobal[i,1] <- countries[i]
  dsglobal[i,2] <- as.character(unique(df_country$continent))
  dsglobal[i,3] <- mean(df_country$gdp)
  dsglobal[i,4] <- mean(df_country$active_HIV_cases_children + df_country$active_HIV_cases_female_adults + df_country$active_HIV_cases_male_adults)
  dsglobal[i,5] <- mean(df_country$aids_deaths_children + df_country$aids_deaths_female_adults + df_country$aids_deaths_male_adults)
}

dsglobal <- data.frame(dsglobal)

convert_to_numeric <- function(values) {
  #Convert the values to characters (there are factors)
  values <- as.character(values)
  #Delete symbols that are not a number and spaces
  converted <- as.numeric(values)
}

d <- dim(dsglobal)
for (i in 3:5) {
  dsglobal[,i] = convert_to_numeric(dsglobal[,i])
}

#Calculate the mean values of the gdp of all the countries
gdp_mean <- mean(dsglobal$gdp)

#Select the poorest and richest countries
poorest_countries <- filter(dsglobal, gdp < gdp_mean)
poorest_countries$country
richest_countries <- filter(dsglobal, gdp > gdp_mean)
richest_countries$country

summary(poorest_countries)
summary(richest_countries)

#Analyse active HIV cases and aids deaths of poorest and richest countries
var(poorest_countries$active_HIV_cases)
var(richest_countries$active_HIV_cases)

#boxplots
par(mfrow=c(2,2))
boxplot(poorest_countries$active_HIV_cases, main='Active HIV cases (Poorest countries)')
boxplot(richest_countries$active_HIV_cases, main='Active HIV cases (Richest countries)')
boxplot(poorest_countries$aids_deaths, main='AIDS deaths (Poorest countries)')
boxplot(richest_countries$aids_deaths, main='AIDS deaths (Richest countries)')

#t test
var.test(poorest_countries$active_HIV_cases, richest_countries$active_HIV_cases)
t.test(poorest_countries$active_HIV_cases, richest_countries$active_HIV_cases, alternative = 'greater')

var.test(poorest_countries$aids_deaths, richest_countries$aids_deaths)
t.test(poorest_countries$aids_deaths, richest_countries$aids_deaths, alternative = 'greater')

#correlation between GDP and VIH cases
library(corrplot)
library(ggplot2)
corrplot(cor(dsglobal[,3:5]))

cor.test(dsglobal$gdp, dsglobal$active_HIV_cases)
cor(dsglobal$gdp, dsglobal$active_HIV_cases)

lm <- lm(dsglobal$active_HIV_cases~dsglobal$gdp)
print(lm)
c <- lm$coefficients
summary(lm)
vals <- predict(lm,data.frame(dsglobal$gdp))

ggplot(dsglobal, aes(x=gdp, y=active_HIV_cases)) + geom_point(color='blue') + 
  geom_line(aes(x=gdp, y=vals), color='red', size=1) + 
  ggtitle('GDP vs. Active HIV cases') + labs(x='GDP', y='Active HIV cases')


#correlation between gdp and aids-related deaths
cor.test(dsglobal$gdp, dsglobal$aids_deaths)
cor(dsglobal$gdp, dsglobal$aids_deaths)

lm <- lm(dsglobal$aids_deaths~dsglobal$gdp)
vals <- predict(lm,data.frame(dsglobal$gdp))
summary(lm)

ggplot(dsglobal, aes(x=gdp, y=aids_deaths)) + geom_point(color='blue') + 
  geom_line(aes(x=gdp, y=vals), color='red', size=1) + ggtitle('GDP vs. AIDS deaths') +
  labs(x='GDP', y='AIDS deaths')

#correlation between active HIV cases and AIDS-related deaths
cor.test(dsglobal$active_HIV_cases, dsglobal$aids_deaths)
cor(dsglobal$active_HIV_cases, dsglobal$aids_deaths)

lm <- lm(dsglobal$aids_deaths~dsglobal$active_HIV_cases)
vals <- predict(lm,data.frame(dsglobal$active_HIV_cases))
summary(lm)

ggplot(dsglobal, aes(x=active_HIV_cases, y=aids_deaths)) + geom_point(color='blue') + 
  geom_line(aes(x=active_HIV_cases, y=vals), color='red', size=1) + ggtitle('Active HIV cases vs. AIDS deaths') +
  labs(x='Active HIV cases', y='AIDS deaths')