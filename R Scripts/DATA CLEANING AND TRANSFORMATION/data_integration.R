library(plyr)

#Read population of countries dataset
df_pop <- read.csv("INPUT FILES/countries_population.csv", header = TRUE)

#Read countries continent dataset
df_continents <- read.csv("INPUT FILES/countries_continent.csv", header = TRUE)

#Get countries from datasets
countries <- as.character(df_pop$Country.Name)
codes <- as.character(df_pop$Country.Code)
countries <- data.frame(country_name = countries, country_code = codes)

#Join the countries with the continents
join <- left_join(countries, df_continents, by = c("country_code" = "code_3"))

#Create a dataframe with the name, code and continents of the countries
df <- data.frame(country_code = join$country_code,
                 country_name = join$country_name,
                 continent = join$continent)
df2 <- df[c(1:217),c(1:3)]

#Join the countries with the populations data
join_pop <- left_join(df2, df_pop, by = c("country_code" = "Country.Code"))

#Create a dataframe with the variables of population (from 2010 to 2019)
df2 <- join_pop[c(1:217),c(1:3,9:18)]

#Rename column names
names(df2)[names(df2) == "X2010..YR2010."] <- "population_2010"
names(df2)[names(df2) == "X2011..YR2011."] <- "population_2011"
names(df2)[names(df2) == "X2012..YR2012."] <- "population_2012"
names(df2)[names(df2) == "X2013..YR2013."] <- "population_2013"
names(df2)[names(df2) == "X2014..YR2014."] <- "population_2014"
names(df2)[names(df2) == "X2015..YR2015."] <- "population_2015"
names(df2)[names(df2) == "X2016..YR2016."] <- "population_2016"
names(df2)[names(df2) == "X2017..YR2017."] <- "population_2017"
names(df2)[names(df2) == "X2018..YR2018."] <- "population_2018"
names(df2)[names(df2) == "X2019..YR2019."] <- "population_2019"

#Change population values to numeric (because they are saved in factor types)
df2[,4:13] <- sapply(df2[,4:13],as.character)
df2[,4:13] <- sapply(df2[,4:13],as.numeric)

#Read deathrates of countries dataset
df_deaths <- read.csv("INPUT FILES/countries_deathrate.csv", header = TRUE)

#Join the countries with the deathrates data
join_deaths <- left_join(df2, df_deaths, by = c("country_code" = "Country.Code"))

#Create a dataframe with the variables of deathrates (from 2010 to 2019)
df2 <- join_deaths[c(1:217),c(1:13,19:28)]

#Rename column names
names(df2)[names(df2) == "X2010..YR2010."] <- "deathrate_2010"
names(df2)[names(df2) == "X2011..YR2011."] <- "deathrate_2011"
names(df2)[names(df2) == "X2012..YR2012."] <- "deathrate_2012"
names(df2)[names(df2) == "X2013..YR2013."] <- "deathrate_2013"
names(df2)[names(df2) == "X2014..YR2014."] <- "deathrate_2014"
names(df2)[names(df2) == "X2015..YR2015."] <- "deathrate_2015"
names(df2)[names(df2) == "X2016..YR2016."] <- "deathrate_2016"
names(df2)[names(df2) == "X2017..YR2017."] <- "deathrate_2017"
names(df2)[names(df2) == "X2018..YR2018."] <- "deathrate_2018"
names(df2)[names(df2) == "X2019..YR2019."] <- "deathrate_2019"

#Change population values to numeric (because they are saved in factor types)
df2[,14:23] <- sapply(df2[,14:23],as.character)
df2[,14:23] <- sapply(df2[,14:23],as.numeric)

#Read gdp of countries dataset
df_gdp <- read.csv("INPUT FILES/countries_gdp.csv", header = TRUE, row.names = NULL)

#Join the countries with the gdp data
join_gdp <- left_join(df2, df_gdp, by = c("country_code" = "Country.Code"))

#Create a dataframe with the variables of population (from 2010 to 2019)
df2 <- join_gdp[c(1:217),c(1:23,77:86)]

#Rename column names
names(df2)[names(df2) == "X2010"] <- "GDP(US$)_2010"
names(df2)[names(df2) == "X2011"] <- "GDP(US$)_2011"
names(df2)[names(df2) == "X2012"] <- "GDP(US$)_2012"
names(df2)[names(df2) == "X2013"] <- "GDP(US$)_2013"
names(df2)[names(df2) == "X2014"] <- "GDP(US$)_2014"
names(df2)[names(df2) == "X2015"] <- "GDP(US$)_2015"
names(df2)[names(df2) == "X2016"] <- "GDP(US$)_2016"
names(df2)[names(df2) == "X2017"] <- "GDP(US$)_2017"
names(df2)[names(df2) == "X2018"] <- "GDP(US$)_2018"
names(df2)[names(df2) == "X2019"] <- "GDP(US$)_2019"

#Change population values to numeric (because they are saved in factor types)
df2[,24:33] <- sapply(df2[,24:33],as.character)
df2[,24:33] <- sapply(df2[,24:33],as.numeric)

#Read active HIV cases children of countries dataset
df_hiv <- read.csv("INPUT FILES/active_HIV_cases_children.csv", header = TRUE, row.names = NULL)
d <- dim(df_hiv)
df_hiv <- df_hiv[c(1:d[1]),c(1,61:d[2])]

#Join the countries with the gdp data
join__hiv <- left_join(df2, df_hiv, by = c("country_name" = "Country"))

#Create a dataframe with the variables of population (from 2010 to 2018)
df2 <- join_hiv[c(1:217),c(1:33,35,38,41,44,47,50,53,56,59)]

#Rename column names
names(df2)[names(df2) == "X2010"] <- "active_HIV_cases_children_2010"
names(df2)[names(df2) == "X2011"] <- "active_HIV_cases_children_2011"
names(df2)[names(df2) == "X2012"] <- "active_HIV_cases_children_2012"
names(df2)[names(df2) == "X2013"] <- "active_HIV_cases_children_2013"
names(df2)[names(df2) == "X2014"] <- "active_HIV_cases_children_2014"
names(df2)[names(df2) == "X2015"] <- "active_HIV_cases_children_2015"
names(df2)[names(df2) == "X2016"] <- "active_HIV_cases_children_2016"
names(df2)[names(df2) == "X2017"] <- "active_HIV_cases_children_2017"
names(df2)[names(df2) == "X2018"] <- "active_HIV_cases_children_2018"
names(df2)[names(df2) == "X2019"] <- "active_HIV_cases_children_2019"

#Read active HIV cases of male adults of countries dataset
df_hiv <- read.csv("INPUT FILES/active_HIV_cases_male_adults.csv", header = TRUE, row.names = NULL)
d <- dim(df_hiv)
df_hiv <- df_hiv[c(1:d[1]),c(1,61:d[2])]

#Join the countries with the gdp data
join_hiv <- left_join(df2, df_hiv, by = c("country_name" = "Country"))

#Create a dataframe with the variables of population (from 2010 to 2018)
df2 <- join_hiv[c(1:217),c(1:42,44,47,50,53,56,59,62,65,68)]

#Rename column names
names(df2)[names(df2) == "X2010"] <- "active_HIV_cases_male_adults_2010"
names(df2)[names(df2) == "X2011"] <- "active_HIV_cases_male_adults_2011"
names(df2)[names(df2) == "X2012"] <- "active_HIV_cases_male_adults_2012"
names(df2)[names(df2) == "X2013"] <- "active_HIV_cases_male_adults_2013"
names(df2)[names(df2) == "X2014"] <- "active_HIV_cases_male_adults_2014"
names(df2)[names(df2) == "X2015"] <- "active_HIV_cases_male_adults_2015"
names(df2)[names(df2) == "X2016"] <- "active_HIV_cases_male_adults_2016"
names(df2)[names(df2) == "X2017"] <- "active_HIV_cases_male_adults_2017"
names(df2)[names(df2) == "X2018"] <- "active_HIV_cases_male_adults_2018"
names(df2)[names(df2) == "X2019"] <- "active_HIV_cases_male_adults_2019"

#Read active HIV cases of females of countries dataset
df_hiv <- read.csv("INPUT FILES/active_HIV_cases_female_adults.csv", header = TRUE, row.names = NULL)
d <- dim(df_hiv)
df_hiv <- df_hiv[c(1:d[1]),c(1,61:d[2])]

#Join the countries with the gdp data
join_hiv <- left_join(df2, df_hiv, by = c("country_name" = "Country"))

#Create a dataframe with the variables of population (from 2010 to 2018)
df2 <- join_hiv[c(1:217),c(1:51,53,56,59,62,65,68,71,74,77)]

#Rename column names
names(df2)[names(df2) == "X2010"] <- "active_HIV_cases_female_adults_2010"
names(df2)[names(df2) == "X2011"] <- "active_HIV_cases_female_adults_2011"
names(df2)[names(df2) == "X2012"] <- "active_HIV_cases_female_adults_2012"
names(df2)[names(df2) == "X2013"] <- "active_HIV_cases_female_adults_2013"
names(df2)[names(df2) == "X2014"] <- "active_HIV_cases_female_adults_2014"
names(df2)[names(df2) == "X2015"] <- "active_HIV_cases_female_adults_2015"
names(df2)[names(df2) == "X2016"] <- "active_HIV_cases_female_adults_2016"
names(df2)[names(df2) == "X2017"] <- "active_HIV_cases_female_adults_2017"
names(df2)[names(df2) == "X2018"] <- "active_HIV_cases_female_adults_2018"
names(df2)[names(df2) == "X2019"] <- "active_HIV_cases_female_adults_2019"

library(stringr)
#Function to convert variables from VIH data to numeric
convert_to_numeric <- function(values) {
  #Convert the values to characters (there are factors)
  values <- as.character(values)
  #Delete symbols that are not a number and spaces
  values <- str_remove(values,'<')
  values <- str_remove(values,' ')
  converted <- as.numeric(values)
}

d <- dim(df2)
for (i in 34:d[2]) {
  df2[,i] = convert_to_numeric(df2[,i])
}

#Read active HIV cases children of countries dataset
df_hiv <- read.csv("INPUT FILES/aids_deaths_children.csv", header = TRUE, row.names = NULL)
d <- dim(df_hiv)
df_hiv <- df_hiv[c(1:d[1]),c(1,61:d[2])]

#Join the countries with the gdp data
join_aids <- left_join(df2, df_hiv, by = c("country_name" = "Country"))

#Create a dataframe with the variables of population (from 2010 to 2018)
df2 <- join_aids[c(1:217),c(1:60,62,65,68,71,74,77,80,83,86)]

#Rename column names
names(df2)[names(df2) == "X2010"] <- "aids_deaths_children_2010"
names(df2)[names(df2) == "X2011"] <- "aids_deaths_children_2011"
names(df2)[names(df2) == "X2012"] <- "aids_deaths_children_2012"
names(df2)[names(df2) == "X2013"] <- "aids_deaths_children_2013"
names(df2)[names(df2) == "X2014"] <- "aids_deaths_children_2014"
names(df2)[names(df2) == "X2015"] <- "aids_deaths_children_2015"
names(df2)[names(df2) == "X2016"] <- "aids_deaths_children_2016"
names(df2)[names(df2) == "X2017"] <- "aids_deaths_children_2017"
names(df2)[names(df2) == "X2018"] <- "aids_deaths_children_2018"
names(df2)[names(df2) == "X2019"] <- "aids_deaths_children_2019"

#Read active HIV cases children of countries dataset
df_hiv <- read.csv("INPUT FILES/aids_deaths_male_adults.csv", header = TRUE, row.names = NULL)
d <- dim(df_hiv)
df_hiv <- df_hiv[c(1:d[1]),c(1,61:d[2])]

#Join the countries with the gdp data
library(dplyr)
join_aids <- left_join(df2, df_hiv, by = c("country_name" = "Country"))

#Create a dataframe with the variables of population (from 2010 to 2018)
df2 <- join_aids[c(1:217),c(1:69,71,74,77,80,83,86,89,92,95)]

#Rename column names
names(df2)[names(df2) == "X2010"] <- "aids_deaths_male_adults_2010"
names(df2)[names(df2) == "X2011"] <- "aids_deaths_male_adults_2011"
names(df2)[names(df2) == "X2012"] <- "aids_deaths_male_adults_2012"
names(df2)[names(df2) == "X2013"] <- "aids_deaths_male_adults_2013"
names(df2)[names(df2) == "X2014"] <- "aids_deaths_male_adults_2014"
names(df2)[names(df2) == "X2015"] <- "aids_deaths_male_adults_2015"
names(df2)[names(df2) == "X2016"] <- "aids_deaths_male_adults_2016"
names(df2)[names(df2) == "X2017"] <- "aids_deaths_male_adults_2017"
names(df2)[names(df2) == "X2018"] <- "aids_deaths_male_adults_2018"
names(df2)[names(df2) == "X2019"] <- "aids_deaths_male_adults_2019"

#Read aids deaths of females of countries dataset
df_hiv <- read.csv("INPUT FILES/aids_deaths_female_adults.csv", header = TRUE, row.names = NULL)
d <- dim(df_hiv)
df_hiv <- df_hiv[c(1:d[1]),c(1,61:d[2])]

#Join the countries with the data
join_aids <- left_join(df2, df_hiv, by = c("country_name" = "Country"))

#Create a dataframe with the variables
df2 <- join_aids[c(1:217),c(1:78,80,83,86,89,92,95,98,101,104)]

#Rename column names
names(df2)[names(df2) == "X2010"] <- "aids_deaths_female_adults_2010"
names(df2)[names(df2) == "X2011"] <- "aids_deaths_female_adults_2011"
names(df2)[names(df2) == "X2012"] <- "aids_deaths_female_adults_2012"
names(df2)[names(df2) == "X2013"] <- "aids_deaths_female_adults_2013"
names(df2)[names(df2) == "X2014"] <- "aids_deaths_female_adults_2014"
names(df2)[names(df2) == "X2015"] <- "aids_deaths_female_adults_2015"
names(df2)[names(df2) == "X2016"] <- "aids_deaths_female_adults_2016"
names(df2)[names(df2) == "X2017"] <- "aids_deaths_female_adults_2017"
names(df2)[names(df2) == "X2018"] <- "aids_deaths_female_adults_2018"
names(df2)[names(df2) == "X2019"] <- "aids_deaths_female_adults_2019"

#Convert to numeric
d <- dim(df2)
for (i in 34:d[2]) {
  df2[,i] = convert_to_numeric(df2[,i])
}

#Save data to file
write.csv(df2,"dataset.csv", row.names = FALSE)
