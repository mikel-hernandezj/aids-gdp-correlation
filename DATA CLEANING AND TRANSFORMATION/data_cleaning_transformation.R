library(VIM)

#Read datasets with the data for the moment
df <- read.csv("dataset.csv", header = TRUE)

#Summary of dataset
df_dim <- dim(df)
cols <- df_dim[2]
rows <- df_dim[1]

#Impute continent column
sum(is.na(df$continent))
as.character(df[is.na(df$continent),2])
idx <- which(df$country_name == 'Channel Islands')
df[idx,3] <- 'Europe'
idx <- which(df$country_name == 'Kosovo')
df[idx,3] <- 'Europe'
sum(is.na(df$continent))

#Create a vector to have all the years
years <- c('2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018')
cont <- 1

#Create empty vectors to save the variables
year <- vector(mode="character")
country_name <- vector(mode="character")
country_code <- vector(mode="character")
continent <- vector(mode="character")
population <- vector(mode='numeric')
deathrate <- vector(mode='numeric')
gdp <- vector(mode='numeric')
active_HIV_cases_children <- vector(mode='numeric')
active_HIV_cases_male_adults <- vector(mode='numeric')
active_HIV_cases_female_adults <- vector(mode='numeric')
aids_deaths_children <- vector(mode='numeric')
aids_deaths_male_adults <- vector(mode='numeric')
aids_deaths_female_adults <- vector(mode='numeric')

#Iterate over years and get the data of each year
for (y in years) {
  end <- cont + rows - 1
  year[cont:end] <- y;
  country_name[cont:end] <- as.character(df$country_name)
  country_code[cont:end] <- as.character(df$country_code)
  continent[cont:end] <- as.character(df$continent)
  
  cols <- grep(y,colnames(df))
  year_df <- df[cols]
  
  idx <- grep('population',colnames(year_df))
  population[cont:end] <- as.numeric(unlist(year_df[idx]))
  
  idx <- grep('deathrate',colnames(year_df))
  deathrate[cont:end] <- as.numeric(unlist(year_df[idx]))
  
  idx <- grep('GDP',colnames(year_df))
  gdp[cont:end] <- as.numeric(unlist(year_df[idx]))
  
  idx <- grep('active_HIV_cases_children',colnames(year_df))
  active_HIV_cases_children[cont:end] <- as.numeric(unlist(year_df[idx]))
  
  idx <- grep('active_HIV_cases_male_adults',colnames(year_df))
  active_HIV_cases_male_adults[cont:end] <- as.numeric(unlist(year_df[idx]))
  
  idx <- grep('active_HIV_cases_female_adults',colnames(year_df))
  active_HIV_cases_female_adults[cont:end] <- as.numeric(unlist(year_df[idx]))
  
  idx <- grep('aids_deaths_children',colnames(year_df))
  aids_deaths_children[cont:end] <- as.numeric(unlist(year_df[idx]))
  
  idx <- grep('aids_deaths_male_adults',colnames(year_df))
  aids_deaths_male_adults[cont:end] <- as.numeric(unlist(year_df[idx]))
  
  idx <- grep('aids_deaths_female_adults',colnames(year_df))
  aids_deaths_female_adults[cont:end] <- as.numeric(unlist(year_df[idx]))
  
  cont <- cont + rows
}

#Create a dataframe with the data of each year
df2 <- data.frame(year, country_name, country_code, continent, population, deathrate, gdp,
                  active_HIV_cases_children, active_HIV_cases_male_adults, active_HIV_cases_female_adults,
                  aids_deaths_children, aids_deaths_male_adults, aids_deaths_female_adults)


#Impute dataset
df_imputed <- kNN(df2, imp_var = FALSE)
df_imputed

#Save data to file
write.csv(df_imputed,"final_dataset.csv", row.names = FALSE)
