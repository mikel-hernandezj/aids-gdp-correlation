library(mlbench)
library(caret)
library(corrplot)
library(e1071)
library(ggplot2)

# 1. Load dataset
df <- read.csv("final_dataset.csv", header = TRUE)

#2. Create the response variable (aids-related deaths)
y <- as.numeric(df$aids_deaths_children + df$aids_deaths_female_adults + df$aids_deaths_male_adults)
cols <- grep('deaths',colnames(df))
df <- df[-cols]

#3. Split out datasets
set.seed(7)
val_idx <- createDataPartition(y, p=0.80, list=FALSE)
# select 20% of the data for validation
X_val <- df[-val_idx,]
y_val <- y[-val_idx]
# use the remaining 80% of data to training and testing the models
X_train <- df[val_idx,]
y_train <- y[val_idx]

#4. Transform the training data
#Preprocess the data
preprocessParams <- preProcess(X_train[,5:10], method = c('center', 'scale', 'BoxCox'))
X_train_trans <- predict(preprocessParams, X_train[,5:10])
preprocessParams <- preProcess(X_train_trans[,1:6], method = c('range'))
X_train_trans <- predict(preprocessParams, X_train_trans[,1:6])

#5. Train the final model
model <- knnreg(x=as.matrix(X_train_trans), y=y_train, k=1)
summary(model)

#6. transform the validation dataset
set.seed(7)
preprocessParams <- preProcess(X_val[,5:10], method = c('center', 'scale', 'BoxCox'))
X_val_trans <- predict(preprocessParams, X_val[,5:10])
preprocessParams <- preProcess(X_val_trans[,1:6], method = c('range'))
X_val_trans <- predict(preprocessParams, X_val_trans[,1:6])

# 7. Use the model to make predictions on the validation dataset
predictions <- predict(model, newdata=as.matrix(X_val_trans))
# calculate RMSE
rmse <- RMSE(predictions, y_val)
# calculate R2 value
r2 <- R2(predictions, y_val)
print(rmse)
print(r2)

#8. Compare results
linModel <- lm(predictions~y_val)
print(linModel)
summary(linModel)
y_preds_lm <- predict(linModel,data.frame(y_val))

residuals <- abs(y_val - predictions)

year <- as.factor(df[-val_idx,1])
country <- df[-val_idx,2]
results <- data.frame(year,country,y_val,predictions,residuals,y_preds_lm)
summary(results)

#Plot the correlation between the real values and the predictions
corrplot(cor(results[,3:4]))
cor.test(results[,3], results[,4])

ggplot(results, aes(x=y_val, y=predictions)) + geom_point(color='blue') + 
  geom_line(aes(x=y_val, y=y_preds_lm), color='red', size=1) +
  ggtitle('Real values vs. Predictions') + labs(x='Real values', y='Predictions')

#9. Save the model in a file
saveRDS(model, "./finalModel.rds")