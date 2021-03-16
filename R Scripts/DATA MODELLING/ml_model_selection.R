library(mlbench)
library(caret)
library(corrplot)
library(e1071)
library(fastICA)
library(lars)
library(RWeka)

# 1. Load dataset
df <- read.csv("final_dataset.csv", header = TRUE)


#2. Create the response variable (aids-related deaths)
aids_deaths <- df$aids_deaths_children + df$aids_deaths_female_adults + df$aids_deaths_male_adults
cols <- grep('deaths',colnames(df))
df <- df[-cols]
df <- cbind(df,aids_deaths)

#3. Split out validation dataset
set.seed(7)
val_idx <- createDataPartition(df$aids_deaths, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- df[-val_idx,]
# use the remaining 80% of data to training and testing the models
dataset <- df[val_idx,]

#4. Preprocess data
summary(dataset)
sapply(dataset[,5:10], sd)
apply(dataset[,5:10], 2, skewness)

#Standardize and normalize data (combining scale, center and box-cox transforms)
preprocessParams <- preProcess(dataset[,5:10], method = c('center', 'scale', 'BoxCox'))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams, dataset)
# summarize the transformed dataset
summary(transformed)
sapply(transformed[,5:10], sd)
apply(transformed[,5:10], 2, skewness)


#5. Evaluate ML algorithms
# Run algorithms using 10-fold cross-validation
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "RMSE"
dataset_transformed <- transformed[,5:11]
# lm
set.seed(7)
fit.lm <- train(aids_deaths~., data=dataset_transformed, method="lm", metric=metric, trControl=trainControl)
# GLM (generalized linear model)
set.seed(7)
fit.glm <- train(aids_deaths~., data=dataset_transformed, method="glm", metric=metric, trControl=trainControl)
# GLMNET
set.seed(7)
fit.glmnet <- train(aids_deaths~., data=dataset_transformed, method="glmnet", metric=metric, trControl=trainControl)
# SVM (support vector machines)
fit.svm <- train(aids_deaths~., data=dataset_transformed, method="svmRadial", metric=metric, trControl=trainControl)
# CART (decision tree)
fit.cart <- train(aids_deaths~., data=dataset_transformed, method="rpart", metric=metric, trControl=trainControl)
# KNN (k nearest neighbour)
set.seed(7)
fit.knn <- train(aids_deaths~., data=dataset_transformed, method="knn", metric=metric, trControl=trainControl)
# ICR (Independent component regression)
fit.icr <- train(aids_deaths~., data=dataset_transformed, method="icr", metric=metric, trControl=trainControl)
# LARS (Least angle regression)
fit.lars <- train(aids_deaths~., data=dataset_transformed, method="lars", metric=metric, trControl=trainControl)
# Compare algorithms
results <- resamples(list(LM=fit.lm, GLM=fit.glm, GLMNET=fit.glmnet, SVM=fit.svm,
                          CART=fit.cart, KNN=fit.knn, ICR=fit.icr, LARS=fit.lars))
summary(results)
dotplot(results)


#6. Idenfify and remove highly correlated features
# calculate correlation matrix
dataset_transformed <- transformed[,5:11]
correlationMatrix <- cor(dataset_transformed)
correlationMatrix
# find attributes that are highly corrected (ideally >0.75)
cutoff <- 0.75
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=cutoff)
dataset3 <- dataset_transformed[,-highlyCorrelated]
results <- nearZeroVar(dataset3, saveMetrics=TRUE)
dataset_transformed <- dataset3[,!results$zeroVar]

#7. Evaluate ML algorithms again
# Run algorithms using 10-fold cross-validation
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "RMSE"
# lm
set.seed(7)
fit.lm <- train(aids_deaths~., data=dataset_transformed, method="lm", metric=metric, trControl=trainControl)
# GLM (generalized linear model)
set.seed(7)
fit.glm <- train(aids_deaths~., data=dataset_transformed, method="glm", metric=metric, trControl=trainControl)
# GLMNET
set.seed(7)
fit.glmnet <- train(aids_deaths~., data=dataset_transformed, method="glmnet", metric=metric, trControl=trainControl)
# SVM (support vector machines)
fit.svm <- train(aids_deaths~., data=dataset_transformed, method="svmRadial", metric=metric, trControl=trainControl)
# CART (decision tree)
fit.cart <- train(aids_deaths~., data=dataset_transformed, method="rpart", metric=metric, trControl=trainControl)
# KNN (k nearest neighbour)
set.seed(7)
fit.knn <- train(aids_deaths~., data=dataset_transformed, method="knn", metric=metric, trControl=trainControl)
# ICR (Independent component regression)
fit.icr <- train(aids_deaths~., data=dataset_transformed, method="icr", metric=metric, trControl=trainControl)
# LARS (Least angle regression)
fit.lars <- train(aids_deaths~., data=dataset_transformed, method="lars", metric=metric, trControl=trainControl)
# Compare algorithms
feature_results <- resamples(list(LM=fit.lm, GLM=fit.glm, GLMNET=fit.glmnet, SVM=fit.svm,
                                  CART=fit.cart, KNN=fit.knn, ICR=fit.icr, LARS=fit.lars))
summary(feature_results)
dotplot(feature_results)

#8. Normalize dataset
#Normalize dataset
preprocessParams <- preProcess(transformed[,1:10], method = c('range'))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed2 <- predict(preprocessParams, transformed)
summary(transformed2)

#9. Evaluate ML algorithms again
# Run algorithms using 10-fold cross-validation
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "RMSE"
dataset_transformed <- transformed2[,5:11]
# lm
set.seed(7)
fit.lm <- train(aids_deaths~., data=dataset_transformed, method="lm", metric=metric, trControl=trainControl)
# GLM (generalized linear model)
set.seed(7)
fit.glm <- train(aids_deaths~., data=dataset_transformed, method="glm", metric=metric, trControl=trainControl)
# GLMNET
set.seed(7)
fit.glmnet <- train(aids_deaths~., data=dataset_transformed, method="glmnet", metric=metric, trControl=trainControl)
# SVM (support vector machines)
fit.svm <- train(aids_deaths~., data=dataset_transformed, method="svmRadial", metric=metric, trControl=trainControl)
# CART (decision tree)
fit.cart <- train(aids_deaths~., data=dataset_transformed, method="rpart", metric=metric, trControl=trainControl)
# KNN (k nearest neighbour)
set.seed(7)
fit.knn <- train(aids_deaths~., data=dataset_transformed, method="knn", metric=metric, trControl=trainControl)
# ICR (Independent component regression)
fit.icr <- train(aids_deaths~., data=dataset_transformed, method="icr", metric=metric, trControl=trainControl)
# LARS (Least angle regression)
fit.lars <- train(aids_deaths~., data=dataset_transformed, method="lars", metric=metric, trControl=trainControl)
# Compare algorithms
feature_results <- resamples(list(LM=fit.lm, GLM=fit.glm, GLMNET=fit.glmnet, SVM=fit.svm,
                                  CART=fit.cart, KNN=fit.knn, ICR=fit.icr, LARS=fit.lars))
summary(feature_results)
dotplot(feature_results)

#10. Idenfify and remove highly correlated features
# calculate correlation matrix
dataset_transformed2 <- transformed2[,5:11]
correlationMatrix <- cor(dataset_transformed2)
correlationMatrix
# find attributes that are highly corrected (ideally >0.75)
cutoff <- 0.75
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=cutoff)
dataset3 <- dataset_transformed2[,-highlyCorrelated]
results <- nearZeroVar(dataset3, saveMetrics=TRUE)
dataset_transformed2 <- dataset3[,!results$zeroVar]

#11. Evaluate ML algorithms again
# Run algorithms using 10-fold cross-validation
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "RMSE"
dataset_transformed <- dataset_transformed2
# lm
set.seed(7)
fit.lm <- train(aids_deaths~., data=dataset_transformed, method="lm", metric=metric, trControl=trainControl)
# GLM (generalized linear model)
set.seed(7)
fit.glm <- train(aids_deaths~., data=dataset_transformed, method="glm", metric=metric, trControl=trainControl)
# GLMNET
set.seed(7)
fit.glmnet <- train(aids_deaths~., data=dataset_transformed, method="glmnet", metric=metric, trControl=trainControl)
# SVM (support vector machines)
fit.svm <- train(aids_deaths~., data=dataset_transformed, method="svmRadial", metric=metric, trControl=trainControl)
# CART (decision tree)
fit.cart <- train(aids_deaths~., data=dataset_transformed, method="rpart", metric=metric, trControl=trainControl)
# KNN (k nearest neighbour)
set.seed(7)
fit.knn <- train(aids_deaths~., data=dataset_transformed, method="knn", metric=metric, trControl=trainControl)
# ICR (Independent component regression)
fit.icr <- train(aids_deaths~., data=dataset_transformed, method="icr", metric=metric, trControl=trainControl)
# LARS (Least angle regression)
fit.lars <- train(aids_deaths~., data=dataset_transformed, method="lars", metric=metric, trControl=trainControl)
# Compare algorithms
feature_results <- resamples(list(LM=fit.lm, GLM=fit.glm, GLMNET=fit.glmnet, SVM=fit.svm,
                                  CART=fit.cart, KNN=fit.knn, ICR=fit.icr, LARS=fit.lars))
summary(feature_results)
dotplot(feature_results)

#Selected model --> knn with data scale, center and box-cox and discretizing


#12. Tunning the model
#Pre-process data
preprocessParams <- preProcess(dataset[,1:10], method = c('center', 'scale', 'BoxCox'))
# transform the dataset using the parameters
transformed <- predict(preprocessParams, dataset)
preprocessParams <- preProcess(transformed[,1:10], method = c('range'))
# transform the dataset using the parameters
transformed2 <- predict(preprocessParams, transformed)
dataset_transformed <- transformed2[,5:11]

# KNN (k nearest neighbour)
set.seed(7)
fit.knn <- train(aids_deaths~., data=dataset_transformed, method="knn", metric=metric, trControl=trainControl)
# estimate variable importance
importance <- varImp(fit.knn, scale=TRUE)
# summarize importance
print(importance)
# plot importance
plot(importance)

#Model tunning
print(fit.knn)
plot(fit.knn)

grid <- expand.grid(.k=seq(1, 14, by=1))
fit.knn <- train(aids_deaths~., data=dataset_transformed, method="knn", metric=metric, tuneGrid=grid, trControl=trainControl)
print(fit.knn)
plot(fit.knn)
#Best model knn with k=1, with the data scalled, centered and box-cox transformed
