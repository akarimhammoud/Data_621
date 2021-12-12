library(randomForest)
require(xgboost)
library(Matrix)
library(caret)
library(here)
require(caTools)
source(here('R', 'partition_train_test_data.R'))

xg_model_func <- function(data) {
  #train, test data for XG model 1
  xg_train = as.matrix(create_train_data(data))
  xg_test = as.matrix(create_test_data(data))
  
  #separate out the label
  adoption_speed_train = xg_train[ , ncol(xg_train)]
  adoption_speed_test = xg_test[ , ncol(xg_test)]
  
  #drop AdoptionSpeed from train and test
  xg_train = subset(xg_train, select=-c(ncol(xg_train)))
  xg_test = subset(xg_test, select=-c(ncol(xg_test)))
  
  #XGBoost requires a dgc matrix
  xg_train=as(xg_train, "dgCMatrix")
  xg_test=as(xg_test, "dgCMatrix")
  
  #Create the model
  xg <- xgboost(data = xg_train, label = adoption_speed_train, nrounds = 300, verbose=0)
  
  #Predict
  xg_pred <- predict(xg, xg_test)
  
  #Create a confusion matrix for XG model 1
  xg_pred = factor(as.integer(xg_pred))
  adoption_speed_test = factor(as.integer(adoption_speed_test))
  
  xg_confusion_matrix = confusionMatrix(xg_pred, adoption_speed_test)
  return(xg_confusion_matrix)
}
