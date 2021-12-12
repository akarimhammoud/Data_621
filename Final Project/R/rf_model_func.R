library(randomForest)
require(xgboost)
library(Matrix)
library(caret)
library(here)
require(caTools)
source(here('R', 'partition_train_test_data.R'))
source(here('R', 'create_rf_confusion_matrix.R'))

rf_model_func <- function(data) {
  #train, test data for RF model 1
  rf_train = create_train_data(data)
  rf_test = create_test_data(data)
  
  #create RF model 1
  rf <- randomForest(
    AdoptionSpeed ~ .,
    data=rf_train
  )
  
  #Predict AdoptionSpeed with RF model 1
  rf_pred = predict(rf, newdata=rf_test)
  
  #Create a confusion matrix for RF model 1
  rf_confusion_matrix = create_rf_confusion_matrix(rf_pred, rf_test)
  return(rf_confusion_matrix)
}