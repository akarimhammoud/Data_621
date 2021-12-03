library(here)
require(here('R', 'partition_train_test_data.R'))

#### Function to create RF confusion matrix
create_rf_confusion_matrix <- function(pred, test){
  
  #Creates vectors having data points
  AdoptionSpeed_test <- as.numeric(unlist(test$AdoptionSpeed))
  expected_value <- factor(c(AdoptionSpeed_test))
  
  predicted_value <- factor(c(pred))
  
  #Creating confusion matrix
  rf_confusion_matrix <- confusionMatrix(data=predicted_value,
                                         reference = expected_value,
                                         positive='1')
  
  #Results 
  return(rf_confusion_matrix)
}