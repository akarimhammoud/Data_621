#### Function to create train and test

partition_data <- function(data, isTest) { 
  sample <- sample.split(data$AdoptionSpeed,
                         SplitRatio = .75)
  train <- subset(data,
                 sample == isTest)
  
  return(train)
}

#Create train data
create_train_data  <- function(data) {
  
  new_data <- partition_data(data, TRUE)
  
  return(new_data)
}

#Create test data
create_test_data  <- function(data) {
  
  new_data <- partition_data(data, FALSE)
  
  return(new_data)
}