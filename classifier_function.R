#' Train classifier and obtain feature importance
#'
#' This code allows you to obtain feature importance and compute classification accuracy.
#' @param feature_data the structured data.
#' @param event_label event label of each epoch: tasks number for between-tasks classifiers and movement label for within-task classifiers.
#' @param testnum number of epochs used as testing sets.
#' @keywords Feature importance and Classifier
#' @import ranger
#' @examples

library(ranger)
## classifier function
rf_classifier <- function(feature_data, event_label, testnum = 4){
  data_ratio <- data.frame(feature_data, label = event_label)
  rate <- c()
  for(iter in 1: 1000){
    test.idx <- sample(1:length(event_label), testnum)
    train_data <- data_ratio[-test.idx, ]
    test_data <- data_ratio[test.idx, ]
    
    fit <- ranger(label ~., data = train_data)
    pred.rf <- predict(fit, data = test_data)
    result <- table(test_data$label, pred.rf$predictions)
    rate <- c(rate, sum(diag(result))/length(test.idx))
  }
  a <- mean(rate)
  return(a)
}

## function to get feature importance with 1000 iterations
rf_feature_importance <- function(feature_data, event_label){
  imp_c1 <- matrix(0, dim(feature_data)[2], 1000)
  data_ratio <- data.frame(feature_data, label = event_label)
  for(iter in 1: 1000){
    test.idx <- sample(1:length(event_label), 1)
    train_data <- data_ratio[-test.idx, ]
    test_data <- data_ratio[test.idx, ]
    
    fit <- ranger(label ~., data = train_data, importance = "impurity")
    imp_c1[, iter] <- fit$variable.importance
  }
  imp_c1_sum <- apply(imp_c1, 1, mean)
  return(imp_c1_sum)
}
