# RF performance stats, based on http://statistics.berkeley.edu/sites/default/files/tech-reports/666.pdf
rfPerformanceStats <- function(tree, newdata, responsevar, beta){
  predictions <- predict(tree, newdata = newdata, type = "class")
  cm <- table(predictions, newdata[,responsevar])
  TN <- cm[1,1]
  TP <- cm[2,2]
  FN <- cm[1,2]
  FP <- cm[2,1]
  true_negative_rate <- TN/(TN+FP)
  true_positive_rate_recall <- TP/(TP+FN)
  G_mean <- (true_negative_rate*true_positive_rate_recall)^(1/2)
  weighted_accuracy <- beta*true_positive_rate_recall + (1-beta)*true_negative_rate
  precision <- TP/(TP+FP)
  f_measure <- (2*(precision)*(true_positive_rate_recall))/(precision + true_positive_rate_recall)
  l <- list(cm, true_negative_rate, true_positive_rate_recall, G_mean, weighted_accuracy, precision, f_measure)
  names(l) <- c("cm", "true_negative_rate", "true_positive_rate_recall", "G_mean", "weighted_accuracy", "precision", "f_measure")
  return(l)
}

# technically this should be done with 10-fold cross-validation to obtain these performance metrics
