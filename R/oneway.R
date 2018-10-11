#'@title One Way Analysis of Variance
#'
#'@description 
#'\code{oneway} computes a one-way analysis of variance
#'and includes group-level summary statistics.
#'
#'@details
#'This function computes a standard one-way ANOVA,
#'means, and standard deviation. Missing values are handled
#'via list-wise deletion
#'
#'@param formula an object of class formula,
#'relating the dependent variable to the 
#'grouping variable
#'
#'@param data a data frame containing the variables 
#'in the model.
#'
#'@export
#'
#'@return a list with 2 elements: 
#'\item{oneway}{a list with the lm results}
#'\item{summarystats}{a data frame with the summary statistics}
#'
#'@author Shane Ross <saross@@wesleyan.edu>
#'
#'@examples
#'mileage <- oneway(hwy ~ class, cars)
#'summary(mileage)
#'print(mileage)
#'plot(mileage)

oneway <- function(formula, data) {
  
  # listwise deletion of missing values
  data_complete <- na.omit(data)
  
  # anova
  fit <- lm(formula, data_complete)
  
  stats <-  aggregate(formula,
                      data,
                      function(x) c(n = length(x), mean = mean(x), sd = sd(x)))
  
  
  result <- list(anova = fit, summarystats = stats)
  class(result) <- c("oneway", "list")
  return(result)
  
}