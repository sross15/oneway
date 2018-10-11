#'@title Summarize one-way ANOVA results
#'
#'@description 
#'\code{summarize.oneway} summarizes the results of
#'   a one-way anova
#'
#'@details
#'This function prints the ANOVA table from the linear model
#'
#'@param x an object of class \code{oneway}
#'@param ... additional arguments passed to the print function

#'
#'@param data a data frame containing the variables 
#'in the model.
#'
#'@export
#'
#'@return the input object is returned silently
#'
#'@author Shane Ross <saross@@wesleyan.edu>
#'
#'@examples
#'mileage <- oneway(hwy ~ class, cars)
#'summary(mileage)

summary.oneway <- function(x, ...){
  if(!inherits(x, "oneway")) stop("x must  be class 'oneway'")
  print(anova(x$anova), ...)
}