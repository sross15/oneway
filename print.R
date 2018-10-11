#'@title Print one-way ANOVA
#'
#'@description 
#'\code{print.oneway} print a one-way anova
#'
#'@details
#'This function creates prints one-way ANOVA results created by
#'the \code{\link{oneway}} function
#'
#'@param x an object of class \code{oneway}
#'@param col fill color for boxplots
#'@param ... additional arguments passed to the
#'\code{\link{boxplot}} function. 
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
#'mileage

print.oneway <- function(x, ...){
  if(!inherits(x, "oneway")) stop("Must be class 'oneway'")
  cat("\nSummary Statistics\n", 
      "====================================================\n", sep="")
  print(x$summarystats)
  cat("\nAnova\n", 
      "====================================================\n", sep="")
  print(summary.lm(x$anova))
}