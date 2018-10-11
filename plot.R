#'@title Plot one-way ANOVA
#'
#'@description 
#'\code{plot.oneway} plots a one-way anova
#'
#'@details
#'This function creates side by side box plots for each group
#'in the analysis.
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
#'@return NULL
#'
#'@author Shane Ross <saross@@wesleyan.edu>
#'
#'@examples
#'mileage <- oneway(hwy ~ class, cars)
#'plot(mileage)

plot.oneway <- function(x, ...){
  if(!inherits(x, "oneway")) stop("Must be class 'oneway'")
  boxplot(x$anova$terms, x$anova$model, col="skyblue")
}
