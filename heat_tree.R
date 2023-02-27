library(rpart)
library(rpart.plot)

HeatTree <- function(tree, low.is.green=FALSE, ...) { # dots args passed to prp
  y <- tree$frame$yval
  if(low.is.green)
    y <- -y
  max <- max(y)
  min <- min(y)
  cols <- rainbow(99, end=.36)[
    ifelse(y >  y[1], (y-y[1]) * (99-50) / (max-y[1]) + 50,
           (y-min)  * (50-1)  / (y[1]-min) + 1)]
  prp(tree, branch.col=cols, box.col=cols, ...)
}


