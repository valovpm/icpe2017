source("monitor.r")

GetMethodInfo <- function(methodID) {
  # Performs Sobol sampling of parameters of the specified regression method
  # 
  # Args:
  #   methodID: ID of the regression method to be sampled

  # Populate methodInfo data structure
  methodInfo <- list()
  methodInfo$methodID <- methodID
  methodInfo$methodName <- methodNames[methodID]
  # methodInfo$sobolSeq <- sobolSeq
  # methodInfo$paramSeq <- paramSeq

  return(methodInfo)
}

# Regression models training methods ##########################################
Trainer <- function(methodID, trainData) {
  #
  #
  # Args:
  #   methodID:
  #   params:
  #   data:

  model <- NULL

  # Select regression method implementation and parameters
  regMethod <<- switch(methodID,
                       "1" = TrainCart,
                       "2" = TrainBag,
                       "3" = TrainFrst)

  methodParams <- switch(methodID,
                         "1" = methodsParams$cart$vals,
                         "2" = methodsParams$bag$vals,
                         "3" = methodsParams$frst$vals)

  # Train regression model
  model <- regMethod(methodParams, trainData)
  return(model)
}

TrainCart <- function(params, data) {
  #
  #
  # Args:
  #   params:
  #   data:
  
  # Initialize parameters dependant on data size
  obsNum <- nrow(data)
  minSplit  <- params[1]
  minBucket <- params[2]
  maxDepth  <- params[3]
  complexity <- params[4]

  # Train regression model
  Monitor("Starting CART model training...")
  require(rpart, quietly = TRUE)
    # set.seed(1)
    model <-
      rpart(PERF ~ .,
            data = data,
            method = "anova",
            parms = list(split = "information"),
            control = rpart.control(minsplit = minSplit,
                                    minbucket = minBucket,
                                    maxdepth = maxDepth,
                                    cp = complexity,
                                    usesurrogate = 0,
                                    maxsurrogate = 0))
  Monitor("Finished CART model training")

  return(model)
}

TrainBag <- function(params, data) {
  #

  Monitor()
  
  # Get data properties
  obsNum <- nrow(data)
  ftrNum <- ncol(data) - 1

  # Initialize parameters dependant on data
  ntree <- params[1]
  nodesize <- params[2]
  mtry <- ftrNum

  # Train regression model
  Monitor("Starting BAGGING model training...")
  require(randomForest, quietly = TRUE)
  # set.seed(1)
  model <-
    randomForest(PERF ~ .,
                 data = data,

                 ntree = ntree,
                 nodesize = nodesize,
                 mtry = ftrNum,

                 importance = TRUE,
                 na.action = na.omit,
                 replace = FALSE)
  Monitor("Finished BAGGING model training")

  return(model)
}

TrainFrst <- function(params, data) {
  # 

  Monitor()

  # Get data properties
  obsNum <- nrow(data)
  ftrNum <- ncol(data) - 1

  # Initialize parameters dependant on data
  ntree <- params[1]
  nodesize <- params[2]
  mtry <- params[3]

  # Train regression model
  Monitor("Starting FOREST model training...")
  require(randomForest, quietly = TRUE)
  # set.seed(1)
  model <-
    randomForest(PERF ~ .,
                 data = data,

                 ntree = ntree,
                 nodesize = nodesize,
                 mtry = mtry,

                 importance = TRUE,
                 na.action = na.omit,
                 replace = FALSE)
  Monitor("Finished FOREST model training")

  return(model)
}
