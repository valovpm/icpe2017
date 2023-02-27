library(dplyr)
library(tidyr)
library(rpart.plot)

source("monitor.r")
source("regmodels.r")
source("data.r")
source("scaling.r")

set.seed(1)

# Analysis functions ##########################################################
DistFeatures <- function() {
  
  # Analyse configurable systems using different regression methods
  for (s in 1:length(systemNames)) {
    for (m in 1:length(methodNames)) {

      # Perform data analysis
      sysDataInfo <- GetSysDataInfo(s)
      methodInfo  <- GetMethodInfo(m)
      results     <- DistSys(sysDataInfo, methodInfo)

      # Export feature distribution results
      distFile <- file.path(distPath,
                            paste(systemNames[s], "_",
                                  methodNames[m], ".csv", sep = ""))
      write.csv(results, file = distFile, row.names = FALSE)
    }
  }
}

DistSys <- function(sysInfo, methodInfo) {
  
  # Generate data frames that contain information about experimental design:
  #   df1: specifies machines to be sampled, sampling sizes and repetitions
  #   df2: specifies machines for transferring trained regression model to,
  #        number of observations to calculate scaling coefficient, and
  #        number of times (repetitions) that scaling should be performed
  df1 <- expand.grid(1:experParams$sampleRep,
                    (1:experParams$sampleSizes) * sysInfo$featureNum,
                    sysInfo$machines)
  df2 <- expand.grid(1:experParams$scaleRep,
                     1:experParams$scaleSizes,
                     sysInfo$machines)
  colnames(df1) <- c("sampleRep", "sampleSize", "train")
  colnames(df2) <- c("scaleRep", "scaleSize", "target")
  featureDist   <- NULL

  for (n in 1:nrow(df1)) {
    # Extract training machine, sampling size, and sampling repetition
    trn      <- df1$train[n]
    smplSize <- df1$sampleSize[n]
    smplRep  <- df1$sampleRep[n]
    Monitor(paste("Training machine:", trn, ";",
                  "Sample size:", smplSize, ";",
                  "Sample repetition:", smplRep, ";"))

    # Extract sample of "smplSize" size from training machine
    trnObs <-
      sysInfo$sysData %>%
      filter(worker_id == trn) %>%
      sample_n(size = smplSize, replace = FALSE)
    
    # Train prediction model
    model <- Trainer(methodInfo$methodID, trnObs)
    
    # Analyse feature distribution and export model plot
    featDist <- model[1]$frame %>%
      select(var) %>%
      filter(var != "<leaf>") %>%
      mutate(train = trn)
    featureDist <- bind_rows(featureDist, featDist)
    
    # Export regression tree plot
    plotPath <- file.path("results/models",
                          paste(sysInfo$sysName, "_",
                                "CART", "_",
                                "machine", trn, "_",
                                "size", smplSize, "_",
                                "rep", smplRep,
                                ".png", sep = ""))
    # png(filename = plotPath, width = 1600, height = 900)
    # prp(model)
    # dev.off()
  
    } # for (n in 1:nrow(df1))

  # Summarize feature distributions of different machines
  grp_cols <- c("train", "var")
  dots <- lapply(grp_cols, as.symbol)

  distResults <-
    featureDist %>%
    group_by_(.dots = dots) %>%
    summarise(n = n())
  print(distResults)
  
  return(distResults)
}



debug_csv <- function(df, name) {
  debugFile <- file.path(debugPath, lpaste(name, ".csv", sep = ""))
  write.csv(df, file = debugFile, row.names = FALSE)
  return(df)
}


