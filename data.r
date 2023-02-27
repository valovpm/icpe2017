library(dplyr)
library(tidyr)

source("monitor.r")
source("scaling.r")

# Data and method preparation functions #######################################
GetSysDataInfo <- function(sysID) {
  # Loads data for a specified configurable software system
  # 
  # Args:
  #   sysID: ID of the system to be loaded

  # Load parsed system data
  sysDataFile <-
    file.path("data", "data_parsed",
              paste(systemNames[sysID], ".csv", sep = ""))

  sysData <-
    read.csv(sysDataFile,
             na.strings = c(".", "NA", "", "?"),
             strip.white = TRUE, encoding = "UTF-8")

  # Configurable software features:
  # Prepare data for analysis by choosing target column
  # TODO: REFACTOR PROPERLY!!!
  colnames(sysData)[which(names(sysData) == targetCol)] <- "PERF"
  sysData <- sysData[, !(colnames(sysData) %in% dropCol)]

  # Non-feature columns: worker_id, PERF
  features <- colnames(sysData)
  nonFeat <- c("worker_id", "PERF")
  features <- features[!(features %in% nonFeat)]
  featureNum <- length(features)

  # Generate dataset for each machine
  machines <- sort(unique(sysData[, "worker_id"]), decreasing = FALSE)
  machinesData <- split(sysData, sysData$worker_id)

  # 'samples' is a list of 'sampleNum' lists of length 'sampleRep'
  # 'samples' contains all necessary data samples for analysing the specified
  # system
  samples <- list()
  samples <- c(samples, 1:length(machines))

  sampleSizes <- 1:experParams$sampleSizes
  sampleSizes <- sampleSizes * featureNum

  # for each machine
  for (m in 1:length(machines)) {

    mSamples <- list()
    mSamples <- c(mSamples, 1:experParams$sampleSizes)

    # for each sampling size
    for (s in 1:experParams$sampleSizes) {

      equisizedSamples <- list()

      # for each sample repetition
      for (r in 1:experParams$sampleRep) {

        # generate actual sample for specific machine of a given size
        smpl <- sample(nrow(machinesData[[m]]), sampleSizes[s])
        equisizedSamples <- c(equisizedSamples, list(smpl))
      }

      mSamples[[s]] <- equisizedSamples
    }

    samples[[m]] <- mSamples
  }

  # Initialize list that will contain all system data information
  dataInfo <- list()
  dataInfo$sysID <- sysID                 # Configurable system ID
  dataInfo$sysName <- systemNames[sysID]  # Configurable system name
  dataInfo$sysData <- sysData             # Configurable system data

  dataInfo$features <- features           # Configuration features
  dataInfo$featureNum <- featureNum       # Number of configuration features

  dataInfo$machines <- machines           # List of machines
  dataInfo$machinesData <- machinesData   # System data split by machines
  
  dataInfo$sampleSizes <- sampleSizes     # List of sample sizes
  dataInfo$samples <- samples             # List (for each machine) of
                                          #   Lists (for each sample size) of
                                          #     Samples

  return(dataInfo)
}


