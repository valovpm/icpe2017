library(boot)
library(dplyr)
library(tidyr)
library(randtoolbox)
library(rpart.plot)
library(reshape2)
library(xtable)

source("data.r")
source("heat_tree.R")
source("monitor.r")
source("regmodels.r")

set.seed(1)

# Analysis functions ##########################################################
Accuracy <- function() {
  # Performs analysis of configurable software systems using different
  # regression methods

  # Analyse configurable systems using different regression methods
  for (s in 1:length(systemNames)) {
    for (m in 1:length(methodNames)) {

      # Get accuracy results
      sysInfo      <- GetSysDataInfo(s)
      methodInfo   <- GetMethodInfo(m)
      accuracy     <- AccuracySysMethod(sysInfo, methodInfo)
      accuracyFile <- file.path(accuracyPath,
                                paste(systemNames[s], "_",
                                      methodNames[m], ".csv", sep = ""))
      write.csv(accuracy, file = accuracyFile, row.names = FALSE)
      
      # Summarise accuracy results
      accuracy    <- read.csv(accuracyFile)
      summary     <- AccuracySummary(accuracy, systemNames[s])
      summaryFile <- file.path(accuracyPath,
                               paste(systemNames[s], "_",
                                     methodNames[m], "_summary.csv", sep = ""))
      write.csv(summary, file = summaryFile, row.names = FALSE)
      latexFile  <- file.path(accuracyPath,
                              paste(systemNames[s], "_",
                                    methodNames[m], "_summary.tex", sep = ""))
      AccuracyLatex(summary, latexFile)
      
      # Summarise difference results
      summary     <- DifferenceSummary(accuracy, systemNames[s])
      summaryFile <- file.path(accuracyPath,
                               paste(systemNames[s], "_",
                                     methodNames[m], "_summary_difference.csv", sep = ""))
      write.csv(summary, file = summaryFile, row.names = FALSE)
      latexFile  <- file.path(accuracyPath,
                              paste(systemNames[s], "_",
                                    methodNames[m], "_summary_difference.tex", sep = ""))
      DifferenceLatex(summary, latexFile)
      
      # Summarise timing results
      summary     <- TimeCostSummary(accuracy, systemNames[s])
      summaryFile <- file.path(accuracyPath,
                               paste(systemNames[s], "_",
                                     methodNames[m], "_summary_time.csv", sep = ""))
      write.csv(summary, file = summaryFile, row.names = FALSE)
      latexFile  <- file.path(accuracyPath,
                              paste(systemNames[s], "_",
                                    methodNames[m], "_summary_time.tex", sep = ""))
      TimeCostLatex(summary, latexFile)
    }
  }
}

AccuracySysMethod <- function(sysInfo, methodInfo) {
  
  # Filter machines
  if (FALSE) {
    if (sysInfo$sysName == "system01") {
      flt <- c(75, 78, 80, 81)
    }
    else if (sysInfo$sysName == "system02") {
      flt <- c(75, 81, 88, 103)
    }
    else if (sysInfo$sysName == "system03") {
      flt <- c(75, 99, 125, 157)
    }
    
    sysInfo$machines <- flt
    sysInfo$sysData  <-
      sysInfo$sysData %>%
      tbl_df() %>%
      filter(worker_id %in% flt)
  }
  
  # Generate design
  design <- expand.grid(
    1:experParams$repetitions,
    c(5, 10, 15), # 1:experParams$scaleSizes,
    sysInfo$machines,
    (3:experParams$sampleSizes) * sysInfo$featureNum,
    sysInfo$machines)

  colnames(design) <- c("repetition", "scaleSize", "target",
                        "sampleSize", "train")
  
  accuracy <- data.frame()

  for (n in 1:nrow(design)) {
    # Extract training machine, sampling size, and sampling repetition
    # Extract target machine, scaling size, and scaling repetition
    trn        <- design$train[n]
    smplSize   <- design$sampleSize[n]
    trgt       <- design$target[n]
    sclSize    <- design$scaleSize[n]
    repetition <- design$repetition[n]
    if (repetition %% 5 == 0) {
      Monitor(paste("Training machine:", trn, ";",
                    "Sample size:",      smplSize, ";",
                    "Target machine:",   trgt, ";",
                    "Scaling size:",     sclSize, ";",
                    "Repetition:",       repetition, ";"))
    }

    # Select TRAINING and TARGET datasets
    trnData <-
      sysInfo$sysData %>%
      filter(worker_id == trn)
    
    trgtData <-
      sysInfo$sysData %>%
      filter(worker_id == trgt)

    # Train CART01 on TRAINING machine:
    #   1. Extract sample of "smplSize" size
    #   2. Train CART using sample
    trnObs <-
      trnData %>%
      sample_n(size = smplSize, replace = FALSE)
    
    time.cost <-
      system.time(model <- Trainer(methodInfo$methodID, trnObs))
    
    # Train CART02 on TARGET machine:
    #   1. Select the same configurations on target machine
    #   2. Train CART using new sample
    trgtObs <-
      trgtData %>%
      inner_join(select_(trnObs, .dots = sysInfo$features),
                 by = sysInfo$features)
    
    trgtModel <- Trainer(methodInfo$methodID, trgtObs)
    
    # Export prediction models
    if (FALSE) {
      ExportModel(model,
                  paste("Machine_", trn, "_",
                        "SampleSize_", smplSize, "_",
                        "Target_", trgt, "_",
                        "Scaling_", sclSize, "_",
                        "Repetition_", repetition, "_trn", sep=""))
      
      ExportModel(trgtModel,
                  paste("Machine_", trn, "_",
                        "SampleSize_", smplSize, "_",
                        "Target_", trgt, "_",
                        "Scaling_", sclSize, "_",
                        "Repetition_", repetition, "_trgt", sep=""))
    }
    
    # Test CART01 and CART 02 on the TARGET machine
    # (except training configurations)
    tstData <-
      trgtData %>%
      setdiff(trgtObs)
    
    predicted <-
      tstData %>%
      select(-PERF) %>%
      predict(model, newdata = .)
    
    trgtPredicted <-
      tstData %>%
      select(-PERF) %>%
      predict(trgtModel, newdata = .)
    
    # Extract all actual performance values for target machine
    # (except training configurations)
    actual <-
      tstData %>%
      select(PERF)
    
    # print(length(predicted))
    # print(length(trgtPredicted))
    # print(nrow(actual))
    # print(length(predicted) == length(trgtPredicted) &&
    #       length(predicted) == nrow(actual))

    # Generate scaling coefficient and linear model:
    #   1. Join data for model building
    #   2. Calculate scale coefficient
    #   3. Generate linear model
    sdata <-
      inner_join(trnObs, trgtObs, by = sysInfo$features) %>%
      arrange(PERF.x) %>%
      .[sobol(sclSize, dim = 1) * nrow(.), ]

    trnSum  <- sum(sdata$PERF.x)
    trgtSum <- sum(sdata$PERF.y)
    scale   <- (trgtSum / trnSum)

    time.cost <-
      time.cost +
      system.time(lmodel <- lm(PERF.y~PERF.x, data=sdata))
    
    # Acquire results:
    #   1. Scale results using coefficient and linear model
    #   2. Calculate relative errors
    #   3. Update overall results
    scaled <- predicted * scale
    lscaled <- as.data.frame(predict(lmodel,
                                     newdata=data.frame(PERF.x=predicted),
                                     interval="confidence"))$fit
    
    errors  <- abs(actual - scaled) / actual * 100
    lerrors <- abs(actual - lscaled) / actual * 100
    terrors <-
      round(abs(
              lerrors -
              abs(actual - trgtPredicted) / actual * 100),
            digits = 2)
    
    accuracyRow <-
      cbind(trn, smplSize, trgt, sclSize,
            repetition, predicted, trgtPredicted,
            errors, lerrors, terrors,
            as.integer(time.cost[3] * 10^3))
    colnames(accuracyRow) <-
      c("train.machine", "sample.size", "target.machine", "scale.size",
        "repetition", "predicted", "trgtPredicted",
        "relative.error", "l.relative.error", "t.relative.error",
        "time.cost")
    accuracy <- bind_rows(accuracy, accuracyRow)

  } # for (n in 1:nrow(design))

  return(accuracy)
}

# Helper function for Sobol sampling
sample_n_sobol <- function(data, n) {
  sbl.seq <- sobol(n = n, dim = 1)
  sbl.seq <- sbl.seq * nrow(data)
  return(data[n, ])
}

AccuracySummary <- function(accuracy, sys.name) {
  
  flt <- c()
  
  if (sys.name == "system01") {
    flt <- c(75, 78, 80, 81)
  }
  else if (sys.name == "system02") {
    flt <- c(75, 81, 88, 103)
  }
  else if (sys.name == "system03") {
    flt <- c(75, 99, 125, 157)
  }
  
  grp.cols <- c("train.machine", "sample.size",
                "target.machine", "scale.size")
  grouped <-
    accuracy %>%
    tbl_df() %>%
    # filter(train.machine  %in% flt) %>%
    # filter(target.machine %in% flt) %>%
    group_by_(.dots = grp.cols) %>%
    summarise(error.mean  = mean(l.relative.error),
              error.std   = sd(l.relative.error),
              error.lower = meanLowerCI(l.relative.error),
              error.upper = meanUpperCI(l.relative.error))
  
  mutated <-
    grouped %>%
    mutate(error.mean  = as.character(format(round(error.mean,  digits = 1), nsmall = 1)),
           error.std   = as.character(format(round(error.std,   digits = 1), nsmall = 1)),
           error.lower = as.character(format(round(error.lower, digits = 1), nsmall = 1)),
           error.upper = as.character(format(round(error.upper, digits = 1), nsmall = 1))) %>%
    # mutate(error = paste("\\Centerstack{", error.mean, "$\\pm$", error.std, "\\\\",
                         # "[", error.lower, ", ", error.upper, "]", "}",  sep="")) %>%
    mutate(error = error.mean) %>%
    # select(-error.mean, -error.std)
    select(-error.mean, -error.std, -error.lower, -error.upper)
  
  suppressWarnings(
    casted <-
      mutated %>%
      dcast(target.machine + scale.size ~ train.machine + sample.size, value.var = "error"))
  
  return(casted)
}

AccuracyLatex <- function(accuracy, print.file) {
  x.tbl <- xtable(accuracy)
  
  hl <- 1:nrow(accuracy)
  hl <- hl[hl %% 3 == 0]
  hlines <- c(-1, 0, hl)
  
  print.xtable(x.tbl,
               file = print.file,
               hline.after = hlines,
               sanitize.text.function = identity,
               scalebox = 0.5)
}

DifferenceSummary <- function(accuracy, sys.name) {
  
  flt <- c()
  
  if (sys.name == "system01") {
    flt <- c(75, 78, 80, 81)
  }
  else if (sys.name == "system02") {
    flt <- c(75, 81, 88, 103)
  }
  else if (sys.name == "system03") {
    flt <- c(75, 99, 125, 157)
  }
  
  grp.cols <- c("train.machine", "sample.size",
                "target.machine", "scale.size")
  grouped <-
    accuracy %>%
    tbl_df() %>%
    filter(train.machine  %in% flt) %>%
    filter(target.machine %in% flt) %>%
    group_by_(.dots = grp.cols) %>%
    summarise(error.mean  = mean(t.relative.error),
              error.std   = sd(t.relative.error),
              error.lower = meanLowerCI(t.relative.error),
              error.upper = meanUpperCI(t.relative.error))
  
  mutated <-
    grouped %>%
    mutate(error.mean  = as.character(format(round(error.mean,  digits = 1), nsmall = 1)),
           error.std   = as.character(format(round(error.std,   digits = 1), nsmall = 1)),
           error.lower = as.character(format(round(error.lower, digits = 1), nsmall = 1)),
           error.upper = as.character(format(round(error.upper, digits = 1), nsmall = 1))) %>%
    # mutate(error = paste("\\Centerstack{", error.mean, "$\\pm$", error.std, "\\\\",
    # "[", error.lower, ", ", error.upper, "]", "}",  sep="")) %>%
    mutate(error = error.mean) %>%
    # select(-error.mean, -error.std)
    select(-error.mean, -error.std, -error.lower, -error.upper)
  
  suppressWarnings(
    casted <-
      mutated %>%
      dcast(target.machine + scale.size ~ train.machine + sample.size, value.var = "error"))
  
  return(casted)
}

DifferenceLatex <- function(accuracy, print.file) {
  x.tbl <- xtable(accuracy)
  
  hl <- 1:nrow(accuracy)
  hl <- hl[hl %% 3 == 0]
  hlines <- c(-1, 0, hl)
  
  print.xtable(x.tbl,
               file = print.file,
               hline.after = hlines,
               sanitize.text.function = identity,
               scalebox = 0.5)
}

TimeCostSummary <- function(accuracy, sys.name) {
  
  flt <- c()
  
  if (sys.name == "system01") {
    flt <- c(75, 78, 80, 81)
  }
  else if (sys.name == "system02") {
    flt <- c(75, 81, 88, 103)
  }
  else if (sys.name == "system03") {
    flt <- c(75, 99, 125, 157)
  }
  
  grp.cols <- c("train.machine", "sample.size",
                "target.machine", "scale.size")
  grouped <-
    accuracy %>%
    tbl_df() %>%
    filter(train.machine  %in% flt) %>%
    filter(target.machine %in% flt) %>%
    group_by_(.dots = grp.cols) %>%
    summarise(time.cost.mean = mean(time.cost),
              time.cost.sd   = sd(time.cost)) %>%
    mutate(time.cost.mean  = as.character(format(round(time.cost.mean, digits = 1), nsmall = 1)),
           time.cost.sd    = as.character(format(round(time.cost.sd,   digits = 1), nsmall = 1))) %>%
    mutate(time.cost = paste(time.cost.mean, "$\\pm$", time.cost.sd,  sep="")) %>%
    select(-time.cost.mean, -time.cost.sd)
  
  suppressWarnings(
    casted <-
      grouped %>%
      dcast(target.machine + scale.size ~ train.machine + sample.size, value.var = "time.cost"))
  
  return(casted)
}

TimeCostLatex <- function(time.cost, print.file) {
  x.tbl <- xtable(time.cost)
  
  hl <- 1:nrow(time.cost)
  hl <- hl[hl %% 3 == 0]
  hlines <- c(-1, 0, hl)
  
  print.xtable(x.tbl,
               file = print.file,
               hline.after = hlines,
               sanitize.text.function = identity,
               scalebox = 0.5)
}



# Utility functions
ExportModel <- function(model, filename) {
  # Output png file for CART
  outfile <- file.path("results", "regression_models",
                       paste(filename, ".png", sep = ""))
  png(outfile, width = 1100, height = 680)
  HeatTree(model, low.is.green = TRUE, type = 2, extra = 101,
           varlen = 0, faclen = 0, fallen.leaves = TRUE)
  dev.off()
}

debug_csv <- function(df, name) {
  debugFile <- file.path(debugPath, lpaste(name, ".csv", sep = ""))
  write.csv(df, file = debugFile, row.names = FALSE)
  return(df)
}

meanWrap <- function(data, indices) {
  d <- data[indices]
  return(mean(d))
}

meanLowerCI <- function(dt) {
  # Calculate confidence interval for mean
  bt <- boot(data = dt, statistic = meanWrap, R = 100)
  ci <- boot.ci(bt, conf = 0.95, type = "norm")
  return(ci$norm[2])
}

meanUpperCI <- function(dt) {
  # Calculate confidence interval for mean
  bt <- boot(data = dt, statistic = meanWrap, R = 100)
  ci <- boot.ci(bt, conf = 0.95, type = "norm")
  return(ci$norm[3])
}

meanCI <- function(dt) {
  # Calculate confidence interval for mean
  bt <- boot(data = dt, statistic = meanWrap, R = 100)
  ci <- boot.ci(bt, conf = 0.95, type = "norm")
  return(ci)
}


