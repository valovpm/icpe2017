library(boot)
library(dplyr)
library(ggplot2)
library(tidyr)

meanWrap <- function(data, indices) {
  d <- data[indices]
  return(mean(d))
}

meanLowerCI <- function(dt) {
  # Calculate confidence interval for mean
  bt <- boot(data = dt, statistic = meanWrap, R = 1000)
  ci <- boot.ci(bt, conf = 0.95, type = "norm")
  return(ci$norm[2])
}

meanUpperCI <- function(dt) {
  # Calculate confidence interval for mean
  bt <- boot(data = dt, statistic = meanWrap, R = 1000)
  ci <- boot.ci(bt, conf = 0.95, type = "norm")
  return(ci$norm[3])
}

meanCI <- function(dt) {
  # Calculate confidence interval for mean
  bt <- boot(data = dt, statistic = meanWrap, R = 1000)
  ci <- boot.ci(bt, conf = 0.95, type = "norm")
  return(ci)
}

medianWrap <- function(data, indices) {
  d <- data[indices]
  return(median(d))
}

AccuracyPlotting <- function() {
  print("AccuracyPlotting")
  # Analyse configurable systems using different regression methods
  for (system in systemNames) {
  # for (system in c("system01")) {
    for (method in methodNames) {
      # Load data
      accuracyFile <- file.path(accuracyPath,
                                paste(system, "_",
                                      method, ".csv", sep = ""))
      smData <- read.csv(accuracyFile,
                         na.strings = c(".", "NA", "", "?"),
                         strip.white = TRUE, encoding = "UTF-8")
      smData <- tbl_df(smData)
      print(smData)
      print("AccuracyPlotting")

      AccuracyBarPlots(system, method, smData)
    }
  }
}

AccuracyBarPlots <- function(system, method, smData) {
  
  # Filter and group data
  scale <- 10
  grpCols <- c("train.machine", "sample.size",
               "target.machine", "scale.size")
  
  gsmData <-
    smData %>%
    filter(scale.size == scale) %>%
    group_by_(.dots = grpCols) %>%
    summarise(error_mean = mean(l.relative.error),
              error_sd   = sd(l.relative.error),
              lower_ci   = meanLowerCI(l.relative.error),
              upper_ci   = meanUpperCI(l.relative.error))
  
  print(tbl_df(gsmData))

  # Split data by training machines
  machines     <- unique(gsmData$train.machine)
  machinesData <- split(gsmData, gsmData$train.machine)

  for (i in 1:length(machines)) {
    # Generate plot
    smPlot <- AccuracyBarPlot(machinesData[[i]])

    # Export plot
    plotPath <- file.path("results/accuracy_plots",
                          paste("L_",
                                system, "_",
                                method, "_",
                                "machine", machines[i], "_",
                                "scale", scale,
                                ".png", sep = ""))

    ggsave(smPlot, filename = plotPath, width = 16, height = 9)
  }
}

AccuracyBarPlot <- function(smData) {
  # Prepare data frame for plotting
  smData$sample.size <- factor(smData$sample.size,
                               # labels = c("1", "2", "3", "4", "5"))
                               # labels = c("1N", "2N", "3N", "4N", "5N"))
                               labels = c("3N", "4N", "5N"))
  smData$target.machine <- factor(smData$target.machine)

  # Plot data frame
  smPlot <-
    ggplot(data = smData, aes(x = sample.size,
                              y = error_mean,
                              fill = sample.size)) +
    geom_bar(stat = "identity", colour = "black") +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5) +
    facet_grid(.~target.machine) +
    xlab("Sampling Size") +
    ylab("Relative error (%)") +
    scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, 1)) +
    theme(axis.title   = element_text(size = 20, face = "bold"),
          axis.text.x  = element_text(size = 20, angle = 90, vjust = 0.5),
          axis.text.y  = element_text(size = 20),
          strip.text.x = element_text(size = 20)) +
    guides(fill = FALSE)
 
  return(smPlot)
}


