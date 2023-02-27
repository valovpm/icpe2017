library(boot)
library(dplyr)
library(ggplot2)
library(grid)
library(tidyr)

DistFeaturesPlot <- function() {
  # Analyse configurable systems using different regression methods
  for (system in systemNames) {
    for (method in methodNames) {
      # Load data
      distFile <- file.path(distPath,
                            paste(system, "_",
                                  method, ".csv", sep = ""))
      smData <- read.csv(distFile,
                         na.strings = c(".", "NA", "", "?"),
                         strip.white = TRUE, encoding = "UTF-8")
      smData <- tbl_df(smData)

      print(distFile)
      print(smData)

      DrawBarPlots(system, method, smData)
    }
  }
}

DrawBarPlots <- function(system, method, smData) {
  # Filter and group data
  grpCols <- c("train", "var")
  gsmData <-
    smData %>%
    mutate(train = paste("Machine ", train, sep="")) %>%
    group_by_(.dots = grpCols) %>%
    summarise(n = sum(n))

  # Generate plot
  smPlot <- DrawBarPlot(gsmData)

  # Export plot
  plotPath <- file.path("results/dist_features_plots_bar",
                        paste(system, "_", method, ".png", sep = ""))

  ggsave(smPlot, filename = plotPath, width = 10, height = 10, units = "cm")
}

DrawBarPlot <- function(smData) {
  
  # Prepare data frame for plotting
  smData$train       <- factor(smData$train)
  smData$var         <- factor(smData$var)
  nlevels            <- length(levels(smData$var))
  new.levels         <- paste("f[", as.character(1:nlevels), "]", sep="")
  levels(smData$var) <- new.levels
  print(labels)

  # Plot data frame
  smPlot <-
    ggplot(data = smData, aes(x = train, y = n)) +
    geom_bar(aes(fill = train), stat = "identity") +
    facet_grid(. ~ var, labeller = label_parsed) +
    xlab("Features") +
    ylab("Frequency of appearance") +
    theme(axis.text.y     = element_text(size = 10),
          axis.text.x     = element_text(size = 5, angle = 90, hjust = 1),
          axis.title      = element_text(size = 12, face = "bold"),
          strip.text.x    = element_text(size = 6),
          legend.key.size = unit(0.2, "cm"),
          legend.position = "top",
          legend.title    = element_text(size = 6),
          legend.text     = element_text(size = 6)) +
    guides(fill = guide_legend(title = "Machines", nrow=3, byrow=TRUE)) +
    scale_x_discrete(breaks=NULL)
    
  return(smPlot)
}


