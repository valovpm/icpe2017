library(dplyr)
library(ggplot2)
library(randtoolbox)
library(reshape2)
library(tidyr)

source(file = "init.r")
source(file = "data.r")

DistPerf <- function() {
  # Variables to control which plots to generate
  for (s in 1:length(systemNames)) {
    
    # Load data for a system
    data.info <- GetSysDataInfo(s)
    machines <- data.info$machines
    sys.data <- data.info$sysData
    sys.data <- split(sys.data, sys.data$worker_id)
    
    for (i in 1:length(sys.data)) {
      # Get machine
      worker.id <- sys.data[[i]]$worker_id[1]

      # Order and drop columns
      df <-
        sys.data[[i]] %>%
        arrange(PERF) %>%
        subset(., select=-c(worker_id))
      
      # Rename columns and save frame back to the list
      colnames(df)[which(names(df) == "PERF")] <- paste("Machine ", worker.id, sep="")
      sys.data[[i]] <- df
    }
    
    # Combine all individual frames:
    #   1. Select the first frame as a seed for joining
    #   2. Join all frames by configuration features
    #   3. Drop configuration features from data
    joined <- sys.data[[1]]
    lapply(sys.data[-1], function(d) joined <<- left_join(joined, d, by=data.info$features))
    joined <- joined[, -which(names(joined) %in% data.info$features)]
    
    #   4. Enumerate all configurations
    #   5. Melt data (necessary for plotting)
    #   6. Rename columns 
    joined$config.id <- 1:nrow(joined)
    joined <- melt(joined, id = c("config.id"))
    colnames(joined) <- c("config.id", "Machines", "perf")
    
    # Plot performance distributions
    joined.plot <-
      ggplot(data = joined) +
      xlab("Configurations") +
      ylab("Measured performance value (s)")
    
    for (i in 1:length(data.info$machines)) {
      m <- data.info$machines[i]
      joined.plot <-
        joined.plot +
        geom_line(aes(x=config.id, y=perf, group=Machines, color=Machines))
    }

    # Plot formatting
    joined.plot <-
      joined.plot +
      theme(axis.text.y     = element_text(size = 10),
            axis.text.x     = element_text(size = 10),
            axis.title      = element_text(size = 12, face = "bold"),
            strip.text.x    = element_text(size = 6),
            legend.key.size = unit(0.2, "cm"),
            legend.position = "top",
            legend.title    = element_text(size = 6, face = "bold"),
            legend.text     = element_text(size = 6)) +
      guides(fill = guide_legend(title = "Machines", nrow=3, byrow=TRUE))
      # guides(colour = FALSE)
      # guides(colour = guide_legend(nrow=3, byrow=TRUE))
    
    # Export curve plot
    print(joined.plot)
    plotPath <- file.path("results/dist_perf",
                          paste(data.info$sysName, ".png", sep = ""))
    ggsave(joined.plot, filename = plotPath, width = 10, height = 10, units = "cm")
  } # for (s in 1:length(systemNames))
}


