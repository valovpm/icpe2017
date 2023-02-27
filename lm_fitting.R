library(dplyr)
library(ggplot2)
library(tidyr)

source(file = "init.r")
source(file = "data.r")

LmFitting <- function() {
  # Variables to control which plots to generate
  compare.relative.order <- TRUE
  generate.individual <- FALSE
  generate.facet <- FALSE
  
  for (s in 1:length(systemNames)) {
    
    # Load data for a system
    sysDataInfo <- GetSysDataInfo(s)
    
    # Generate combinations of all machines
    combs <- expand.grid(1:length(sysDataInfo$machines),
                         1:length(sysDataInfo$machines))
    
    # Dataset for generating group plots
    facetCloudData <- NULL
    
    for (c in 1:nrow(combs)) {
      # Extract machine indexes
      m01 <- combs[c, 1]
      m02 <- combs[c, 2]
      
      # Generate cloud data based on machine data
      md01 <- sysDataInfo$machinesData[[m01]]
      md02 <- sysDataInfo$machinesData[[m02]]
      cloudData <- inner_join(md01, md02, by = sysDataInfo$features)
      
      # Update facet data
      if (generate.facet || compare.relative.order) {
        if (is.null(facetCloudData)) { facetCloudData <- cloudData }
        else { facetCloudData <- union(facetCloudData, cloudData) }
      }
      
      if (generate.individual) {
        # Generate sequences for plotting
        x.start <- floor(min(cloudData$PERF.x))
        x.end <- ceiling(max(cloudData$PERF.x))
        x.seq <- seq(from = x.start, to = x.end, by = 10)
        
        y.start <- floor(min(cloudData$PERF.y))
        y.end <- ceiling(max(cloudData$PERF.y))
        y.seq <- seq(from = y.start, to = y.end, by = 10)
        
        xseq <- seq(from = min(cloudData$PERF.x),
                    to = max(cloudData$PERF.x),
                    by = 1)
        
        # Generate linear prediction model
        linear <- lm(PERF.y~PERF.x, data = cloudData)
        linear.pred <- predict(linear, data.frame(PERF.x = xseq),
                               interval = 'confidence', level = 0.99)
        linear.data <- data.frame(x = xseq, y = linear.pred[,1])
        
        # Generate quadratic prediction model
        nlinear2 <- lm(PERF.y~poly(PERF.x, 2), data = cloudData)
        nlinear2.pred <- predict(nlinear2, data.frame(PERF.x = xseq),
                                 interval = 'confidence', level = 0.99)
        nlinear2.data <- data.frame(x = xseq, y = nlinear2.pred[,1])
        
        # Generate quadratic prediction model
        nlinear3 <- lm(PERF.y~poly(PERF.x, 3), data = cloudData)
        nlinear3.pred <- predict(nlinear3, data.frame(PERF.x = xseq),
                                 interval = 'confidence', level = 0.99)
        nlinear3.data <- data.frame(x = xseq, y = nlinear3.pred[,1])
        
        # Generate cloud plot
        cloudPlot <-
          ggplot(data = cloudData, aes(x = PERF.x, y = PERF.y)) +
          geom_point(aes(size = 2)) +
          geom_line(data = linear.data, aes(x = x, y = y, colour = 'blue', size = 1)) +
          geom_line(data = nlinear2.data, aes(x = x, y = y, colour = 'green', size = 1)) +
          geom_line(data = nlinear3.data, aes(x = x, y = y, colour = 'red', size = 1)) +
          xlab(paste("Machine", sysDataInfo$machines[[m01]])) +
          ylab(paste("Machine", sysDataInfo$machines[[m02]])) +
          scale_x_continuous(limits = c(x.start, x.end), breaks = x.seq) +
          scale_y_continuous(limits = c(y.start, y.end), breaks = y.seq) +
          scale_size_continuous(guide = FALSE) +
          scale_colour_manual(name = 'Polynomial',
                              values = c('blue' = 'blue', 'green' = 'green', 'red' = 'red'),
                              labels = c('1st degree (linear)', '2nd degree', '3rd degree'))
        #theme(legend.position = "none")
        # geom_abline(intersept = coef(linear)[1],
        #             slope = coef(linear)[2],
        #             col = "red") +
        # stat_smooth(method = "lm", col = "red")
        
        # Export cloud plot
        plotPath <- file.path("results/transfer_dist",
                              paste(sysDataInfo$sysName, "_",
                                    "machine", sysDataInfo$machines[m01], "_",
                                    "machine", sysDataInfo$machines[m02],
                                    ".png", sep = ""))
        
        ggsave(cloudPlot, filename = plotPath) # width = 16, height = 16)
      } # if (generate.individual)
    } # for (c in 1:nrow(combs))
    
    # Generate facet plot
    if (generate.facet) {
      cloudPlot <-
        ggplot(data = facetCloudData, aes(x = PERF.x, y = PERF.y)) +
        facet_grid(worker_id.x ~ worker_id.y) +
        geom_point() +
        xlab(paste("Machine", sysDataInfo$machines[[m01]])) +
        ylab(paste("Machine", sysDataInfo$machines[[m02]]))
      
      plotPath <- file.path("results/transfer_dist",
                            paste("facet_", sysDataInfo$sysName, ".png", sep = ""))
      
      ggsave(cloudPlot, filename = plotPath, width = 16, height = 9)
    }
    
    # Compare relative order
    if (compare.relative.order) {
      grpCols <- c("worker_id.x", "worker_id.y")
      
      distance <-
        facetCloudData %>%
        group_by_(.dots = grpCols) %>%
        # Optimal string aligment, (restricted Damerau-Levenshtein distance)
        # Levenshtein distance
        # Full Damerau-Levenshtein distance
        # Hamming distance
        # Longest common substring distance
        # q-gram distance
        summarise(n = n(),
                  osa_distance = DistrDistance(PERF.x, PERF.y, "osa"),
                  lv_distance = DistrDistance(PERF.x, PERF.y, "lv"),
                  dl_distance = DistrDistance(PERF.x, PERF.y, "dl"),
                  hm_distance = DistrDistance(PERF.x, PERF.y, "hamming"),
                  lcs_distance = DistrDistance(PERF.x, PERF.y, "lcs"),
                  qgram_distance = DistrDistance(PERF.x, PERF.y, "qgram"))
      
      # print(tbl_df(distance))
      print(distance)
    }
    
  } # for (s in 1:length(systemNames))
}


