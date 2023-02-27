library(dplyr)
library(ggplot2)
library(randtoolbox)
library(stringdist)
library(tidyr)

source(file = "init.r")
source(file = "data.r")

DistrDistance <- function(x, y, metric) {
  # Enumerate and order input vectors
  df1 <-
    x %>%
    as.data.frame(.) %>%
    mutate(ID1 = row_number()) %>%
    arrange(x)
  
  df2 <-
    y %>%
    as.data.frame(.) %>%
    mutate(ID2 = row_number()) %>%
    arrange(y)
  
  # Convert vectors to strings and calculate distance
  str1 <- intToUtf8(df1$ID1)
  str2 <- intToUtf8(df2$ID2)
  distance <- stringdist(str1, str2, method=metric)
  return(distance)
}

GetMSE <- function(linear.model, test.data) {
  test.pred <- predict(linear.model, newdata=test.data)
  mse <- sum((test.pred - test.data$PERF.y)^2) / nrow(test.data)
  return(mse)
}

GetSobolSeq <- function(sbl.source) {
  # Initialize training data and update source data
  sbl.n <- nrow(sbl.source)
  sbl.seq <- round(sobol(n = sbl.n, dim = 1) * sbl.n)
  sbl.leftovers <- setdiff(1:sbl.n, sbl.seq)
  sbl.repeat <- c()
  
  for (i in 1:length(sbl.seq)) {
    # If element is repeated in Sobol sequece,
    # replace this element with another from leftovers
    if (sbl.seq[i] %in% sbl.repeat) {
      sbl.seq[i] <- sbl.leftovers[1]
      sbl.leftovers <- sbl.leftovers[-1]
    }
    # If element is NOT repeated in Sobol sequence,
    # mark this element as repeated for future
    else {
      sbl.repeat <- c(sbl.repeat, sbl.seq[i])
    }
  }
  
  return(sbl.seq)
}

GetStrataSeq <- function(str.source) {
  str.n <- nrow(str.source)
  str.count <- 10
  str.size <- str.n %/% str.count
  str.rmnd <- str.n %% str.count
  str.sizes <- rep(str.size, str.count)
  
  # If str.n is not divisible by str.count
  if (str.rmnd > 0) {
    for (i in 1:str.rmnd) {
      str.sizes[i] <- str.sizes[i] + 1
    }
  }
  
  str.groups <- rep(1:str.count, str.sizes)
  str.stratas <- split(1:str.n, str.groups)
  for (i in 1:str.count) { str.stratas[[i]] <- sample(str.stratas[[i]]) }
  attributes(str.stratas) <- list(names = names(str.stratas),
                                  row.names = 1:length(str.stratas[[1]]),
                                  class='data.frame')
  str.seq <- c()
  for (i in 1:nrow(str.stratas)) { 
    str.seq <- c(str.seq, as.numeric(str.stratas[i,]))
  }
  str.seq <- str.seq[!is.na(str.seq)]
  
  if (FALSE) {
    print(str.sizes)
    print(str.groups)
    print(str.stratas)
    print(str.seq)
  }
  
  return(str.seq)
}

GetMinMaxSeq <- function(mm.source) {
  temp.seq <- 1:nrow(mm.source)
  mm.seq <- c()
  
  for (i in 1:nrow(mm.source)) {
    # Add a minimum to the result
    if (i %% 2 == 1) {
      mm.seq <- c(mm.seq, temp.seq[1])
      temp.seq <- temp.seq[-1]
    }
    # Add a maximum to the result
    else {
      mm.seq <- c(mm.seq, temp.seq[length(temp.seq)])
      temp.seq <- temp.seq[-length(temp.seq)]
    }
  }
  
  return(mm.seq)
}

GetMinMedianMaxSeq <- function(mmm.source) {
  n <- nrow(mmm.source)
  temp.seq <- 1:n
  mmm.seq <- c()
  
  for (i in 1:n) {
    # Get minimal element
    if ((i-1) %% 3 == 0) {
      mmm.seq <- c(mmm.seq, temp.seq[1])
      temp.seq <- temp.seq[-1]
    }
    # Get median element
    else if ((i-2) %% 3 == 0) {
      median.index <- ceiling(length(temp.seq)/2)
      mmm.seq <- c(mmm.seq, temp.seq[median.index])
      temp.seq <- temp.seq[-median.index]
    }
    # Get max element
    else {
      mmm.seq <- c(mmm.seq, temp.seq[length(temp.seq)])
      temp.seq <- temp.seq[-length(temp.seq)]
    }
  }
  
  return(mmm.seq)
}

GetNextRow <- function(training.size, train.data, test.data) {
  # Generate linear prediction model and MSE
  linear <- lm(PERF.y~PERF.x, data = train.data)
  train.mse <- GetMSE(linear, train.data)
  test.mse  <- GetMSE(linear, test.data)
  
  # Generate a new row for the learning curve
  new.row <- c(training.size, train.mse, test.mse)
  return(new.row)
}

DistTransfer <- function() {
  # Variables to control which plots to generate
  compare.relative.order        <- TRUE
  generate.individual           <- TRUE
  generate.facet                <- TRUE
  generate.learning.curve       <- TRUE
  generate.learning.curve.total <- TRUE
  
  for (s in 1:length(systemNames)) {
    
    # Prepare for analysis:
    #   1. Load data for a system
    #   2. Prepare combinations of all machines
    sys.data.info <- GetSysDataInfo(s)
    combs <- expand.grid(1:length(sys.data.info$machines),
                         1:length(sys.data.info$machines))
    
    # Utility data frames:
    facetCloudData  <- data.frame()
    rnd.curve.total <- data.frame()
    sbl.curve.total <- data.frame()
    str.curve.total <- data.frame()
    mm.curve.total  <- data.frame()
    mmm.curve.total <- data.frame()
    
    for (c in 1:nrow(combs)) {
      # Prepare cloud data based on machines data
      m01  <- combs[c, 1]
      m02  <- combs[c, 2]
      md01 <- sys.data.info$machinesData[[m01]]
      md02 <- sys.data.info$machinesData[[m02]]
      cloudData <- inner_join(md01, md02, by = sys.data.info$features)
      
      # Update facet data
      if (generate.facet || compare.relative.order) {
        if (nrow(facetCloudData) == 0) { facetCloudData <- cloudData }
        else { facetCloudData <- union(facetCloudData, cloudData) }
      }
      
      # Fit linear and polynomial models
      if (generate.individual) {
        # Generate sequences for plotting
        x.start <- floor(min(cloudData$PERF.x))
        x.end   <- ceiling(max(cloudData$PERF.x))
        x.seq   <- seq(from = x.start, to = x.end, by = 10)
        
        y.start <- floor(min(cloudData$PERF.y))
        y.end   <- ceiling(max(cloudData$PERF.y))
        y.seq   <- seq(from = y.start, to = y.end, by = 10)
        
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
          geom_line(data = nlinear3.data, aes(x = x, y = y, colour = 'red', size = 1)) +
          geom_line(data = nlinear2.data, aes(x = x, y = y, colour = 'green', size = 1)) +
          geom_line(data = linear.data,   aes(x = x, y = y, colour = 'blue', size = 1)) +
          xlab(paste("Machine", sys.data.info$machines[[m01]])) +
          ylab(paste("Machine", sys.data.info$machines[[m02]])) +
          scale_x_continuous(limits = c(x.start, x.end), breaks = x.seq) +
          scale_y_continuous(limits = c(y.start, y.end), breaks = y.seq) +
          scale_size_continuous(guide = FALSE) +
          theme(text = element_text(size=25),
                axis.text.x = element_text(angle=90, vjust=0.5),
                legend.position = "top") +
          scale_colour_manual(name = 'Polynomials:',
                              values = c('blue' = 'blue', 'green' = 'green', 'red' = 'red'),
                              labels = c('1st degree', '2nd degree', '3rd degree'))
          
          #theme(legend.position = "none")
          # geom_abline(intersept = coef(linear)[1],
          #             slope = coef(linear)[2],
          #             col = "red") +
          # stat_smooth(method = "lm", col = "red")
        
        # Export cloud plot
        plotPath <- file.path("results/dist_transfer",
                              paste(sys.data.info$sysName, "_",
                                    "machine", sys.data.info$machines[m01], "_",
                                    "machine", sys.data.info$machines[m02],
                                    ".png", sep = ""))
        
        ggsave(cloudPlot, filename = plotPath, width = 20, height = 20, units="cm")
      } # if (generate.individual)
      
      # Fit linear model to random data
      if (generate.learning.curve) {
        # Prepare data for learning curve generation
        source.data <- cloudData %>% arrange(PERF.x)
        test.data   <- cloudData
        
        # For each sampling method create the following data frames:
        #   1. Source data, to be sampled for values
        #   2. Training data, containing sampled values
        #   3. Testing data
        #   4. Learning curve data for a particular combination of machines
        #   5. Learning curve data for all machines
        
        # Random sampling method
        rnd.train       <- data.frame()
        rnd.curve       <- data.frame()
        rnd.seq         <- sample(1:nrow(source.data),
                                  size=nrow(source.data),
                                  replace=F)
        
        # Sobol sampling method
        sbl.train       <- data.frame()
        sbl.curve       <- data.frame()
        sbl.seq         <- GetSobolSeq(source.data)
        
        # Stratified sampling method
        str.train       <- data.frame()
        str.curve       <- data.frame()
        str.seq         <- GetStrataSeq(source.data)
        
        # Min-max (mm) sampling method
        mm.train        <- data.frame()
        mm.curve        <- data.frame()
        mm.seq          <- GetMinMaxSeq(source.data)
        
        # Min-median-max (mmm) sampling method
        mmm.train       <- data.frame()
        mmm.curve       <- data.frame()
        mmm.seq         <- GetMinMedianMaxSeq(source.data)
        
        # Generate combinations of sampling size and iterations
        for (size in 2:nrow(cloudData)) {
          
          # Random sampling
          rnd.train       <- source.data[rnd.seq[c(1:size)], ]
          new.row         <- GetNextRow(size, rnd.train, test.data)
          rnd.curve       <- rbind(rnd.curve, new.row)
          rnd.curve.total <- rbind(rnd.curve.total, new.row)
          
          # Sobol sampling
          sbl.train       <- source.data[sbl.seq[c(1:size)], ]
          new.row         <- GetNextRow(size, sbl.train, test.data)
          sbl.curve       <- rbind(sbl.curve, new.row)
          sbl.curve.total <- rbind(sbl.curve.total, new.row)
          
          # Stratified sampling
          str.train       <- source.data[str.seq[c(1:size)], ]
          new.row         <- GetNextRow(size, str.train, test.data)
          str.curve       <- rbind(str.curve, new.row)
          str.curve.total <- rbind(str.curve.total, new.row)
          
          # Min-max sampling
          mm.train        <- source.data[mm.seq[c(1:size)], ]
          new.row         <- GetNextRow(size, mm.train, test.data)
          mm.curve        <- rbind(mm.curve, new.row)
          mm.curve.total  <- rbind(mm.curve.total, new.row)
          
          # Min-median-max sampling
          mmm.train       <- source.data[mmm.seq[c(1:size)], ]
          new.row         <- GetNextRow(size, mmm.train, test.data)
          mmm.curve       <- rbind(mmm.curve, new.row)
          mmm.curve.total <- rbind(mmm.curve.total, new.row)
        }
        
        # Prepare curve data for plotting
        colnames(rnd.curve) <- c("sample.size", "rnd.train.mse", "rnd.test.mse")
        colnames(sbl.curve) <- c("sample.size", "sbl.train.mse", "sbl.test.mse")
        colnames(str.curve) <- c("sample.size", "str.train.mse", "str.test.mse")
        colnames(mm.curve)  <- c("sample.size", "mm.train.mse", "mm.test.mse")
        colnames(mmm.curve) <- c("sample.size", "mmm.train.mse", "mmm.test.mse")
        
        learning.curve <-
          inner_join(rnd.curve, mm.curve, by="sample.size") %>%
          inner_join(mmm.curve, by="sample.size") %>%
          inner_join(sbl.curve, by="sample.size") %>%
          inner_join(str.curve, by="sample.size") %>%
          filter(sample.size > 0)
        
        if(T){
        # Generate learning curve plot
        colours <- c('rnd.train'='red',   'Pseudorandom'='darkred',
                     'mm.train'='blue',   'mm.test'='darkblue',
                     'mmm.train'='green', 'mmm.test'='darkgreen',
                     'sbl.train'='blue',  'Sobol'='darkblue',
                     'str.train'='green', 'Stratified'='darkgreen')
        
        curvePlot <-
          ggplot() +
          # geom_line(data = learning.curve, aes(x = sample.size, y = rnd.train.mse, colour = 'rnd.train')) +
          geom_line(data = learning.curve, aes(x = sample.size, y = rnd.test.mse,  colour = 'Pseudorandom')) +
          # geom_line(data = learning.curve, aes(x = sample.size, y = sbl.train.mse,  colour = 'sbl.train')) +
          geom_line(data = learning.curve, aes(x = sample.size, y = sbl.test.mse,   colour = 'Sobol')) +
          # geom_line(data = learning.curve, aes(x = sample.size, y = str.train.mse,  colour = 'str.train')) +
          geom_line(data = learning.curve, aes(x = sample.size, y = str.test.mse,   colour = 'Stratified')) +
          # geom_line(data = learning.curve, aes(x = sample.size, y = mm.train.mse,  colour = 'mm.train')) +
          # geom_line(data = learning.curve, aes(x = sample.size, y = mm.test.mse,   colour = 'mm.test')) +
          # geom_line(data = learning.curve, aes(x = sample.size, y = mmm.train.mse, colour = 'mmm.train')) +
          # geom_line(data = learning.curve, aes(x = sample.size, y = mmm.test.mse,  colour = 'mmm.test')) +
          # xlab("Training size") +
          xlab(expression("Size of the training sample," ~ size(bold(C)[both]))) +
          ylab("Mean Squared Error (MSE)") +
          # scale_x_continuous(limits = c(x.start, x.end), breaks = x.seq) +
          # scale_y_continuous(limits = c(0, 20)) +
          # scale_size_continuous(guide = FALSE) +
          scale_colour_manual(name = 'Sampling:',
                              values = colours) +
          theme(text = element_text(size=10),
                legend.position = "top") +
          coord_cartesian(xlim = c(0,30), ylim = c(0, 8))
        
        # Export curve plot
        plotPath <- file.path("results/transfer_curve",
                              paste(sys.data.info$sysName, "_",
                                    "machine", sys.data.info$machines[m01], "_",
                                    "machine", sys.data.info$machines[m02],
                                    ".png", sep = ""))
        
        ggsave(curvePlot, filename = plotPath, width = 10, height = 10, units = "cm")
        }
      } # if (generate.learning.curve)
    } # for (c in 1:nrow(combs))
    
    # Generate facet plot
    if (generate.facet) {
      # print(tbl_df(facetCloudData))
      
      cloudPlot <-
        ggplot(data = facetCloudData, aes(x = PERF.x, y = PERF.y)) +
        facet_grid(worker_id.y ~ worker_id.x) +
        geom_point() +
        xlab("Training Machines") +
        ylab("Target Machines") +
        theme(axis.text.y  = element_text(size = 15),
              axis.text.x  = element_text(size = 15, angle = 90, vjust = 0.5),
              axis.title   = element_text(size = 20, face = "bold"),
              strip.text.x = element_text(size = 20, face = "bold"),
              strip.text.y = element_text(size = 20, face = "bold"))
      
      plotPath <- file.path("results/dist_transfer",
                            paste("facet_", sys.data.info$sysName, ".png", sep = ""))
      
      ggsave(cloudPlot, filename = plotPath, width = 16, height = 9)
    }
    
    # Generate averaged learning curve
    if (generate.learning.curve.total) {
      colnames(rnd.curve.total) <- c("sample.size", "rnd.train.mse", "rnd.test.mse")
      colnames(sbl.curve.total) <- c("sample.size", "sbl.train.mse", "sbl.test.mse")
      colnames(str.curve.total) <- c("sample.size", "str.train.mse", "str.test.mse")
      colnames(mm.curve.total)  <- c("sample.size", "mm.train.mse", "mm.test.mse")
      colnames(mmm.curve.total) <- c("sample.size", "mmm.train.mse", "mmm.test.mse")
      
      # Prepare data for summarised curve plot
      rnd.curve.total <-
        rnd.curve.total %>%
        group_by(sample.size) %>%
        summarise(rnd.train.mse = mean(rnd.train.mse),
                  rnd.test.mse  = mean(rnd.test.mse))
      
      sbl.curve.total <-
        sbl.curve.total %>%
        group_by(sample.size) %>%
        summarise(sbl.train.mse = mean(sbl.curve.total),
                  sbl.test.mse  = mean(sbl.test.mse))
      
      str.curve.total <-
        str.curve.total %>%
        group_by(sample.size) %>%
        summarise(str.train.mse = mean(str.curve.total),
                  str.test.mse  = mean(str.test.mse))
      
      mm.curve.total <-
        mm.curve.total %>%
        group_by(sample.size) %>%
        summarise(mm.train.mse = mean(mm.curve.total),
                  mm.test.mse  = mean(mm.test.mse))
      
      mmm.curve.total <-
        mmm.curve.total %>%
        group_by(sample.size) %>%
        summarise(mmm.train.mse = mean(mmm.curve.total),
                  mmm.test.mse  = mean(mmm.test.mse))
      
      learning.curve.total <-
        inner_join(rnd.curve.total, mm.curve.total, by="sample.size") %>%
        inner_join(mmm.curve.total, by="sample.size") %>%
        inner_join(sbl.curve.total, by="sample.size") %>%
        inner_join(str.curve.total, by="sample.size") %>%
        filter(sample.size > 0)
      
      # Generate learning curve plot
      colours <- c('rnd.train'='red',   'Pseudorandom'='darkred',
                   'sbl.train'='blue',  'Sobol'='darkblue',
                   'str.train'='green', 'Stratified'='darkgreen',
                   'mm.train'='blue',   'mm.test'='darkblue',
                   'mmm.train'='green', 'mmm.test'='darkgreen')
      
      curvePlot <-
        ggplot() +
        # geom_line(data = learning.curve, aes(x = sample.size, y = rnd.train.mse, colour = 'rnd.train')) +
        geom_line(data = learning.curve.total, aes(x = sample.size, y = rnd.test.mse,  colour = 'Pseudorandom')) +
        # geom_line(data = learning.curve, aes(x = sample.size, y = sbl.train.mse,  colour = 'sbl.train')) +
        geom_line(data = learning.curve.total, aes(x = sample.size, y = sbl.test.mse,   colour = 'Sobol')) +
        # geom_line(data = learning.curve, aes(x = sample.size, y = str.train.mse,  colour = 'str.train')) +
        geom_line(data = learning.curve.total, aes(x = sample.size, y = str.test.mse,   colour = 'Stratified')) +
        # geom_line(data = learning.curve, aes(x = sample.size, y = mm.train.mse,  colour = 'mm.train')) +
        # geom_line(data = learning.curve, aes(x = sample.size, y = mm.test.mse,   colour = 'mm.test')) +
        # geom_line(data = learning.curve, aes(x = sample.size, y = mmm.train.mse, colour = 'mmm.train')) +
        # geom_line(data = learning.curve, aes(x = sample.size, y = mmm.test.mse,  colour = 'mmm.test')) +
        xlab(expression("Size of the training sample," ~ size(bold(C)[both]))) +
        ylab("Mean Squared Error (MSE)") +
        # scale_x_continuous(limits = c(x.start, x.end), breaks = x.seq) +
        # scale_y_continuous(limits = c(0, 20)) +
        # scale_size_continuous(guide = FALSE) +
        scale_colour_manual(name = 'Sampling: ',
                            values = colours) +
        theme(text = element_text(size=10),
              legend.position = "top") +
        coord_cartesian(xlim = c(0,50), ylim = c(3, 20))
      
      # Export curve plot
      plotPath <- file.path("results/transfer_curve_total",
                            paste(sys.data.info$sysName, ".png", sep = ""))
      
      ggsave(curvePlot, filename = plotPath, width = 10, height = 10, units = "cm")
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


