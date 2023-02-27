source("monitor.r")

Init <- function() {
  # Aggregated function for initializing all parameters
  InitMonitorParams()
  InitMethodsParams()
  InitParamsExperiment()
  Monitor("Initialization is complete")
}

InitMonitorParams <- function() {
  # Specifies global parameters used by debugging functions

  # List specifies debugging levels for functions
  # Messages from functions not in the list will not be printed
  # Messages from functions that have negative level will not be printed
  # Messages with level exceeding specified in the list for function will not
  # be printed
  funLevels <<- list(# accuracy.r
                     AccuracySysMethod = 2,
    
                     # analysis.r
                     AnalyseSystem = -2,
                     AnalyseSystemMethod = 2,
                     GetErrors = -2,

                     # data.r
                     FilterMachineData = -2,

                     # scaling.r
                     GetSameRows = -2,
                     GetScale = -2,

                     AnalyseError = 2,
                     AnalyseParamsSens = -2,
                     GetPredError = -2,
                     Init = -2,
                     ModelStub = -2,
                     TestMonitor = -2,
                     TrainModel = -2,
                     TrainCart = -2)
}

InitMethodsParams <- function() {
  # Initializes parameter ranges for CART regression method
  methodNames <<- list(cart = "CART")

  # Initialize CART parameters
  cartNames   <- c("minsplit", "minbucket", "maxdepth", "complexity")
  cartVals    <- c(2, 1,  10, 0)
  cart        <- list(names = cartNames, vals = cartVals)

  methodsParams <<- list(cart = cart)
}

InitParamsExperiment <- function() {
  # Initializes parameters relevant to the whole experiment
  systemNames   <<- c("system01", "system02", "system03")
  systemAliases <<- c("XZ", "x264", "SQLite")
  
  # Target metrics
  targets <<- c("time", "size")
  targetCol <<- "time"
  dropCol <<- c("size")

  experParams <<- list()          # List of global experimental constants
  experParams$sampleSizes <<- 5   # Sampling sizes to generate (1*N, 2*N, ...)
  experParams$sampleRep   <<- 10  # Number of equisized samples to generate
  experParams$scaleSizes  <<- 10  # Scaling sizes to use (1 obs, 2 obs, ...)
  experParams$scaleRep    <<- 10  # Number of times to repeat scaling
  experParams$repetitions <<- 10  # Number of times to repeat scaling

  accuracyPath   <<- file.path("results", "accuracy")
  distPath       <<- file.path("results", "dist_features")
  debugPath      <<- file.path("debug")
  hwPath         <<- file.path("data", "data_hw")
  dataRawPath    <<- file.path("data", "data_raw")
  dataParsedPath <<- file.path("data", "data_parsed")
}


