source("monitor.r")

GetScale <- function(sysDataInfo, firstID, firstRowsIDs, secondID) {
  # Calculates number of equal configurations, measured on two machines
  # 
  # Args:
  #   sysDataInfo: data info of the configurable system
  #   firstID: first machine ID
  #   secondID: second machine ID
  Monitor()

  # Prepare data frames for comparison
  a <- tbl_df(sysDataInfo$machinesData[[firstID]][firstRowsIDs, ])
  b <- tbl_df(sysDataInfo$machinesData[[secondID]])
  # cat("\nData frames for comparison\n")
  Monitor(a)
  Monitor(b)

  # Find same configurations
  features <- sysDataInfo$features
  af <- a[, features]
  b <- left_join(af, b, by = features)
  # cat("\nData frames with the same configurations\n")
  Monitor(af)
  Monitor(b)

  # Summarise performance
  aSum <- summarise(a, s = sum(PERF))
  bSum <- summarise(b, s = sum(PERF))

  # Calculate scale
  scale <- bSum / aSum
  scale <- scale[1,1]
  # cat("\nCalculating scale:\n")
  Monitor(aSum)
  Monitor(bSum)
  Monitor(scale)

  return(scale)
}