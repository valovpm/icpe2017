source("init.r")
source("monitor.r")
source("data.r")
source("scaling.r")

test.CountSameRows <- function() {
  c1 = 1:10
  c2 = c(0,1,0,1,0,1,0,1,0,1)
  c2 <- c1 + c2

  a = data.frame(c1, c1, c1)
  b = data.frame(c1, c1, c2)

  print("Data frame #1:")
  print(a)

  print("Data frame #2:")
  print(b)

  print("Comparison")
  count <- GetSameRows(a,b)
  print(count)
  # combn((1:count[[1]]), 4)
}

test.GetScale <- function() {
  print("Testing GetScale():")

  sysDataInfo <- GetSysDataInfo(2)
  firstID <- 1
  firstRowsIDs <- c(1:10)
  secondID <- 2

  print("Variables are prepared")

  scale <- GetScale(sysDataInfo,
                    firstID,
                    firstRowsIDs,
                    secondID)
}

Init()
test.GetScale()