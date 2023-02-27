source(file = "accuracy.r")
source(file = "accuracy_plotting.r")
source(file = "dist_features.r")
source(file = "dist_features_plot.r")
source(file = "dist_perf.r")
source(file = "dist_transfer.r")
source(file = "init.r")

Init()

# Exploratory analysis:
#   1. Performance distributions of configurable software systems
#      in different hardware environments
#   2. Feature distributions of regression trees
#      trained for different software systems
#   3. Plot feature distributions of regression trees
#   4. Transferring distributions between different hardware
#      environments
DistPerf()
DistFeatures()
DistFeaturesPlot()
DistTransfer()

# Transferring accuracy analysis:
#   1. Perform transferring accuracy analysis
#   2. Plot transferring results
Accuracy()
AccuracyPlotting()


