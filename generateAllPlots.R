

asd <- c(
    "PlotChaosComparisons.R",
    "PlotFlocklabComparison.R",
    "PlotResyncTresholdTests.R",
    "PlotCompetitionRadiusTests.R",
    "PlotMinNodeCountTests.R",
    "PlotMaxNodeCountTests.R"
)


foreach(result = asd) %do% {
  source(result)
}

source("PlotResyncTresholdTests.R")
source("PlotChaosComparisons.R")
source("PlotCompetitionRadiusTests.R")
source("PlotMinNodeCountTests.R")
source("PlotMaxNodeCountTests.R")
source("PlotFlocklabComparison.R")
