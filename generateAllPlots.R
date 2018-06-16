

asd <- c("PlotResyncTresholdTests.R",
"PlotChaosComparisons.R",
"PlotCompetitionRadiusTests.R",
"PlotMinNodeCountTests.R",
"PlotMaxNodeCountTests.R",
"PlotFlocklabComparison.R")


foreach(result = asd) %dopar% {
  source(result)
}