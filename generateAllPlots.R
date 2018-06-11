

asd <- c("PlotResyncTresholdTests.R",
"PlotChaosComparisons.R",
"PlotCompetitionRadiusTests.R",
"PlotMinNodeCountTests.R",
"PlotMaxNodeCountTests.R")


foreach(result = asd) %dopar% {
  source(result)
}