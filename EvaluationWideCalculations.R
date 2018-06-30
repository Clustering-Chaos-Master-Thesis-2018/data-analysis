library(memoise)
library(data.table)

db <- cache_filesystem("~/.rcache")

fiftynodes <- c(
  file.path(evaluation_directory, "competition-radius"),
  file.path(evaluation_directory, "max-node-count"),
  file.path(evaluation_directory, "min-node-count")
  #file.path(evaluation_directory, "resync-treshold")
)

findAllTestsFromPath <- function(path) {
  allTests <- list.files(path, recursive = T, full.names = T, pattern = "cooja.log")
  
  allTests <- unlist(lapply(allTests, dirname))
  allTests <- unlist(allTests, use.names=FALSE)
  unique(unlist(lapply(allTests, dirname)))
}


loadAllTests <- function(testPaths) {
  testSuites <- unlist(lapply(testPaths, function(path) {findAllTestsFromPath(path)}))
  unlist(mclapply(testSuites, loadResultsFromTestSuitePath, mc.cores = 8))
  
  #loadedtestResults <- testResults[!is.na(testResults)]
}

loadAllTests <- memoise(loadAllTests, cache = db)

calculateWrongClusterHeadRoundState <- function(testResult) {
  maxRounds <- c(36:199, 236:399, 436:600)
  rounds <- testResult@data
  output <- rounds[c("round", "node_id")]
  output$status = "Correct Cluster Head Round"
  rounds <- unique(rounds[rounds$round %in% maxRounds,])
  rounds <- rounds[rounds$round %% 2 == 0,]
  
  roundsWithWrongClusterHeadRoundState <- rounds[rounds$is_cluster_head_round == 0 & rounds$app == "chaos_max_app",][c("round", "node_id")]

  output <- setDT(output)
  roundsWithWrongClusterHeadRoundState <- setDT(roundsWithWrongClusterHeadRoundState)
  
  output[roundsWithWrongClusterHeadRoundState, on = c("round", "node_id"), status := "WRONG"]
  
  return(output)
}


calculateWrongClusterHeadRoundState <- memoise(calculateWrongClusterHeadRoundState, cache = db)

#asd <- lapply(loadAllTests(fiftynodes), calculateWrongClusterHeadRoundState)
#asd <- mclapply(loadAllTests(fiftynodes), calculateWrongClusterHeadRoundState, mc.cores = 8)
