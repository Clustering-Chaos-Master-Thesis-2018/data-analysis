library(memoise)
source ("TestResult.R")

db <- cache_filesystem("~/.rcache")

setGeneric(name="calculateClusterHeadStatistics", def=function(theObject) {standardGeneric("calculateClusterHeadStatistics")})

preDemoteRoundList <- c(20, 220, 420)
postDemoteRoundList <- c(31, 231, 431)

getRemovedCHs <- function(roundData) {
  mapply(function(pre, post) {
    sort(setdiff(unique(roundData[roundData$round == pre & roundData$cluster_id == roundData$node_id,]$node_id), unique(roundData[roundData$round == post & roundData$cluster_id == roundData$node_id,]$node_id)))
  }, preDemoteRoundList, postDemoteRoundList, SIMPLIFY = F)
}

getPromotedCHs <- function(roundData) {
  lapply(preDemoteRoundList, function(round) {
    sort(unique(roundData[roundData$round == round & roundData$cluster_id == roundData$node_id,]$node_id))
  })
}

getAssociatingCHs <- function(roundData) {
  mapply(function(pre, post) {
    asd <- lapply(c(pre:post), function(round) {
      roundData[roundData$round == round & roundData$app == "association" & roundData$node_id == roundData$cluster_id, ]$node_id
    })
    unlist(unique(asd))
  }, preDemoteRoundList, postDemoteRoundList)
}

setMethod(f="calculateClusterHeadStatistics", signature = "TestResult", definition = function(theObject) {
  maxData <- theObject@max_data
  roundData <- theObject@data
  all_max_rounds <- unique(maxData$rd)
  
  uniqueCHCount <- length(unique(theObject@data$cluster_id)) - 1 #Remove cluster_id 0, it is not a valid cluster.
  #browser()
  removedCHs <- getRemovedCHs(roundData)
  promotedCHs <- getPromotedCHs(roundData)
  associatingCHs <- getAssociatingCHs(roundData)
#  browser()
  #CHs that are proper demoted, and not removed because thery associated.
  demotedCHs <- (mapply(function(a, b) {setdiff(a, b)}, removedCHs, associatingCHs))
  
  promotedCHCount    <- sapply(promotedCHs, length)
  demotedCHCount     <- sapply(demotedCHs, length)
  removedCHCount     <- sapply(removedCHs, length)
  associatingCHCount <- sapply(associatingCHs, length)
  
  CHStatistics <- data.frame(name = theObject@testName, test_suite = theObject@testDirectory, promotedCHCount = promotedCHCount)
  if(length(demotedCHCount) > 3) {
    browser()
  }
  CHStatistics$demotedCHCount <- demotedCHCount
  CHStatistics$associatingCHCount <- associatingCHCount
  CHStatistics$CHCountAfterDemotion <- promotedCHCount - removedCHCount
 
  CHStatistics$clusterHeads <- promotedCHs  
  CHStatistics$demotedClusterHeads <- demotedCHs
  CHStatistics$associatingCHs <- associatingCHs
  
  CHStatistics$spread <- calculateSpread(theObject)
  CHStatistics
  
})

calculateClusterHeadStatisticsCached <- memoise(calculateClusterHeadStatistics, cache=db)

