library(memoise)

db <- cache_filesystem("~/.rcache")

TestResult <- setClass(
  "TestResult",
  
  slots = c(
    testName = "character",
    simulationFile = "character",
    testDirectory = "character",
    reliability = "numeric",
    data = "data.frame",
    max_data = "data.frame",
    energy_data = "data.frame",
    location_data = "data.frame"
  )
)

setGeneric(name="calculatePostPresentationChaosReliability", def=function(theObject) {standardGeneric("calculatePostPresentationChaosReliability")})
setGeneric(name="calculatePostPresentationReliability", def=function(theObject) {standardGeneric("calculatePostPresentationReliability")})
setGeneric(name="calculateStability", def=function(theObject) {standardGeneric("calculateStability")})

setGeneric(name="calculateSpread", def=function(theObject) {standardGeneric("calculateSpread")})
setGeneric(name="totalPowerUsage", def=function(theObject) {standardGeneric("totalPowerUsage")})
setGeneric(name="calculateReliability", def=function(theObject) {standardGeneric("calculateReliability")})
setGeneric(name="calculateChaosReliability", def=function(theObject) {standardGeneric("calculateChaosReliability")})
setGeneric(name="calculateWeakReliability", def=function(theObject) {standardGeneric("calculateWeakReliability")})
setGeneric(name="getOffSlots", def=function(theObject) {standardGeneric("getOffSlots")})
setGeneric(name="meanOffSlot", def=function(theObject) {standardGeneric("meanOffSlot")})
setGeneric(name="sdOffSlot", def=function(theObject) {standardGeneric("sdOffSlot")})

setMethod(f="getOffSlots", signature = "TestResult", definition = function(theObject) {
  return(theObject@max_data$off_slot)
})

setMethod(f="calculateSpread", signature = "TestResult", definition = function(theObject) {
  loc <- theObject@location_data
  dist(cbind(loc$x,loc$y))
  coord <- cbind(loc$x, loc$y)
  return(max(as.matrix(dist(coord))))
})

setMethod(f="totalPowerUsage", signature = "TestResult", definition = function(theObject) {
  energyData <- theObject@energy_data
  
  energyUsed <- sum(energyData$cpu) +
                sum(energyData$lpm) +
                sum(energyData$transmit) +
                sum(energyData$listen) +
                sum(energyData$idle_transmit) +
                sum(energyData$idle_listen)
  
    return (energyUsed / max(theObject@energy_data$clock_time) / length(unique(theObject@data$node_id)))
})

getLastRoundsMax <- function(round, maxData) {
  lastRoundMaxData <- maxData[maxData$rd == (round - 1) & maxData$node_id == maxData$cluster_id,]$max
  if(length(lastRoundMaxData) == 0) {
    #No max data for the previous round, the correct max this round is the highest cluster head ID.
    max(maxData[maxData$rd == round & maxData$node_id == maxData$cluster_id,]$node_id)
  } else {
    max(lastRoundMaxData) 
  }
}

setMethod(f="calculatePostPresentationReliability", signature = "TestResult", definition = function(theObject) {
  maxData <- theObject@max_data
  all_max_rounds <- unique(maxData$rd)
  #print(theObject@testName)
  #browser()
  round_result <- sapply(all_max_rounds, function(round) {
    cluster_heads <- maxData[maxData$rd == round & maxData$node_id == maxData$cluster_id,]
    if(nrow(cluster_heads) == 0) {
      return(NA)
    }
    if(round %% 2 == 0) {
      highest_local_max_last_round = getLastRoundsMax(round, maxData)
      nrow(cluster_heads[cluster_heads$max == highest_local_max_last_round,]) / nrow(cluster_heads)
    } else {
      nodes_succeded_with_max <- sum(sapply(cluster_heads$node_id, function(cluster_id) {
        nodes_done_max <- maxData[maxData$rd == round & maxData$cluster_id == cluster_id,]
        clusterwide_max <- max(nodes_done_max$node_id)
        nrow(nodes_done_max[nodes_done_max$max == clusterwide_max,])
      }))
      nodes_succeded_with_max / nrow(maxData[maxData$rd == round,])
    }
  })
  mean(round_result, na.rm = T)
})


calculatePostPresentationReliabilityCached <- memoise(calculatePostPresentationReliability, cache=db)

setMethod(f="calculateReliability", signature = "TestResult", definition = function(theObject) {
  networkwide_max <- max(theObject@location_data$node_id)
  all_rounds <- unique(theObject@max_data$rd)

  roundData <- theObject@data
  maxData <- theObject@max_data

  round_result <- sapply(all_rounds, function(round) {
    cluster_heads <- roundData[roundData$round == round & roundData$node_id == roundData$cluster_id,]
    if(round %% 2 == 0) {
      cluster_head_done_max <- maxData[maxData$rd == round & maxData$node_id %in% cluster_heads$node_id,]
      return(all(cluster_head_done_max$max == networkwide_max, nrow(cluster_head_done_max) == nrow(cluster_heads)))
    } else {
      if(nrow(cluster_heads) == 0){
        return(FALSE)
      }
      a <- all(apply(cluster_heads, 1, function(cluster_head) {
        nodes_in_cluster <- roundData[roundData$round == round & roundData$cluster_id == cluster_head[["cluster_id"]],]
        clusterwide_max <- max(nodes_in_cluster$node_id)
        #nodes_in_cluster$max ==
        nodes_done_max <- maxData[maxData$rd == round & maxData$cluster_id == cluster_head[["cluster_id"]],]
        all(nodes_done_max$max == clusterwide_max, nrow(nodes_done_max) == nrow(nodes_in_cluster))
      }))
      return(a)
    }

  })

  expectedMaxRounds <- 599-105
  if(expectedMaxRounds - length(round_result) > 0) {
    round_result <- c(round_result,  rep(F, expectedMaxRounds - length(round_result)))
  }

  return(mean(round_result))
})

setMethod(f="calculateChaosReliability", signature = "TestResult", definition = function(theObject) {
  networkwide_max <- max(theObject@location_data$node_id)
  all_rounds <- unique(theObject@max_data$rd)
  
  roundData <- theObject@data
  maxData <- theObject@max_data
  node_count <- length(unique(roundData$node_id))
  
  round_result <- sapply(all_rounds, function(round) {
    nodes_done_max <- maxData[maxData$rd == round,]
    all(nodes_done_max$max == networkwide_max, nrow(nodes_done_max) == nrow(node_count))
  })
  
  expectedMaxRounds <- round(599-node_count / 10)
  if(expectedMaxRounds - length(round_result) > 0) {
    round_result <- c(round_result,  rep(F, expectedMaxRounds - length(round_result)))
  }
  
  return(mean(round_result))
})

setMethod(f="calculatePostPresentationChaosReliability", signature = "TestResult", definition = function(theObject) {
  maxData <- theObject@max_data
  maxRounds <- unique(maxData$rd)
  
  round_result <- sapply(maxRounds, function(round) {
    nodesRunningMax <- maxData[maxData$rd == round,]
    maxValue = max(nodesRunningMax$max)
    nrow(nodesRunningMax[nodesRunningMax$max == maxValue,]) / nrow(nodesRunningMax)
  })
  
  return(mean(round_result))
})

calculatePostPresentationChaosReliabilityCached <- memoise(calculatePostPresentationChaosReliability, cache=db)
chaos_reliability <- memoise(calculateChaosReliability, cache=db)
reliability <- memoise(calculateReliability, cache=db)

setMethod(f="calculateWeakReliability", signature = "TestResult", definition = function(theObject) {
  # counts cluster failures as partial failures for a round. e.g. 0.333 for a round where 1 cluster succeeds and 2 fails.
  
  all_rounds <- unique(theObject@max_data$rd)
  if (is.null(all_rounds) ) {
    warning(paste("All rounds is null. There is no max data for ", theObject@testDirectory))
    return(0)
  }
  
  all_cluster_ids <- unique(theObject@max_data$cluster_id)
  g <- expand.grid(t(all_rounds), t(all_cluster_ids))
  colnames(g) <- c("round","cluster")
  
  #calculate completion rate for each row
  g <- mdply(g, Curry(completion_rate, theObject@max_data, max(theObject@location_data$node_id)))
  colnames(g)[3] <- "completion_rate"
  g <- g[complete.cases(g),] # remove NA values
  g <- arrange(g,round)
  
  reliability <- mean(t(g["completion_rate"]))
  
  return(reliability)
})

weakReliability <- memoise(calculateWeakReliability, cache=db)

setMethod(f="meanOffSlot", signature = "TestResult", definition = function(theObject) {
  return(mean(theObject@max_data$off_slot))
})

setMethod(f="sdOffSlot", signature = "TestResult", definition = function(theObject) {
  return(sd(theObject@max_data$off_slot))
})

max_count <- function(testResult) {
  testResult@
  return(5)
}
