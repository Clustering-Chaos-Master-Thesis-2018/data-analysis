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
setGeneric(name="calculatePostPresentationReliability", def=function(theObject, ..., roundRange=c(-Inf,Inf)) {standardGeneric("calculatePostPresentationReliability")})
setGeneric(name="calculateStability", def=function(theObject, ..., roundRange=c(-Inf,Inf)) {standardGeneric("calculateStability")})

setGeneric(name="successPerNodeAndId", def=function(theObject) {standardGeneric("successPerNodeAndId")})

setGeneric(name="calculateSpread", def=function(theObject) {standardGeneric("calculateSpread")})
setGeneric(name="totalPowerUsage", def=function(theObject) {standardGeneric("totalPowerUsage")})
setGeneric(name="calculateReliability", def=function(theObject, ..., roundRange=c(-Inf,Inf)) {standardGeneric("calculateReliability")})
setGeneric(name="calculateChaosReliability", def=function(theObject) {standardGeneric("calculateChaosReliability")})
setGeneric(name="calculateWeakReliability", def=function(theObject, ..., roundRange=c(-Inf,Inf)) {standardGeneric("calculateWeakReliability")})
setGeneric(name="getOffSlots", def=function(theObject) {standardGeneric("getOffSlots")})
setGeneric(name="meanOffSlot", def=function(theObject, ..., roundRange=c(-Inf,Inf)) {standardGeneric("meanOffSlot")})
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

getLastRoundsMax <- function(round, clusterHeads, maxData) {
  filteredMaxData <- maxData[maxData$rd < round,]
  
  if(nrow(filteredMaxData) == 0) {
    #No max data for the previous round, the correct max this round is the highest cluster head ID.
    max(clusterHeads$node_id)
  } else {
    filteredMaxData <- filteredMaxData[filteredMaxData$node_id %in% clusterHeads$node_id,]
    lastClusterRoundMax <- do.call("rbind", lapply(clusterHeads$node_id, function(cluster_id) {
      maxDataForClusterHead <- filteredMaxData[filteredMaxData$node_id == cluster_id,]
      maxDataForClusterHead[which.max(maxDataForClusterHead$rd),]
    }))
    
    max(lastClusterRoundMax$max)
  }
}

setMethod(f="successPerNodeAndId", signature = "TestResult", definition = function(theObject) {
  maxData <- theObject@max_data
  all_max_rounds <- unique(maxData$rd)
  #print(theObject@testName)
  #browser()
  successPerNode <- do.call("rbind", lapply(all_max_rounds, function(round) {
    cluster_heads <- maxData[maxData$rd == round & maxData$node_id == maxData$cluster_id,]
    if(nrow(cluster_heads) == 0) {
     
      return(data.frame(node_id = maxData$node_id, status="unknown", rd=round))
      #return(NA)
    }
    if(round %% 2 == 0) { # CH round
      #browser()
      highest_local_max_last_round = getLastRoundsMax(round, cluster_heads, maxData)
      cluster_heads$status <- with(cluster_heads, ifelse(max == highest_local_max_last_round, "success", "fail"))
      return(cluster_heads[c("rd", "node_id", "status")])
      #nrow(cluster_heads[cluster_heads$max == highest_local_max_last_round,]) / nrow(cluster_heads)
    } else { # Cluster round
      #browser()
      clusterRoundSuccessPerNode <- do.call("rbind", lapply(cluster_heads$node_id, function(cluster_id) {
        #browser()
        nodes_done_max <- maxData[maxData$rd == round & maxData$cluster_id == cluster_id,]
        clusterwide_max <- max(nodes_done_max$node_id)
        nodes_done_max$status <- with(nodes_done_max, ifelse(max == clusterwide_max, "success", "fail"))
        nodes_done_max[c("rd", "node_id", "status")]
        #nrow(nodes_done_max[nodes_done_max$max == clusterwide_max,])
      }))
      return(clusterRoundSuccessPerNode)
    }
  }))
  colnames(successPerNode)[[1]] <- "round"
  
  roundData <- theObject@data
  roundDataDoneMax <- roundData[roundData$app == "chaos_max_app",]
  colnames(maxData)[[1]] <- "round"
  
  #bepa <- merge(roundDataDoneMax, maxData, by=intersect(colnames(roundDataDoneMax),colnames(maxData)))
  
  # Select the elements in roundDataDone max for which there is no corresponding row in roundData with the same round an node_id
  max_rounds_without_result <- roundDataDoneMax[! paste(roundDataDoneMax$round, roundDataDoneMax$node_id) %in% 
        c(paste(maxData$round, maxData$node_id),
          paste(maxData$node_id, maxData$round)), ]
  apa <- max_rounds_without_result[c("round", "node_id")]
  apa$status <- "unknown"
  
  successPerNode <- rbind(successPerNode, apa)
  
  
  return(successPerNode)
})


setMethod(f="calculatePostPresentationReliability", signature = "TestResult",
          definition = function(theObject, ..., roundRange) {
  maxData <- theObject@max_data
  maxData <- maxData[roundRange[[1]] <= maxData$rd & maxData$rd <= roundRange[[2]],]
  all_max_rounds <- unique(maxData$rd)
  #print(theObject@testName)
  #browser()
  round_result <- sapply(all_max_rounds, function(round) {
    cluster_heads <- maxData[maxData$rd == round & maxData$node_id == maxData$cluster_id,]
    if(nrow(cluster_heads) == 0) {
      return(NA)
    }
    if(round %% 2 == 0) {
      highest_local_max_last_round = getLastRoundsMax(round, cluster_heads, maxData)
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


setMethod(f="calculateReliability", signature = "TestResult", definition = function(theObject, ..., roundRange) {
  networkwide_max <- max(theObject@location_data$node_id)
  all_rounds <- unique(theObject@max_data$rd)
  all_rounds[roundRange[[1]] <= all_rounds & all_rounds <= roundRange[[2]]]

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
      a <- all(sapply(cluster_heads$node_id, function(cluster_head) {
        nodes_in_cluster <- roundData[roundData$round == round & roundData$cluster_id == cluster_head,]
        clusterwide_max <- max(nodes_in_cluster$node_id)
        #nodes_in_cluster$max ==
        nodes_done_max <- maxData[maxData$rd == round & maxData$cluster_id == cluster_head,]
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
reliability <- memoise(calculateReliability, cache=db) # deprecated
calculateReliabilityCached <- memoise(calculateReliability, cache=db)


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
chaos_reliability <- memoise(calculateChaosReliability, cache=db) # deprecated
calculateChaosReliabilityCached <- memoise(calculateChaosReliability, cache=db)

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


setMethod(f="calculateStability", signature = "TestResult", definition = function(theObject, ..., roundRange) {
  maxData <- theObject@max_data
  maxData <- maxData[roundRange[[1]] <= maxData$rd & maxData$rd <= roundRange[[2]],]
  nodeCount <- length(unique(theObject@data$node_id))
  maxRounds <- c(36:199, 236:399, 436:600)
  maxRounds <- maxRounds[roundRange[[1]] <= maxRounds & maxRounds <= roundRange[[2]]]

  round_result <- sapply(maxRounds, function(round) {
    nrow(maxData[maxData$rd == round,]) / nodeCount
  })
  
  return(mean(round_result))
})
calculateStabilityCached <- memoise(calculateStability, cache=db)


setMethod(f="calculateWeakReliability", signature = "TestResult", definition = function(theObject, ..., roundRange) {
  # counts cluster failures as partial failures for a round. e.g. 0.333 for a round where 1 cluster succeeds and 2 fails.
  
  all_rounds <- unique(theObject@max_data$rd)
  all_rounds[roundRange[[1]] <= all_rounds & all_rounds <= roundRange[[2]]]
  
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
weakReliability <- memoise(calculateWeakReliability, cache=db) #deprecated
calculateWeakReliabilityCached <- memoise(calculateWeakReliability, cache=db)


setMethod(f="meanOffSlot", signature = "TestResult", definition = function(theObject, ..., roundRange) {
  maxData <- theObject@max_data
  maxData <- maxData[roundRange[[1]] <= maxData$rd & maxData$rd <= roundRange[[2]],]
  return(mean(maxData$off_slot))
})


setMethod(f="sdOffSlot", signature = "TestResult", definition = function(theObject) {
  return(sd(theObject@max_data$off_slot))
})


max_count <- function(testResult) {
  testResult@
  return(5)
}
