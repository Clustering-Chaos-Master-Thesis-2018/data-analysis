source("EvaluationWideCalculations.R")

library(ggplot2)
library(reshape2)

plotReliabilityHeatmap <- function(testResult, round_interval=range(testResult@data$round)) {
  roundData <- testResult@data
  successDF <- successPerNodeAndId(testResult)  
  
  successDF <- successDF[successDF$round %in% seq(round_interval[[1]],round_interval[[2]]),]
  
  emptySuccessRow <- data.frame(status = "success", round = NA, node_id = NA)
  emptyFailRow <- data.frame(status = "fail", round = NA, node_id = NA)
  emptyUnknownRow <- data.frame(status = "unknown", round = NA, node_id = NA)
  spanToRoundOneRow <- data.frame(status = "unknown", round = round_interval[[1]]:min(round_interval[[2]], max(roundData$round)), node_id = 0)
  
  successDF <- rbind(emptySuccessRow, emptyFailRow, emptyUnknownRow, spanToRoundOneRow, successDF)
  
  ySteps <- c(1,seq(5,max(roundData$node_id),5), max(roundData$node_id))
  xSteps <- floor(seq(round_interval[1], round_interval[2], (round_interval[2] - round_interval[1]) / 9))
  
  cbPalette <- c("#009E73", "#D55E00", "#CC79A7", "#E69F00", "#56B4E9")
  #browser()
  p <- ggplot(successDF, aes(round, node_id, fill=status)) +
    geom_raster() +
    geom_tile(colour="white",size=0.1) +
    scale_y_discrete(expand=c(0,0), limits=ySteps, labels=ySteps, breaks=ySteps) +
    scale_x_discrete(expand=c(0,0), limits=xSteps, labels=xSteps ,breaks=xSteps) +
    #coord_fixed(ratio = 1) +
    # coord_fixed() +
    labs(x="Round",y="Node ID", fill="Application Names") +
    scale_fill_manual(values = cbPalette)
  return(p)
}

plotWrongCHStateHeatmap <- function(testResult, round_interval=range(testResult@data$round)) {
  roundData <- testResult@data
  #browser()
  chRoundStatus <- calculateWrongClusterHeadRoundState(testResult)  
  #browser()
  chRoundStatus <- chRoundStatus[chRoundStatus$round %in% seq(round_interval[[1]],round_interval[[2]]),]
  
 # emptySuccessRow <- data.frame(success = T, rd = NA, node_id = NA)
#  emptyFailRow <- data.frame(success = F, rd = NA, node_id = NA)
#  spanToRoundOneRow <- data.frame(success = F, rd = 1, node_id = 0)
  
#  chRoundStatus <- rbind(emptySuccessRow, emptyFailRow, spanToRoundOneRow, chRoundStatus)
  
  ySteps <- c(1,seq(5,max(roundData$node_id),5), max(roundData$node_id))
  xSteps <- floor(seq(round_interval[1], round_interval[2], (round_interval[2] - round_interval[1]) / 9))
  
  cbPalette <- c("#009E73", "#D55E00", "#E69F00")
  
  p <- ggplot(chRoundStatus, aes(round, node_id, fill=status)) +
    geom_raster() +
    geom_tile(colour="white",size=0.1) +
    scale_y_discrete(expand=c(0,0), limits=ySteps, labels=ySteps, breaks=ySteps) +
    scale_x_discrete(expand=c(0,0), limits=xSteps, labels=xSteps ,breaks=xSteps) +
    #coord_fixed(ratio = 1) +
    # coord_fixed() +
    labs(x="Round",y="Node ID", fill="Application Names") +
    scale_fill_manual(labels = c("Success", "Failure"), values = cbPalette)
  return(p)
}


plotHeatmap <- function(testResult, round_interval=range(testResult@data$round)) {
  
  print(testResult@testName)
  app <- c("association", "sleeping", "cluster", "join", "demote", "chaos_max_app")
  color <- 1:length(app)

  appToColorTable <- data.frame(app, color, stringsAsFactors = F)
  
  node_ids <- sort(unique(testResult@data$node_id))

  roundData <- testResult@data
  roundData <- roundData[roundData$round %in% seq(round_interval[[1]],round_interval[[2]]),]
  roundData <- roundData[c("round", "node_id", "app")]
  roundData <- merge(roundData, appToColorTable, by="app")
  #rowser()
  points <-  9
  step <-  max(roundData$round)/points
  
  cbPalette <- c("#D55E00", "#E69F00", "#009E73", "#CC79A7", "#56B4E9")
  
  ySteps <- c(1,seq(5,max(roundData$node_id),5), max(roundData$node_id))
  xSteps <- floor(seq(round_interval[1], round_interval[2], (round_interval[2] - round_interval[1]) / 9))

  emptyAssociateRow <- data.frame(app = "association", round = NA, node_id = NA, color = NA)
  emptyMaxAppRow <- data.frame(app = "chaos_max_app", round = NA, node_id = NA, color = NA)
  emptyClusterRow <- data.frame(app = "cluster", round = NA, node_id = NA, color = NA)
  emptyDemoteRow <- data.frame(app = "demote", round = NA, node_id = NA, color = NA)
  emptyJoinRow <- data.frame(app = "join", round = NA, node_id = NA, color = NA)
  
  duplicatedRoundPrints <- roundData[duplicated(roundData[c("round","node_id")]),]
  correspondingDuplicates <- roundData[duplicated(roundData[c("round","node_id")], fromLast = T),]
  bothDuplicates <- rbind(duplicatedRoundPrints, correspondingDuplicates)
  duplicatesForRemoval <- bothDuplicates[bothDuplicates$app != "association",]
  roundData <- roundData[setdiff(rownames(roundData), rownames(duplicatesForRemoval)),]

  roundDataWithAllApplications <- rbind(emptyAssociateRow, emptyClusterRow, emptyJoinRow, emptyDemoteRow, emptyMaxAppRow, roundData)
  #browser()
  p <- ggplot(roundDataWithAllApplications, aes(round, node_id, fill=app)) +
    geom_raster() +
    geom_tile(colour="white",size=0.1) +
    scale_y_discrete(expand=c(0,0), limits=ySteps, labels=ySteps, breaks=ySteps) +
    scale_x_discrete(expand=c(0,0), limits=xSteps, labels=xSteps ,breaks=xSteps) +
    #coord_fixed(ratio = 1) +
    #coord_fixed() +
    labs(x="Round",y="Node ID", fill="Application Names") +
    scale_fill_manual(labels = c("Association", "Cluster Service", "Join Service", "Demote Service", "Max"), values = cbPalette)
  ggsave(filename = "/tmp/apa.pdf",width = 13, height=1.8)
  return(p)
}
