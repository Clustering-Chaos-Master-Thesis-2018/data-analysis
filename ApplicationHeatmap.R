
library(ggplot2)
library(reshape2)

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
  xSteps <- floor(c(1, seq(step, max(roundData$round)-step, step), max(roundData$round)))
  # browser()
  
  emptyAssociateRow <- data.frame(app = "association", round = NA, node_id = NA, color = NA)
  emptyMaxAppRow <- data.frame(app = "chaos_max_app", round = NA, node_id = NA, color = NA)
  emptyClusterRow <- data.frame(app = "cluster", round = NA, node_id = NA, color = NA)
  emptyDemoteRow <- data.frame(app = "demote", round = NA, node_id = NA, color = NA)
  emptyJoinRow <- data.frame(app = "join", round = NA, node_id = NA, color = NA)
  
  roundDataWithAllApplications <- rbind(emptyAssociateRow, emptyClusterRow, emptyJoinRow, emptyDemoteRow, emptyMaxAppRow, roundData)
  #browser()
  p <- ggplot(roundDataWithAllApplications, aes(round, node_id, fill=app)) +
    geom_raster() +
    geom_tile(colour="white",size=0.1) +
    scale_y_discrete(expand=c(0,0), limits=ySteps, labels=ySteps, breaks=ySteps) +
    scale_x_discrete(expand=c(0,0), limits=xSteps, labels=xSteps ,breaks=xSteps) +
    #coord_fixed(ratio = 0.5) +
    coord_fixed() +
    labs(x="Round",y="Node ID", fill="Application") +
    scale_fill_manual(labels = c("Association", "Cluster Service", "Join Service", "Demote Service", "Max"), values = cbPalette)
  ggsave(filename = "/tmp/apa.pdf",width = 13, height=1.8)
  return(p)
}