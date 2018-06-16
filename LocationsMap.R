library(xml2)
library(RColorBrewer) # For unique colors
library(plyr)


prepareAndPlotNodeLocations <- function(testResult) {
  print(testResult@testName)

  #roundData <- load_all_nodes_round_data(row["testDirectory"])
  roundData <- testResult@data
  max_round <- max(roundData$round, na.rm = TRUE)
  pdf(file = file.path(testResult@testDirectory, "locations.pdf"))
  for (round in 50:70 ){
    filteredRoundData <- roundData[roundData$round == round,]
    clusters <- clusterHeadIds(filteredRoundData)

    # Create node to cluster map
    a <- !duplicated(filteredRoundData[c("node_id")])
    roundDataSub <- subset(filteredRoundData, a)
    
    node_id <- 1:max(roundDataSub$node_id)
    cluster_id <- 0
    node_cluster_map <- data.frame(node_id, cluster_id)
    
    if("cluster_id" %in% colnames(roundDataSub)) {
      node_cluster_map <- roundDataSub[c("node_id","cluster_id")]
    }
  
    plotNodeLocations(testResult, clusters, node_cluster_map, round)

  }
  dev.off()
}

lastAssociatingNodes <<- data.frame()
lastClusterHeads <<- c()
lastNodes <<- data.frame()

#' Plots a tests mote locations and marks the cluster heads
#'
#' @param testPath Absolute path to a single test
#' @param clusterHeads A vector of the cluster heads
plotNodeLocations <- function(testResult, clusterHeads=c(), node_cluster_map, round_id) {
  root <- read_xml(testResult@simulationFile)
  # Fetch data from xml file
  node_id <- as.numeric(xml_text(xml_find_all(root, ".//id")))
  x  <- as.double(xml_text(xml_find_all(root, ".//x")))
  y  <- as.double(xml_text(xml_find_all(root, ".//y")))
  nodes <- data.frame(node_id,x,y)
  
  # Merge data from log files with data from xml file
  nodes <- merge(node_cluster_map, nodes, by = "node_id", all = TRUE)

  
  # Nodes which hasn't logged anything yet have no clusters. i.e. cluster 0.
  nodes[is.na(nodes)] <- 0
  
  # Remove nodes with no location.
  nodes <- nodes[complete.cases(nodes),]
  
  # Mapping from cluster_id to color index
  clusters <- unique(nodes[["cluster_id"]])
  minColors <- order(clusters)

  roundData <- testResult@data
  associatingNodes <- roundData[roundData$round == round_id & roundData$app == "association",]

  if(nrow(associatingNodes) == nrow(lastAssociatingNodes) && (nrow(associatingNodes) == 0 || all(associatingNodes == lastAssociatingNodes)) &&
     nrow(lastNodes) == nrow(nodes) && (nrow(lastNodes) == 0 || all(lastNodes == nodes)) && 
     all(lastClusterHeads == clusterHeads)) {
    return()
  }
  
  #lastAssociatingNodes <<- associatingNodes
  #lastNodes <<- nodes
  #lastClusterHeads <<- clusterHeads
  #browser()
  #filterClusterHeads <- roundData[roundData$round > 0,]
  #filterClusterHeads <- filterClusterHeads[filterClusterHeads$round < 200,]
  #allClusterHeads <- unique(filterClusterHeads$cluster_id)
  #browser()
  allClusterHeads <- c(21, 35, 46)
  #allClusterHeads <- c(1, 22, 30)
  minColors <- order(allClusterHeads)
  # Add color column
  # Get distinguishable colors
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors[c(2, 3, 7)], c("Dark2", "Paired", "Set2")))

  nodes$node_color_indexes <- mapvalues(nodes[["cluster_id"]], from=allClusterHeads, to=minColors)
  nodes$color <- with(nodes, col_vector[node_color_indexes])

  # Add size column
  nodes$node_sizes <- rep(2,length(nodes[["node_id"]]))
  
  if(!is.null(clusterHeads) && length(clusterHeads) != 0) {
    # Mark cluster heads as larger than normal nodes.
    nodes[nodes$node_id %in% clusterHeads,]$node_sizes <- 5
  }
  
  if(nrow(associatingNodes) > 0) {
    assoc <- subset(nodes, node_id %in% associatingNodes$node_id)
    if(nrow(assoc) > 0) {
      plot(assoc$y~assoc$x, xlim = range(nodes$x), ylim = rev(range(nodes$y)), asp=1, col="orange", pch=16, cex=assoc$node_sizes + 2, xlab="x", ylab="y", main = paste0("Round Number: ",round_id))
      points(nodes$y~nodes$x, ylim = rev(range(nodes$y)), asp=1, col=nodes$color, pch=16, cex=nodes$node_sizes, xlab="x", ylab="y")
    } else {
      plot(nodes$y~nodes$x, ylim = rev(range(nodes$y)), asp=1, col=nodes$color, pch=16, cex=nodes$node_sizes, xlab="x", ylab="y", main = paste0("Round Number: ",round_id))
    }
  } else {
    plot(nodes$y~nodes$x, ylim = rev(range(nodes$y)), asp=1, col=nodes$color, pch=16, cex=nodes$node_sizes, xlab="x", ylab="y", main = paste0("Round Number: ",round_id))
  }

  clusterHeads <- append(clusterHeads, c(40, 23))
  #text(nodes$y~nodes$x, labels=nodes$node_id, col="black") # Write node_id on top of the nodes
  if(nrow(nodes[nodes$node_id %in% clusterHeads,]) > 0) {
    text(nodes[nodes$node_id %in% clusterHeads,]$y~nodes[nodes$node_id %in% clusterHeads,]$x, labels=nodes[nodes$node_id %in% clusterHeads,]$node_id, col="black") # Write node_id on top of the nodes 
  }
}
