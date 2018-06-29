source('Main.R')
source('AppLib.R')

library(memoise)

db <- cache_filesystem("~/.rcache")

LoadCHStatisticsData <- function(fullNames, partialNames) {
  testSuites <- mclapply(fullNames, loadResultsFromTestSuitePath, mc.cores = 8)
  stats <- mapply(function(testSuite, partialName) {
    asd <- lapply(testSuite, function(result) {
      if(is.na(result)) {
        return(NA)
      }
      # data.frame(name=result@testName, meanOffSlot=meanOffSlot(result), sdOffSlot=sdOffSlot(result), test_suite=partialName, spread=calculateSpread(result))
      testName <- sub(".+?-motes-(.+?x.+?)-(random|spread)", "\\1", result@testName)
      frame <-calculateClusterHeadStatistics(result)
      frame$test_suite = partialName
      frame$name = testName
      return(frame)
    })
    do.call("rbind", asd)
  }, testSuites, partialNames, SIMPLIFY = F)
  # browser() 
  stats <- do.call("rbind", stats)
  stats$name <- factor(stats$name, levels = unique(stats$name[order(stats$spread)]))
  
  return(stats)
}


loadNormal <- Curry(LoadCHStatisticsData, mapplyFunc = mapply, lapplyFunc = lapply)
loadParallel <- Curry(LoadCHStatisticsData, mapplyFunc = Curry(mcmapply, mc.cores = 4), lapplyFun = Curry(mclapply, mc.cores = 4))

#LoadCHStatisticsData <- loadNormal
#LoadCHStatisticsData <- loadParallel
#LoadCHStatisticsData <- memoise(loadParallel, cache=db)
#LoadCHStatisticsData <- memoise(loadNormal, cache=db)

LoadCHStatisticsData <- memoise(LoadCHStatisticsData, cache=db)

createPlot <- function(agg, point, xLab = "Test Name", yLab = "Y Axis") {
  return(
    ggplot(agg) +
      point +
      theme(
        axis.text.x=element_text(angle=45, hjust=1),
        plot.margin=unit(c(1,1,1,2),"cm"),
        text = element_text(size=20)
      ) +
      xlab(xLab) +
      ylab(yLab) +
      scale_y_continuous(breaks = seq(0, 100, 2))
  )
}

AverageCHCountPlot <- function(input) {
  renderPlot({
    partialNames <- input$checkedTests
    fullNames <- lookupFullNames(partialNames)
    if (length(fullNames) == 0) {
      return(plot(1,1))
    }
    
    print(input$checkedTests)
    
    
    stats <- LoadCHStatisticsData(fullNames, partialNames)
    
    agg <- aggregate(promotedCHCount~name+test_suite+spread, stats, function(a) c(mean=mean(a)))
    agg <- do.call(data.frame, agg)
    
    return(createPlot(agg, geom_point(aes(name, promotedCHCount, color=test_suite), size=6, position=position_dodge(width=0.3)), yLab = "Average CH Count Before Demote"))
  })
}

CHCountAfterDemotionPlot <- function(input) {
  renderPlot({
    partialNames <- input$checkedTests
    fullNames <- lookupFullNames(partialNames)
    if (length(fullNames) == 0) {
      return(plot(1,1))
    }
    
    print(input$checkedTests)
    
    stats <- LoadCHStatisticsData(fullNames, partialNames)
    
    agg <- aggregate(CHCountAfterDemotion~name+test_suite+spread, stats, function(a) c(mean=mean(a)))
    agg <- do.call(data.frame, agg)
    
    return(createPlot(agg, geom_point(aes(name, CHCountAfterDemotion, color=test_suite), size=6, position=position_dodge(width=0.3)), yLab = "Average CH Count After Demotion"))
  })
}



DemotedCHPlot <- function(input) {
  renderPlot({
    partialNames <- input$checkedTests
    fullNames <- lookupFullNames(partialNames)
    if (length(fullNames) == 0) {
      return(plot(1,1))
    }
    
    print(input$checkedTests)
    
    
    stats <- LoadCHStatisticsData(fullNames, partialNames)
    
    agg <- aggregate(demotedCHCount~name+test_suite+spread, stats, function(a) c(sum=sum(a)))
    agg <- do.call(data.frame, agg)
    
    return(createPlot(agg, geom_point(aes(name, demotedCHCount, color=test_suite), size=6, position=position_dodge(width=0.3)), yLab = "Demoted CH Count"))
  })
}

TotalCHPlot <- function(input) {
  renderPlot({
    partialNames <- input$checkedTests
    fullNames <- lookupFullNames(partialNames)
    if (length(fullNames) == 0) {
      return(plot(1,1))
    }
    
    print(input$checkedTests)
    
    stats <- LoadCHStatisticsData(fullNames, partialNames)
    agg <- aggregate(promotedCHCount~name+test_suite+spread, stats, function(a) c(sum=sum(a)))
    agg <- do.call(data.frame, agg)
    
    return(createPlot(agg, geom_point(aes(name, promotedCHCount, color=test_suite), size=6, position=position_dodge(width=0.3)), yLab = "Total CH Count"))
  })
}

AverageAssociatingNodes <- function(input) {
  renderPlot({
    partialNames <- input$checkedTests
    fullNames <- lookupFullNames(partialNames)
    if (length(fullNames) == 0) {
      return(plot(1,1))
    }
    
    print(input$checkedTests)
    
    stats <- LoadCHStatisticsData(fullNames, partialNames)
    agg <- aggregate(associatingCHCount~name+test_suite+spread, stats, function(a) c(mean=mean(a)))
    agg <- do.call(data.frame, agg)
    
    return(createPlot(agg, geom_point(aes(name, associatingCHCount, color=test_suite), size=6, position=position_dodge(width=0.3)), yLab = "Average Associating CH Count"))
  })
}

