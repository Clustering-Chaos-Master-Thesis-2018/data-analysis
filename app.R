library(shiny)

source('Main.R')
source('AppLib.R')
source('CHStatisticsShinyApp.R')

getTestNames <- function(input) {
  abs_test_suite_path <- lookupFullNames(input$test_suite_path)
  testNames(abs_test_suite_path)
}

loadTestForHeatmaps <- function(input, plotName) {
  abs_test_suite_path <- lookupFullNames(input$test_suite_path)
  
  tests <- testNames(abs_test_suite_path)
  if(length(tests) == 0) {
    print(paste("Could not load", plotName, "no tests found."))
    return (NA)
  } else {
    print(paste("Working on ", plotName, ".", sep = ""))
    rows <- lapply(tests, Curry(createTestInfoRow, abs_test_suite_path))
    rows <- rows[!is.na(rows)]
    testResults <- load_data_m(rows)
    index <- which(sapply(testResults, function(result) result@testName == input$num))
    return(testResults[[index]])
  }
}

shinyApp(
  ui = tagList(
    #shinythemes::themeSelector(),
    navbarPage(
      #theme = "cerulean",  # <--- To use a theme, uncomment this
      "Bringing Order to Chaos",
      tabPanel("Found test data",
               mainPanel(
                 h1("Collected test suits"),
                 p(verbatimTextOutput("active_test_suite")),
                 uiOutput("test_suits_radio_buttons"),
                 uiOutput("test_suites_check_boxes")
               )
      ),
      tabPanel("Application Heatmap",
                 verticalLayout(
                   textOutput("application_plot_name"),
                   selectInput("num", label = h3("Select Topology"), choices = list()),
                   actionButton("pdf", "Open Location Plot"),
                   sliderInput("application_plot_range", label = h3("Round Span"), min = 0, 
                               max = 700, value = c(1, 50)),
                   htmlOutput("range_reliability"),
                   plotOutput("application_plot"),#, height  = "1080", width = "3840"),
                   plotOutput("reliability_heatmap"),#, height  = "1080", width = "3840"),
                   plotOutput("wrong_chstate_heatmap")#, height = "1080", width = "3840")
               )
      ),
      tabPanel("Reliability", 
               verticalLayout(
                 plotOutput("reliability_plot"),
                 plotOutput("new_reliability_plot"),
                 plotOutput("stability_plot")
               )
      ),
      tabPanel("Latency", 
               verticalLayout(
                 plotOutput("latency_plot")
               )
      ),
      tabPanel("Energy", 
               verticalLayout(
                 plotOutput("energy_plot")
               )
      ),
      tabPanel("CH Information",
              verticalLayout(
                plotOutput("average_ch_count_plot"),
                plotOutput("ch_count_after_demotion"),
                plotOutput("demoted_ch_plot"),
                plotOutput("average_associating_ch_count_plot"),
                plotOutput("total_ch_plot")
              ) 
      )
    )
  ),
  server = function(input, output, session) {
    output$test_suits_radio_buttons <- renderUI({
      dirs <- partial()
      
      
      test_suites <- dirs[dirs != "Simulations"]
      file_infos <- file.info(test_suites)
      file_infos <- file_infos[order(file_infos$ctime, decreasing = T),]
      
      buttons <- radioButtons("test_suite_path", label = NULL, rownames(file_infos), selected = NULL)
      
      return(buttons)
    })
    
    output$test_suites_check_boxes <- renderUI({
      dirs <- partial()
      
      test_suites <- dirs[dirs != "Simulations"]
      file_infos <- file.info(test_suites)
      file_infos <- file_infos[order(file_infos$ctime, decreasing = T),]
      updateSelectInput(session, "num", label = "Select Topology", choices = getTestNames(input))
      checkboxGroupInput("checkedTests",
                         label = h3("Test to use in the reliability plot"),
                         choices = test_suites,
                          selected = test_suites)

    })
    
    output$active_test_suite <- renderText({
      dirs <- full()
      ifelse(
        is.null(input$test_suite_path),
        "",
        dirs[endsWith(dirs, input$test_suite_path)]
        )
      
    })
    
    output$range_reliability <- renderUI({
      testResult <- loadTestForHeatmaps(input, "Metric print")
      return(
        HTML(
            paste(
              paste( "Reliability:", calculatePostPresentationReliabilityCached(testResult, roundRange=input$application_plot_range) ),
              paste( "OldReliability:", calculateReliabilityCached(testResult, roundRange=input$application_plot_range) ),
              paste( "OldWeakReliability:", calculateWeakReliabilityCached(testResult, roundRange=input$application_plot_range) ),
              paste( "Stability:", calculateStabilityCached(testResult, roundRange=input$application_plot_range) ),
              paste( "MeanOffSlot:", meanOffSlot(testResult, roundRange=input$application_plot_range) ),
              sep = "</br>"
            )
          )
      )
    })
    
    output$application_plot <- renderPlot({
        testResult <- loadTestForHeatmaps(input, "Application heatmap")
        return(plotHeatmap(testResult, input$application_plot_range))
    })
    
    output$reliability_heatmap <- renderPlot({
        testResult <- loadTestForHeatmaps(input, "Reliability heatmap")
        return(plotReliabilityHeatmap(testResult, input$application_plot_range))
    })
    
    output$wrong_chstate_heatmap <- renderPlot({
        testResult <- loadTestForHeatmaps(input, "Wrong CH state heatmap")
        return(plotWrongCHStateHeatmap(testResult, input$application_plot_range))
    })
    
    output$reliability_plot <- renderPlot({
      
      partialNames <- input$checkedTests
      fullNames <- lookupFullNames(partialNames)
      if (length(fullNames) == 0) {
        return(plot(1,1))
      }
      
      print(input$checkedTests)
      
      testSuites <- lapply(fullNames, loadResultsFromTestSuitePath)
      stats <- do.call("rbind", mapply(function(testSuite, partialName) {
        do.call("rbind", lapply(testSuite, function(result) {
          if(is.na(result)) {
            return(NA)
          }
          testName <- sub(".+?-motes-(.+?x.+?)-(random|spread)", "\\1", result@testName)
          data.frame(name=testName, reliability=reliability(result), test_suite=partialName, spread=calculateSpread(result))
        }))
      }, testSuites, partialNames, SIMPLIFY = F))
      stats$name <- factor(stats$name, levels = unique(stats$name[order(stats$spread)]))
      
      return(
        ggplot(stats, aes(name, reliability, color=test_suite)) +
          geom_point(size=5, position=position_dodge(width=0.3)) +
          theme(
            axis.text.x=element_text(angle=45, hjust=1),
            plot.margin=unit(c(1,1,1,2),"cm"),
            text = element_text(size=20)
            ) +
          xlab("Test Name") +
          ylab("Reliability") +
          ylim(c(0, 1))
      )

    })
    
    output$new_reliability_plot <- renderPlot({
      
      partialNames <- input$checkedTests
      fullNames <- lookupFullNames(partialNames)
      if (length(fullNames) == 0) {
        return(plot(1,1))
      }
      
      print(input$checkedTests)
      
      testSuites <- lapply(fullNames, loadResultsFromTestSuitePath)
      stats <- do.call("rbind", mapply(function(testSuite, partialName) {
        do.call("rbind", lapply(testSuite, function(result) {
          if(is.na(result)) {
            return(NA)
          }
          testName <- sub(".+?-motes-(.+?x.+?)-(random|spread)", "\\1", result@testName)
          data.frame(name=testName, reliability=calculatePostPresentationReliabilityCached(result), test_suite=partialName, spread=calculateSpread(result))
        }))
      }, testSuites, partialNames, SIMPLIFY = F))
      stats$name <- factor(stats$name, levels = unique(stats$name[order(stats$spread)]))
      
      return(
        ggplot(stats, aes(name, reliability, color=test_suite)) +
          geom_point(size=5, position=position_dodge(width=0.3)) +
          theme(
            axis.text.x=element_text(angle=45, hjust=1),
            plot.margin=unit(c(1,1,1,2),"cm"),
            text = element_text(size=20)
          ) +
          xlab("Test Name") +
          ylab("New Reliability") +
          ylim(c(0, 1)) +
          scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0,1))
      )
      
    })
    
    output$stability_plot <- renderPlot({
      
      partialNames <- input$checkedTests
      fullNames <- lookupFullNames(partialNames)
      if (length(fullNames) == 0) {
        return(plot(1,1))
      }
      
      print(input$checkedTests)
      
      testSuites <- lapply(fullNames, loadResultsFromTestSuitePath)
      stats <- do.call("rbind", mapply(function(testSuite, partialName) {
        do.call("rbind", lapply(testSuite, function(result) {
          if(is.na(result)) {
            return(NA)
          }
          testName <- sub(".+?-motes-(.+?x.+?)-(random|spread)", "\\1", result@testName)
          data.frame(name=testName, reliability=calculateStabilityCached(result), test_suite=partialName, spread=calculateSpread(result))
        }))
      }, testSuites, partialNames, SIMPLIFY = F))
      stats$name <- factor(stats$name, levels = unique(stats$name[order(stats$spread)]))
      
      return(
        ggplot(stats, aes(name, reliability, color=test_suite)) +
          geom_point(size=5, position=position_dodge(width=0.3)) +
          theme(
            axis.text.x=element_text(angle=45, hjust=1),
            plot.margin=unit(c(1,1,1,2),"cm"),
            text = element_text(size=20)
          ) +
          xlab("Test Name") +
          ylab("Stability") +
          ylim(c(0, 1)) + 
          scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0,1))
      )
      
    })
    
    output$latency_plot <- renderPlot({
      
      partialNames <- input$checkedTests
      fullNames <- lookupFullNames(partialNames)
      if (length(fullNames) == 0) {
        return(plot(1,1))
      }
      
      print(input$checkedTests)
      
      
      
      testSuites <- lapply(fullNames, loadResultsFromTestSuitePath)
      stats <- do.call("rbind", mapply(function(testSuite, partialName) {
        do.call("rbind", lapply(testSuite, function(result) {
          if(is.na(result)) {
            return(NA)
          }
          data.frame(name=result@testName, meanOffSlot=meanOffSlot(result), sdOffSlot=sdOffSlot(result), test_suite=partialName, spread=calculateSpread(result))
        }))
      }, testSuites, partialNames, SIMPLIFY = F))
      stats$name <- factor(stats$name, levels = unique(stats$name[order(stats$spread)]))
      
      return(
        ggplot(stats) +
          geom_pointrange(aes(name, meanOffSlot, color=test_suite, ymax=meanOffSlot+sdOffSlot, ymin=meanOffSlot-sdOffSlot), size=1, position=position_dodge(width=0.3)) +
          theme(
            axis.text.x=element_text(angle=45, hjust=1),
            plot.margin=unit(c(1,1,1,2),"cm"),
            text = element_text(size=20)
          ) +
          xlab("Test Name") +
          ylab("Latency")
      )
      
    })
    
    
    output$energy_plot <- renderPlot({
      
      partialNames <- input$checkedTests
      fullNames <- lookupFullNames(partialNames)
      if (length(fullNames) == 0) {
        return(plot(1,1))
      }
      
      print(input$checkedTests)
      
      
      
      testSuites <- lapply(fullNames, loadResultsFromTestSuitePath)
      stats <- do.call("rbind", mapply(function(testSuite, partialName) {
        do.call("rbind", lapply(testSuite, function(result) {
          if(is.na(result)) {
            return(NA)
          }
          data.frame(name=result@testName, totalPowerUsage=totalPowerUsage(result), test_suite=partialName, spread=calculateSpread(result))
        }))
      }, testSuites, partialNames, SIMPLIFY = F))
      stats$name <- factor(stats$name, levels = unique(stats$name[order(stats$spread)]))
      
      return(
        ggplot(stats) +
          geom_point(aes(name, totalPowerUsage, color=test_suite), size=1, position=position_dodge(width=0.3)) +
          theme(
            axis.text.x=element_text(angle=45, hjust=1),
            plot.margin=unit(c(1,1,1,2),"cm"),
            text = element_text(size=20)
          ) +
          xlab("Test Name") +
          ylab("Total power useage")
      )
      
    })
    
    output$average_ch_count_plot <- AverageCHCountPlot(input)
    output$demoted_ch_plot <- DemotedCHPlot(input)
    output$total_ch_plot <- TotalCHPlot(input)
    output$ch_count_after_demotion<- CHCountAfterDemotionPlot(input)
    output$average_associating_ch_count_plot <- AverageAssociatingNodes(input)
    
    observeEvent(input$pdf, {
      abs_test_suite_path <- lookupFullNames(input$test_suite_path)
      tests <- testNames(abs_test_suite_path)
      index <- which(sapply(tests, function(result) result == input$num))
      system(paste("open ", abs_test_suite_path, "/", tests[index], "/locations.pdf", sep = ''))
    })
  }
)
