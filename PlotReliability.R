library(dplyr)

run <- function(test_suites, group_labels, plot_name, meanFunction, label, position, width, height, xyratio, ylim = NA, xlab = expression(paste("Network Size ", (m^2))), ylab, num_cols = 3) {
  the_plot <- plot_reliability(test_suites, group_labels, meanFunction, label, position, xyratio, ylim, xlab, ylab, num_cols)
  ggsave(file.path(evaluation_directory, "Plots", plot_name),  plot=the_plot, width=width, height = height)
}

label_and_flatten_data <- function(test_suite_groups, group_labels, meanFunction) {
  a <- mcmapply(function(list_of_test_suite_with_same_comp_radius, group_label) {
    b <- do.call("rbind", mclapply(list_of_test_suite_with_same_comp_radius, function(test_suite) {
      dc <- do.call("rbind", lapply(test_suite, function(test) {
        if(is.na(test)) {
          return(NA)
        }
        #Only include the network spread in the plot.
        testName <- sub(".+?-motes-(.+?x.+?)-(random|spread)", "\\1", test@testName)
        if(testName == "flocklab") {
          testName = ""
        }
        if(length(meanFunction(test)) == 0) {
          return(NA)
        }
        if(grepl("clustering-off", test@testDirectory) && (identical(meanFunction, reliability) || identical(meanFunction, calculatePostPresentationReliabilityCached))) {
          if(identical(meanFunction, reliability)) {
            data.frame(simulation_name=testName, reliability=chaos_reliability(test), group=group_label, spread=calculateSpread(test))
          } else if(identical(meanFunction, calculatePostPresentationReliabilityCached)) {
            data.frame(simulation_name=testName, reliability=calculatePostPresentationChaosReliabilityCached(test), group=group_label, spread=calculateSpread(test))
          }
        } else {
          data.frame(simulation_name=testName, reliability=meanFunction(test), group=group_label, spread=calculateSpread(test)) 
        }
      }))
      dc
    }, mc.cores = 2))
    b
  }, test_suite_groups, group_labels, SIMPLIFY = F, mc.cores = 4)
  do.call("rbind", a)
}

plot_reliability <- function(test_suite_groups, group_labels, reliabilityFunction, label, legend_position, xyratio, ylim, xlab, ylab, num_cols) {
  if(length(test_suite_groups) != length(group_labels)) {
    stop("Requires same length on number of test suite groups and labels")
  }
  cbPalette <- c("#000000", "#009E73", "#56B4E9", "#D55E00", "#000000", "#009E73", "#56B4E9", "#D55E00", "#000000", "#009E73", "#56B4E9", "#D55E00")
  stats <- label_and_flatten_data(test_suite_groups, group_labels, reliabilityFunction)

  # Aggregate reliability for rows with same spread. Create mean and sd
  agg <- aggregate(reliability~simulation_name+group+spread, stats, function(a) c(mean=mean(a), sd=sd(a)))
  agg <- do.call(data.frame, agg)

  #stats <- stats[complete.cases(stats),]
  #order by spread
  
  plot <- NA
  if(identical(reliabilityFunction, reliability) ||
     identical(reliabilityFunction, chaos_reliability) ||
     identical(reliabilityFunction, calculatePostPresentationReliabilityCached) ||
     identical(reliabilityFunction, calculateStabilityCached)) {
    
    stats$simulation_name <- factor(stats$simulation_name, levels = stats$simulation_name[order(unique(stats$spread))])
    #browser()
    pos = position_dodge(width = 0)
    data = stats %>%
      group_by(simulation_name, group) %>%
      summarise(min = min(reliability), max = max(reliability), 
                reliability = mean(reliability))
  
     plot <- ggplot(data, mapping = aes(simulation_name, reliability, color=group, linetype = group)) +
      geom_errorbar(aes(ymin = min, ymax = max), width = 0.4, size = 0.4, position = pos, show.legend = F) +
      geom_point(position = pos, show.legend = F, size = 1) +
      geom_line(aes(group=group), show.legend = T, position = pos, size = 0.4)

    #if(identical(reliabilityFunction, calculateStabilityCached)) {
    #  ylab="Stability"
    #} else {
    #  ylab="Reliability"
    #}
    
  } else {
    agg$simulation_name <- factor(agg$simulation_name, levels = unique(agg$simulation_name[order(agg$spread)]))
    pos = position_dodge(width = 0)
    data = stats %>%
      group_by(simulation_name, group) %>%
      summarise(min = mean(reliability) - sd(reliability), max = mean(reliability) + sd(reliability), 
                reliability = mean(reliability))
    
    plot <- ggplot(data, mapping = aes(simulation_name, reliability, color=group, linetype = group)) +
      geom_errorbar(aes(ymin = min, ymax = max), width = 0.4, size = 0.4, position = pos, show.legend = F) +
      geom_point(position = pos, show.legend = F, size = 1) +
      geom_line(aes(group=group), show.legend = T, position = pos, size = 0.4)
    #plot <- ggplot(agg) + geom_pointrange(
    #  size=1.5,
    #  aes(
    #    simulation_name,
    #    reliability.mean,
    #    ymax=reliability.mean+reliability.sd,
    #    ymin=reliability.mean-reliability.sd,
    #    color=group
    #  ),
    #  position = position_dodge(width = 0.5))
  }
  
  plot <- plot + ylab(ylab) + 
    xlab(xlab) +
    labs(color=label) +
    guides(color=guide_legend(ncol=num_cols)) +
    scale_color_manual(name = label, values = cbPalette) +
    scale_linetype_manual(name = label, values = order(cbPalette)) +
    theme(
      text = element_text(size=9),
      #axis.text.x=element_text(angle=45, hjust=1),
      legend.justification = c(0, 0),
      legend.position = legend_position,
      plot.margin=grid::unit(c(0,0,0,0), "mm")
    ) + coord_fixed(xyratio, ylim = ylim) + scale_y_continuous(breaks = do.call(seq, as.list(ylim)))
  
  return(plot)
}
#run(chaos_comparison_loaded, chaos_comparison_labels, "ChaosComparison_200_Reliability.pdf", calculatePostPresentationReliabilityCached, "Protocol", legendBottomLeftCorner, pdfWidth, pdfHeight, xyratio=fiveTestsXYRatio, ylim=c(0,1), ylab=reliabilityText)
#run(chaos_comparison_loaded, chaos_comparison_labels, "ChaosComparison_200_Stability.pdf", calculateStabilityCached, "Protocol", legendBottomLeftCorner, pdfWidth, pdfHeight, xyratio=fiveTestsXYRatio, ylim=c(0,1), ylab=stabilityText)
#run(competition_radius_loaded, competition_radius_labels, "ResyncThreshold2.pdf", "Competition Radius", c(0.735, 0.80), 13, 6, 3)
