run <- function(test_suites, group_labels, plot_name, meanFunction, label, position, width, height, xyratio, ylim = NA, xlab = "Network Size (m)", ylab) {
  the_plot <- plot_reliability(test_suites, group_labels, meanFunction, label, position, xyratio, ylim, xlab, ylab)
  ggsave(file.path(evaluation_directory, "Plots", plot_name),  plot=the_plot, width=width, height = height)
}

label_and_flatten_data <- function(test_suite_groups, group_labels, meanFunction) {
  a <- mapply(function(list_of_test_suite_with_same_comp_radius, group_label) {
    b <- do.call("rbind", lapply(list_of_test_suite_with_same_comp_radius, function(test_suite) {
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
        if(grepl("clustering-off", test@testDirectory) && identical(meanFunction, reliability)) {
          data.frame(simulation_name=testName, reliability=chaos_reliability(test), group=group_label, spread=calculateSpread(test))
        } else {
          data.frame(simulation_name=testName, reliability=meanFunction(test), group=group_label, spread=calculateSpread(test)) 
        }
      }))
      dc
    }))
    b
  }, test_suite_groups, group_labels, SIMPLIFY = F)
  do.call("rbind", a)
}

plot_reliability <- function(test_suite_groups, group_labels, reliabilityFunction, label, legend_position, xyratio, ylim, xlab, ylab) {
  if(length(test_suite_groups) != length(group_labels)) {
    stop("Requires same length on number of test suite groups and labels")
  }

  stats <- label_and_flatten_data(test_suite_groups, group_labels, reliabilityFunction)
  #We are calculating the energy used.
  if(identical(reliabilityFunction, totalPowerUsage)) {
    if(nrow(stats[stats$reliability == 1918979041,]) == 1) {
      stats <- stats[stats$reliability != 1918979041,] 
    } else if(nrow(stats[stats$reliability == 7621758712,]) == 1) {
      stats <- stats[stats$reliability != 7621758712,] 
    }
  }
  # Aggregate reliability for rows with same spread. Create mean and sd
  agg <- aggregate(reliability~simulation_name+group+spread, stats, function(a) c(mean=mean(a), sd=sd(a)))
  agg <- do.call(data.frame, agg)
  
  agg$simulation_name <- factor(agg$simulation_name, levels = unique(agg$simulation_name[order(agg$spread)]))
  #stats <- stats[complete.cases(stats),]
  #order by spread
  #stats$simulation_name <- factor(stats$simulation_name, levels = stats$simulation_name[order(unique(stats$spread))])
  plot <- ggplot(agg) +
    geom_pointrange(
      size=1.5,
      aes(
        simulation_name,
        reliability.mean,
        ymax=reliability.mean+reliability.sd,
        ymin=reliability.mean-reliability.sd,
        color=group
      ),
      position = position_dodge(width = 0.5)) +
    ylab(ylab) + 
    xlab(xlab) +
    labs(color=label) +
    guides(color=guide_legend(ncol=3)) +
    theme(
      text = element_text(size=16),
      #axis.text.x=element_text(angle=45, hjust=1),
      legend.justification = c(0, 0),
      legend.position = legend_position,
      plot.margin=grid::unit(c(0,0,0,0), "mm")
    )
  
  plot <- plot + coord_fixed(xyratio, ylim = ylim)
  return(plot)
}

#run(competition_radius_loaded, competition_radius_labels, "ResyncThreshold2.pdf", "Competition Radius", c(0.735, 0.80), 13, 6, 3)
