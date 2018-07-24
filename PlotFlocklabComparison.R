source("PlotReliability.R")

clustering_on <- c(
  file.path(evaluation_directory, "chaos-comparison/flocklab/cluster/cluster-comp3-min2-max10-initiator31_1"),
  file.path(evaluation_directory, "chaos-comparison/flocklab/cluster/cluster-comp3-min2-max10-initiator31_3"),
  file.path(evaluation_directory, "chaos-comparison/flocklab/cluster/cluster-comp3-min2-max10-initiator31_7"),
  file.path(evaluation_directory, "chaos-comparison/flocklab/cluster/cluster-comp3-min2-max10-initiator31_8")
)

clustering_off <- c(
  file.path(evaluation_directory, "chaos-comparison/flocklab/clustering-off/cluster-off-initiator-31_1"),
  file.path(evaluation_directory, "chaos-comparison/flocklab/clustering-off/cluster-off-initiator-31_2"),
  file.path(evaluation_directory, "chaos-comparison/flocklab/clustering-off/cluster-off-initiator-31_3"),
  file.path(evaluation_directory, "chaos-comparison/flocklab/clustering-off/cluster-off-initiator-31_4")
)



chaos_comparison_labels <- c("Clustering", "A2")


chaos_comparison_test_suites <- list(clustering_on, clustering_off)
chaos_comparison_loaded <- mclapply(chaos_comparison_test_suites, function(test_suite_vector) mclapply(test_suite_vector, loadResultsFromTestSuitePath, mc.cores = 4), mc.cores = 4)


#run(chaos_comparison_loaded, chaos_comparison_labels, "FlocklabComparison_OldReliability.pdf", reliability, "Protocol", c(2.65, 0.05), 3.5, 5.5, xyratio=2, ylim=c(0, 1, 0.20), ylab=reliabilityText)
run(chaos_comparison_loaded, chaos_comparison_labels, "FlocklabComparison_Reliability.pdf", calculatePostPresentationReliabilityCached, "Protocol", position = c(0.05, 0.05), 3.5, 5.5, xyratio=2, ylim=c(0, 1, 0.20), ylab=reliabilityText, xlab = "Network")
run(chaos_comparison_loaded, chaos_comparison_labels, "FlocklabComparison_Stability.pdf", calculateStabilityCached, "Protocol", position = c(2.65, 0.05), 3.5, 5.5, xyratio=2, ylim=c(0, 1, 0.20), ylab=stabilityText, xlab = "Network")
#run(chaos_comparison_loaded, chaos_comparison_labels, "FlocklabComparison_WeakReliability.pdf", weakReliability, "Protocol", c(0.735, 0.80), 3.5, 5.5, xyratio=2, ylim=c(0, 1, 0.20), ylab=reliabilityText)
run(chaos_comparison_loaded, chaos_comparison_labels, "FlocklabComparison_Latency.pdf", getOffSlots, "Protocol", position = c(2.65, 0.8), 3.5, 5.5, 0.0065, ylim=c(0, 300, 30), ylab=latencyText, xlab = "Network")
run(chaos_comparison_loaded, chaos_comparison_labels, "FlocklabComparison_Energy.pdf", totalPowerUsage, "Protocol", position = c(2.65, 0.80), 3.5, 5.5, 0.032, ylim=c(125, 175, 10), ylab=energyText, xlab = "Network")
