source("PlotReliability.R")

clustering_on <- c(
  file.path(evaluation_directory, "chaos-comparison/comparing-200-motes-test/cluster-on/run1"),
  file.path(evaluation_directory, "chaos-comparison/comparing-200-motes-test/cluster-on/run2"),
  file.path(evaluation_directory, "chaos-comparison/comparing-200-motes-test/cluster-on/run3")
)

clustering_off <- c(
  file.path(evaluation_directory, "chaos-comparison/comparing-200-motes-test/cluster-off/chaos-200-motes_1_2018-05-30_14.16.19"),
  file.path(evaluation_directory, "chaos-comparison/comparing-200-motes-test/cluster-off/chaos-200-motes_2_2018-06-02_17.21.50"),
  file.path(evaluation_directory, "chaos-comparison/comparing-200-motes-test/cluster-off/chaos-200-motes_3_2018-06-05_13.48.05")
)



chaos_comparison_labels <- c("Clustering", "A2")


chaos_comparison_test_suites <- list(clustering_on, clustering_off)
chaos_comparison_loaded <- mclapply(chaos_comparison_test_suites, function(test_suite_vector) mclapply(test_suite_vector, loadResultsFromTestSuitePath, mc.cores = 4), mc.cores = 4)
#chaos_comparison_loaded <- lapply(chaos_comparison_test_suites, function(test_suite_vector) lapply(test_suite_vector, loadResultsFromTestSuitePath))

#run(chaos_comparison_loaded, chaos_comparison_labels, "ChaosComparison_200_OldReliability.pdf", reliability, "Protocol", legendBottomLeftCorner, pdfWidth, pdfHeight, xyratio=fiveTestsXYRatio, ylim=c(0, 1, 0.20), ylab=reliabilityText)
run(chaos_comparison_loaded, chaos_comparison_labels, "ChaosComparison_200_Reliability.pdf", calculatePostPresentationReliabilityCached, "Protocol", legendBottomLeftCorner, pdfWidth, pdfHeight, xyratio=fiveTestsXYRatio, ylim=c(0, 1, 0.20), ylab=reliabilityText)
run(chaos_comparison_loaded, chaos_comparison_labels, "ChaosComparison_200_Stability.pdf", calculateStabilityCached, "Protocol", legendBottomLeftCorner, pdfWidth, pdfHeight, xyratio=fiveTestsXYRatio, ylim=c(0, 1, 0.20), ylab=stabilityText)
#run(chaos_comparison_loaded, chaos_comparison_labels, "ChaosComparison_200_WeakReliability.pdf", weakReliability, "Protocol", legendBottomLeftCorner, pdfWidth, pdfHeight, xyratio=fiveTestsXYRatio, ylim=c(0, 1, 0.20), ylab=reliabilityText)
run(chaos_comparison_loaded, chaos_comparison_labels, "ChaosComparison_200_Latency.pdf", getOffSlots, "Protocol", legendTopRightCorner, pdfWidth, pdfHeight, fiveTestsLatencyXYRatio, ylim=c(0, 150, 20), ylab=latencyText)
run(chaos_comparison_loaded, chaos_comparison_labels, "ChaosComparison_200_Energy.pdf", totalPowerUsage, "Protocol",legendBottomLeftCorner, pdfWidth, pdfHeight, fiveTestsEnergyXYRatio, ylim=c(120, 150, 10), ylab=energyText)

clustering_off_50 <- c(
  file.path(evaluation_directory, "chaos-comparison/comparing-50-motes-test/cluster-off/clustering_off_1_2018-05-28_20.29.23"),
  file.path(evaluation_directory, "chaos-comparison/comparing-50-motes-test/cluster-off/clustering_off_2_2018-05-28_23.09.24"),
  file.path(evaluation_directory, "chaos-comparison/comparing-50-motes-test/cluster-off/clustering_off_3_2018-05-29_01.43.03"),
  file.path(evaluation_directory, "chaos-comparison/comparing-50-motes-test/cluster-off/clustering_off_4_2018-05-29_04.12.00"),
  file.path(evaluation_directory, "chaos-comparison/comparing-50-motes-test/cluster-off/clustering_off_5_2018-05-29_06.51.35"),
  file.path(evaluation_directory, "chaos-comparison/comparing-50-motes-test/cluster-off/clustering_off_6_2018-05-29_09.21.44"),
  file.path(evaluation_directory, "chaos-comparison/comparing-50-motes-test/cluster-off/clustering_off_7_2018-05-28_10.20.58"),
  file.path(evaluation_directory, "chaos-comparison/comparing-50-motes-test/cluster-off/clustering_off_8_2018-05-28_10.20.36"),
  file.path(evaluation_directory, "chaos-comparison/comparing-50-motes-test/cluster-off/clustering_off_9_2018-05-28_15.03.46")
)

clustering_on_50 <- c(
  file.path(evaluation_directory, "chaos-comparison/comparing-50-motes-test/cluster-on/run1"),
  file.path(evaluation_directory, "chaos-comparison/comparing-50-motes-test/cluster-on/run2"),
  file.path(evaluation_directory, "chaos-comparison/comparing-50-motes-test/cluster-on/run3")
)
chaos_comparison_labels <- c("Clustering", "A2")


chaos_comparison_test_suites <- list(clustering_on_50, clustering_off_50)
chaos_comparison_loaded <- mclapply(chaos_comparison_test_suites, function(test_suite_vector) mclapply(test_suite_vector, loadResultsFromTestSuitePath, mc.cores = 4), mc.cores = 4)


#run(chaos_comparison_loaded, chaos_comparison_labels, "ChaosComparison_50_OldReliability.pdf", reliability, "Protocol", legendBottomLeftCorner, pdfWidth, pdfHeight, xyratio=nineTestsXYRatio, ylim=c(0, 1, 0.20), ylab=reliabilityText)
run(chaos_comparison_loaded, chaos_comparison_labels, "ChaosComparison_50_Reliability.pdf", calculatePostPresentationReliabilityCached, "Protocol", legendBottomLeftCorner, pdfWidth, pdfHeight, xyratio=nineTestsXYRatio, ylim=c(0, 1, 0.20), ylab=reliabilityText)
run(chaos_comparison_loaded, chaos_comparison_labels, "ChaosComparison_50_Stability.pdf", calculateStabilityCached, "Protocol", legendBottomLeftCorner, pdfWidth, pdfHeight, xyratio=nineTestsXYRatio, ylim=c(0, 1, 0.20), ylab=stabilityText)
#run(chaos_comparison_loaded, chaos_comparison_labels, "ChaosComparison_50_WeakReliability.pdf", weakReliability, "Protocol", legendBottomLeftCorner, pdfWidth, pdfHeight, xyratio=nineTestsXYRatio, ylim=c(0, 1, 0.20), ylab=reliabilityText)
run(chaos_comparison_loaded, chaos_comparison_labels, "ChaosComparison_50_Latency.pdf", getOffSlots, "Protocol", legendTopRightCorner, pdfWidth, pdfHeight, nineTestsLatencyXYRatio, ylim=c(0, 150, 20), ylab=latencyText)
run(chaos_comparison_loaded, chaos_comparison_labels, "ChaosComparison_50_Energy.pdf", totalPowerUsage, "Protocol", legendBottomLeftCorner, pdfWidth, pdfHeight, nineTestsEnergyXYRatio, ylim=c(120, 150, 10), ylab=energyText)


