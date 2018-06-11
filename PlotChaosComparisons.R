source("PlotReliability.R")

clustering_on <- c(
  file.path(evaluation_directory, "chaos-comparison/200-comp1-max30-min4/cluster-200-comp1-max30-min4_1_2018-05-30_16.08.18/"),
  file.path(evaluation_directory, "chaos-comparison/200-comp1-max30-min4/cluster-200-comp1-max30-min4_2_2018-06-01_13.16.48/"),
  file.path(evaluation_directory, "chaos-comparison/200-comp1-max30-min4/cluster-200-comp1-max30-min4_3_2018-06-03_09.33.51/")
)

clustering_off <- c(
  file.path(evaluation_directory, "/chaos-comparison/clustering-off/chaos-200-motes_1_2018-05-30_14.16.19"),
  file.path(evaluation_directory, "/chaos-comparison/clustering-off/chaos-200-motes_2_2018-06-02_17.21.50"),
  file.path(evaluation_directory, "/chaos-comparison/clustering-off/chaos-200-motes_3_2018-06-05_13.48.05")
)



chaos_comparison_labels <- c("Clustering On", "Clustering Off")


chaos_comparison_test_suites <- list(clustering_on, clustering_off)
chaos_comparison_loaded <- lapply(chaos_comparison_test_suites, function(test_suite_vector) lapply(test_suite_vector, loadResultsFromTestSuitePath))


run(chaos_comparison_loaded, chaos_comparison_labels, "ChaosComparison.pdf", reliability, "Comparing Clustering", c(0.735, 0.80), 13, 5.5, xyratio=2, ylim=c(0,1), ylab="Reliability (Mean & STDEV)")
run(chaos_comparison_loaded, chaos_comparison_labels, "ChaosComparison_WeakReliability.pdf", weakReliability, "Comparing Clustering", c(0.735, 0.80), 13, 5.5, xyratio=2, ylim=c(0,1), ylab="Reliability (Mean & STDEV)")
run(chaos_comparison_loaded, chaos_comparison_labels, "ChaosComparison_Latency.pdf", getOffSlots, "Comparing Clustering", c(0.05, 0.80), 13, 5.5, 0.012, ylim=c(0, 150), ylab="Latency (Mean & STDEV)")


clustering_off_50 <- c(
  file.path(evaluation_directory, "chaos-comparison/clustering-off/clustering_off_1_2018-05-28_20.29.23"),
  file.path(evaluation_directory, "chaos-comparison/clustering-off/clustering_off_2_2018-05-28_23.09.24"),
  file.path(evaluation_directory, "chaos-comparison/clustering-off/clustering_off_3_2018-05-29_01.43.03"),
  file.path(evaluation_directory, "chaos-comparison/clustering-off/clustering_off_4_2018-05-29_04.12.00"),
  file.path(evaluation_directory, "chaos-comparison/clustering-off/clustering_off_5_2018-05-29_06.51.35"),
  file.path(evaluation_directory, "chaos-comparison/clustering-off/clustering_off_6_2018-05-29_09.21.44"),
  file.path(evaluation_directory, "chaos-comparison/clustering-off/clustering_off_7_2018-05-28_10.20.58"),
  file.path(evaluation_directory, "chaos-comparison/clustering-off/clustering_off_8_2018-05-28_10.20.36"),
  file.path(evaluation_directory, "chaos-comparison/clustering-off/clustering_off_9_2018-05-28_15.03.46")
)

chaos_comparison_labels <- c("Clustering Off")


chaos_comparison_test_suites <- list(clustering_off_50)
chaos_comparison_loaded <- lapply(chaos_comparison_test_suites, function(test_suite_vector) lapply(test_suite_vector, loadResultsFromTestSuitePath))


run(chaos_comparison_loaded, chaos_comparison_labels, "ChaosComparison_50.pdf", reliability, "Comparing Clustering", c(0.735, 0.80), 13, 5.5, xyratio=2, ylim=c(0,1), ylab="Reliability (Mean & STDEV)")
run(chaos_comparison_loaded, chaos_comparison_labels, "ChaosComparison_50_WeakReliability.pdf", weakReliability, "Comparing Clustering", c(0.735, 0.80), 13, 5.5, xyratio=2, ylim=c(0,1), ylab="Reliability (Mean & STDEV)")
run(chaos_comparison_loaded, chaos_comparison_labels, "ChaosComparison_50_Latency.pdf", getOffSlots, "Comparing Clustering", c(0.05, 0.80), 13, 5.5, 0.012, ylim=c(0, 150), ylab="Latency (Mean & STDEV)")


