source("PlotReliability.R")

comp_1_test_suites <- c(
  file.path(evaluation_directory, "competition-radius/competition-radius-1_1_2018-05-26_17.05.14/"),
  file.path(evaluation_directory, "competition-radius/competition-radius-1_2_2018-05-27_03.07.25/"),
  file.path(evaluation_directory, "competition-radius/competition-radius-1_3_2018-05-27_20.42.28/")
)

comp_2_test_suites <- c(
  file.path(evaluation_directory, "competition-radius/competition-radius-2_1_2018-05-26_21.12.48/"),
  file.path(evaluation_directory, "competition-radius/competition-radius-2_2_2018-05-27_07.23.09/"),
  file.path(evaluation_directory, "competition-radius/competition-radius-2_3_2018-05-28_00.54.29/")
)

comp_3_test_suites <- c(
  file.path(evaluation_directory, "competition-radius/competition-radius-3_1_2018-05-27_00.05.08/"),
  file.path(evaluation_directory, "competition-radius/competition-radius-3_2_2018-05-27_11.19.51/"),
  file.path(evaluation_directory, "competition-radius/competition-radius-3_3_2018-05-28_03.40.21/")
)

competition_radius_labels <- c("1  ", "2  ", "3  ")

competition_radius_test_suites <- list(comp_1_test_suites, comp_2_test_suites, comp_3_test_suites)
competition_radius_loaded <- mclapply(competition_radius_test_suites, function(test_suite_vector) mclapply(test_suite_vector, loadResultsFromTestSuitePath, mc.cores = 4), mc.cores = 4)


#run(competition_radius_loaded, competition_radius_labels, "CompetitionRadius_OldReliability.pdf", reliability, "Competition Radius (hops)", legendBottomLeftCorner, pdfWidth, pdfHeight, xyratio=nineTestsXYRatio, ylim=c(0, 1, 0.20), ylab=reliabilityText)
run(competition_radius_loaded, competition_radius_labels, "CompetitionRadius_Reliability.pdf", calculatePostPresentationReliabilityCached, "Competition Radius (hops)", legendBottomLeftCorner, pdfWidth, pdfHeight, xyratio=nineTestsXYRatio, ylim=c(0, 1, 0.20), ylab=reliabilityText)
run(competition_radius_loaded, competition_radius_labels, "CompetitionRadius_Stability.pdf", calculateStabilityCached, "Competition Radius (hops)", legendBottomLeftCorner, pdfWidth, pdfHeight, xyratio=nineTestsXYRatio, ylim=c(0, 1, 0.20), ylab=stabilityText)
#run(competition_radius_loaded, competition_radius_labels, "CompetitionRadius_WeakReliability.pdf", weakReliability, "Competition Radius (hops)", legendBottomLeftCorner, pdfWidth, pdfHeight, xyratio=nineTestsXYRatio, ylim=c(0, 1, 0.20), ylab=reliabilityText)
run(competition_radius_loaded, competition_radius_labels, "CompetitionRadius_Latency.pdf", getOffSlots, "Competition Radius (hops)", legendTopLeftCorner, pdfWidth, pdfHeight, xyratio=nineTestsLatencyXYRatio, ylim=c(0, 150, 20), ylab=latencyText)
