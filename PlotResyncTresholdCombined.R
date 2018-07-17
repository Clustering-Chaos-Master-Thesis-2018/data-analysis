source("PlotReliability.R")


resync_treshold_1_comp_1_test_suites <- c(
  file.path(evaluation_directory, "/resync-treshold/resync-treshold-1/competition-radius-1_resync-1_2018-05-18_1/"),
  file.path(evaluation_directory, "/resync-treshold/resync-treshold-1/competition-radius-1_resync-1_2018-05-19_2/"),
  file.path(evaluation_directory, "/resync-treshold/resync-treshold-1/competition-radius-1_resync-1_2018-05-20_3/")
)

resync_treshold_1_comp_2_test_suites <- c(
  file.path(evaluation_directory, "/resync-treshold/resync-treshold-1/competition-radius-2_resync-1_2018-05-18_1/"),
  file.path(evaluation_directory, "/resync-treshold/resync-treshold-1/competition-radius-2_resync-1_2018-05-18_1/"),
  file.path(evaluation_directory, "/resync-treshold/resync-treshold-1/competition-radius-2_resync-1_2018-05-21_3/")
)

resync_treshold_1_comp_3_test_suites <- c(
  file.path(evaluation_directory, "/resync-treshold/resync-treshold-1/competition-radius-3_resync-1_2018-05-19_1/"),
  file.path(evaluation_directory, "/resync-treshold/resync-treshold-1/competition-radius-3_resync-1_2018-05-20_2/"),
  file.path(evaluation_directory, "/resync-treshold/resync-treshold-1/competition-radius-3_resync-1_2018-05-21_3/")
)

resync_treshold_2_comp_1_test_suites <- c(
  file.path(evaluation_directory, "/resync-treshold/resync-treshold-2/competition-radius-1_resync-2_2018-05-24_1/"),
  file.path(evaluation_directory, "/resync-treshold/resync-treshold-2/competition-radius-1_resync-2_2018-05-24_2/"),
  file.path(evaluation_directory, "/resync-treshold/resync-treshold-2/competition-radius-1_resync-2_2018-05-24_3/")
)

resync_treshold_2_comp_2_test_suites <- c(
  file.path(evaluation_directory, "/resync-treshold/resync-treshold-2/competition-radius-2_resync-2_2018-05-24_1/"),
  file.path(evaluation_directory, "/resync-treshold/resync-treshold-2/competition-radius-2_resync-2_2018-05-24_2/"),
  file.path(evaluation_directory, "/resync-treshold/resync-treshold-2/competition-radius-2_resync-2_2018-05-24_3/")
)

resync_treshold_2_comp_3_test_suites <- c(
  file.path(evaluation_directory, "/resync-treshold/resync-treshold-2/competition-radius-3_resync-2_2018-05-24_1/"),
  file.path(evaluation_directory, "/resync-treshold/resync-treshold-2/competition-radius-3_resync-2_2018-05-24_2/"),
  file.path(evaluation_directory, "/resync-treshold/resync-treshold-2/competition-radius-3_resync-2_2018-05-24_3/")
)

resync_treshold_3_comp_1_test_suites <- c(
  file.path(evaluation_directory, "competition-radius/competition-radius-1_1_2018-05-26_17.05.14/"),
  file.path(evaluation_directory, "competition-radius/competition-radius-1_2_2018-05-27_03.07.25/"),
  file.path(evaluation_directory, "competition-radius/competition-radius-1_3_2018-05-27_20.42.28/")
)

resync_treshold_3_comp_2_test_suites <- c(
  file.path(evaluation_directory, "competition-radius/competition-radius-2_1_2018-05-26_21.12.48/"),
  file.path(evaluation_directory, "competition-radius/competition-radius-2_2_2018-05-27_07.23.09/"),
  file.path(evaluation_directory, "competition-radius/competition-radius-2_3_2018-05-28_00.54.29/")
)

resync_treshold_3_comp_3_test_suites <- c(
  file.path(evaluation_directory, "competition-radius/competition-radius-3_1_2018-05-27_00.05.08/"),
  file.path(evaluation_directory, "competition-radius/competition-radius-3_2_2018-05-27_11.19.51/"),
  file.path(evaluation_directory, "competition-radius/competition-radius-3_3_2018-05-28_03.40.21/")
)



competition_radius_labels <- c("1:1  ", "1:2  ", "1:3  ", "2:1  ", "2:2  ", "2:3  ", "3:1  ", "3:2  ", "3:3  ")

competition_radius_test_suites <- list(resync_treshold_1_comp_1_test_suites, resync_treshold_1_comp_2_test_suites, resync_treshold_1_comp_3_test_suites,
                                       resync_treshold_2_comp_1_test_suites, resync_treshold_2_comp_2_test_suites, resync_treshold_2_comp_3_test_suites,
                                       resync_treshold_3_comp_1_test_suites, resync_treshold_3_comp_2_test_suites, resync_treshold_3_comp_3_test_suites)

competition_radius_loaded <- mclapply(competition_radius_test_suites, function(test_suite_vector) mclapply(test_suite_vector, loadResultsFromTestSuitePath, mc.cores = 4), mc.cores = 4)


#run(competition_radius_loaded, competition_radius_labels, "ResyncThresholdCombined_OldReliability.pdf", reliability, "Competition Radius (hops)", c(0.735, 0.80), pdfWidth, pdfHeight, nineTestsXYRatio, ylim=c(0, 1, 0.20), ylab=reliabilityText)
run(competition_radius_loaded, competition_radius_labels, "ResyncThresholdCombined_Reliability.pdf", calculatePostPresentationReliabilityCached, "Competition Radius (hops)", legendBottomLeftCorner, pdfWidth, pdfHeight, nineTestsXYRatio, ylim=c(0, 1, 0.20), ylab=reliabilityText)
run(competition_radius_loaded, competition_radius_labels, "ResyncThresholdCombined_Stability.pdf", calculateStabilityCached, "Competition Radius (hops)", legendBottomLeftCorner, pdfWidth, pdfHeight, nineTestsXYRatio, ylim=c(0, 1, 0.20), ylab=stabilityText)
#run(competition_radius_loaded, competition_radius_labels, "ResyncThresholdCombined_WeakReliability.pdf", weakReliability, "Competition Radius (hops)", c(0.735, 0.80), pdfWidth, pdfHeight, nineTestsXYRatio, ylim=c(0, 1, 0.20), ylab=reliabilityText)
run(competition_radius_loaded, competition_radius_labels, "ResyncThresholdCombined_Latency.pdf", getOffSlots, "Competition Radius (hops)", legendTopLeftCorner, pdfWidth, pdfHeight, nineTestsLatencyXYRatio, ylim=c(0, 150, 20), ylab=latencyText)

#dev.off()
