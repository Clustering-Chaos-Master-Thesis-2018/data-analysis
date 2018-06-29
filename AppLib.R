library(memoise)

load_data <- function(rows) {
  lapply(rows, loadResultFromTestInfoRow)
}

load_data_m <- memoise(load_data)



tests_path <- "/Users/mattias/Exjobb/Evaluation/chaos-comparison/comparing-50-motes-test/cluster-on/"
partial <- function() c(list.dirs(tests_path, recursive = F, full.names = F))
full <- function() c(list.dirs(tests_path, recursive = F, full.names = T))

lookupFullNames <- function(short_names) {
  partial <- partial()
  full <- full()
  lookupTable <- data.frame(partial, full, stringsAsFactors = F)
  lookupTable[lookupTable$partial %in% short_names,]$full
}
