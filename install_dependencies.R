dependencies <- c("functional", "foreach", "doMC", "data.table", "devtools", "shiny", "xml2", "RColorBrewer", "plyr")

lapply(dependencies, function(dep) {
  if(!dep %in% rownames(installed.packages())) {
    install.packages(dep)
  }
})

library(devtools)
dev_dependencies <- c("r-lib/memoise", "tidyverse/ggplot2")

lapply(dev_dependencies, function(repo) {
  dep_name <- strsplit(repo,'/')[[1]][[2]]
  if(!dep_name %in% rownames(installed.packages())) {
    devtools::install_github(dep)
  }
})
