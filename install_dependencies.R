dependencies <- c("functional", "foreach", "doMC", "data.table", "devtools", "shiny", "xml2", "RColorBrewer", "plyr")
install.packages(dependencies)

library(devtools)
# Require dev install to work with shiny
devtools::install_github("r-lib/memoise")
# Require dev since ggplot from CRAN is not available for r version 3.5.0
devtools::install_github("tidyverse/ggplot2")