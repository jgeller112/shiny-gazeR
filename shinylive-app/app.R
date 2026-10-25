# Browser (shinylive / webR) entry point for shiny gazeR.
#
# Everything runs inside the visitor's browser: data are never uploaded to a
# server. gazeR and shinygazer come from r-universe
# (https://jgeller112.r-universe.dev), which builds them for WebAssembly;
# shinylive bundles them, and the CRAN packages listed below, with the site.
#
# To run locally instead, use shinygazer::run_app().

if (FALSE) {
  # Not run: lets shinylive detect and bundle these packages
  library(shiny); library(bslib); library(plotly); library(DT); library(jsonlite)
  library(markdown); library(data.table); library(dplyr); library(tibble); library(zoo)
  library(ggplot2); library(purrr); library(readr); library(signal)
  library(stringr); library(tidyr); library(zip)
}

library(shinygazer)
library(gazer)

shiny::shinyAppDir(system.file("app", package = "shinygazer"))
