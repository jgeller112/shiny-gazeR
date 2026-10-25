# Browser (shinylive / webR) entry point for shiny gazeR.
#
# Everything runs inside the visitor's browser: data are never uploaded to a
# server. gazeR and shinygazer aren't on CRAN; r-universe builds them for
# WebAssembly, and they are installed from there at start-up. Their CRAN
# dependencies are listed below so shinylive bundles them with the site.
#
# To run locally instead, use shinygazer::run_app().

universe <- "https://jgeller112.r-universe.dev"

if (FALSE) {
  # Not run: lets shinylive detect and bundle these packages
  library(shiny); library(bslib); library(plotly); library(DT); library(jsonlite)
  library(markdown); library(data.table); library(dplyr); library(tibble); library(zoo)
  library(ggplot2); library(lme4); library(purrr); library(readr); library(signal)
  library(stringr); library(tidyr); library(zip)
}

# Only in the browser: in regular R, run this file (or shinygazer::run_app())
# with the packages installed as usual.
# webr is looked up at run time (not written as webr::install) so shinylive's
# dependency scan doesn't try to bundle it: it only exists in the browser.
if (grepl("emscripten", R.version$os)) {
  webr_install <- get("install", envir = asNamespace("webr"))
  webr_install(c("gazer", "shinygazer"), repos = c(universe, "https://repo.r-wasm.org"))
}

shiny::shinyAppDir(system.file("app", package = "shinygazer"))
