# Browser (shinylive / webR) entry point for shiny gazeR.
#
# Everything runs inside the visitor's browser: data are never uploaded to a
# server. gazeR and shinygazer aren't on CRAN, so the GitHub Actions
# workflow builds WebAssembly versions of them into this site's /repo, and
# they are installed at start-up. Their CRAN dependencies are listed below so
# shinylive bundles them with the site.
#
# To run locally instead, use shinygazer::run_app().
# If you fork this repository, change `site_repo` to your own GitHub Pages URL.

site_repo <- "https://jgeller112.github.io/shiny-gazeR/repo"

if (FALSE) {
  # Not run: lets shinylive detect and bundle these packages
  library(shiny); library(bslib); library(plotly); library(DT); library(jsonlite)
  library(markdown); library(data.table); library(dplyr); library(tibble); library(zoo)
  library(ggplot2); library(lme4); library(purrr); library(readr); library(signal)
  library(stringr); library(tidyr); library(zip)
}

# Only in the browser: in regular R, run this file (or shinygazer::run_app())
# with the packages installed as usual.
if (grepl("emscripten", R.version$os)) {
  webr::install(c("gazer", "shinygazer"),
                repos = c(site_repo, "https://repo.r-wasm.org"))
}

shiny::shinyAppDir(system.file("app", package = "shinygazer"))
