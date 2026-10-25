#' @keywords internal
#' @importFrom data.table := .I .N .SD
"_PACKAGE"

# Packages used only inside the app (inst/app); listed here so R CMD check
# sees them as used.
#' @importFrom bslib page_sidebar
#' @importFrom DT datatable
#' @importFrom jsonlite write_json
#' @importFrom markdown mark_html
#' @importFrom plotly plot_ly
#' @importFrom zoo na.approx
#' @importFrom dplyr mutate
#' @importFrom gazer extend_blinks
#' @importFrom stats median
NULL

utils::globalVariables(c("trial", "time", "value", "name", "row_id", "text"))
