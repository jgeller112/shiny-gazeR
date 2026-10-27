#' @keywords internal
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
#' @importFrom data.table fread
#' @importFrom tibble as_tibble
#' @importFrom utils head
#' @importFrom zip zip
#' @importFrom ggplot2 ggplot
#' @importFrom htmltools htmlEscape
NULL


#' @importFrom gazer read_asc
#' @export
gazer::read_asc
