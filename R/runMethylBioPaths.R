#' Launch Shiny App for MethylBioPaths
#'
#' A function that launches the Shiny app for MethylBioPaths
#' The purpose of this app is only to illustrate how a Shiny app works. The
#' code has been placed in \code{./inst/shiny-scripts}.
#'
#' @return No return value but open up a shiny page.
#'
#' @examples \dontrun{
#'
#' MethylBioPaths::runMethylBioPaths()
#' }
#'
#' @references
#' Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J,
#' McPherson J, Dipert A, Borges B (2022). _shiny: Web Application Framework
#' for R_. R package version 1.7.3, <https://CRAN.R-project.org/package=shiny>.
#'
#' @export
#' @importFrom shiny runApp

runMethylBioPaths <- function() {
  appDir <- system.file("shiny-scripts",
                        package = "MethylBioPaths")
  shiny::runApp(appDir, display.mode = "normal")
  return()
}
# [END]
