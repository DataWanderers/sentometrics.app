
#' Shiny application for sentiment computation, aggregation and visualisation
#'
#' @author Samuel Borms, Jeroen Van Pelt
#'
#' @description A Shiny application built on the \pkg{sentometrics} package to allow straightforward
#' textual sentiment computation, aggregation and visualisation. The default loaded .csv file is a
#' subset of the \code{\link[sentometrics]{usnews}} corpus.
#'
#' @details This Shiny application demonstrates mainly the \code{\link[sentometrics]{compute_sentiment}}, 
#' \code{\link[sentometrics]{corpus_summarize}} and \code{\link[sentometrics]{aggregate.sentiment}}
#' functions from the \pkg{sentometrics} package, allowing to gain insights into an uploaded corpus. The corpus should 
#' be uploaded in .csv format. Lexicons and valence shifters can be chosen from the built-in options or 
#' uploaded, and the weighting schemes are those available within \pkg{sentometrics}. All calculated values 
#' and statistics can be downloaded as a .csv file.
#'
#' @export
sento_app <- function() {
  pkgs <- sapply(c("shiny", "shinyWidgets", "shinythemes", "shinycssloaders", "DT"), function(pkg) 
    !requireNamespace(pkg, quietly = TRUE))
  if (any(pkgs)) {
    stop("Make sure to have installed following packages before running the app: ",
         paste0(names(pkgs)[pkgs], collapse = ", "), ".")
  }
  appDir <- system.file("shiny", package = "sentometrics.app")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing the sentometrics.app package.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}

