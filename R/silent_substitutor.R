#' Launch the app
#' @export
silent_substitutor <- function() {
  shiny::shinyApp(ui, server, enableBookmarking = "url")
}


