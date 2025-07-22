require(shiny)
luminance_slider <- function(n, name, ...) {
  sliderInput(
    paste0("luminance_primary_", n),
    name,
    ...
  )
}

contrast_slider <- function(id, label) {
  numericInput(
    inputId = id,
    label = paste(label, "[%]"),
    min = -100,
    max = 100,
    value = 0,
    step = 1
  )
}
