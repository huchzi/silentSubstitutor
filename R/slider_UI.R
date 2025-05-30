require(shiny)
luminance_slider <- function(n, name) {
  sliderInput(
    paste0("luminance_primary_", n),
    name,
    min = 0.5,
    max = 100,
    value = 20,
    step = .5
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
