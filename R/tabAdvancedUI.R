advanced_row <- function(n) {
  fluidRow(
    column(
      width = 4,
      numericInput(
        paste0("lower", n),
        paste0("Lower limit primary #", n),
        0.5,
        min = 0,
        max = 100000,
        step = 10
      )
    ),
    column(
      width = 4,
      numericInput(
        paste0("upper", n),
        paste0("Upper limit primary #", n),
        100,
        min = 0,
        max = 100000,
        step = 10
      )
    ),
    column(
      width = 4,
      numericInput(
        paste0("step", n),
        paste0("Step primary #", n),
        0.5,
        min = 0,
        max = 100000,
        step = 10
      )
    )
  )
}

tabAdvancedUI <- function() {
  tabPanelBody(
    "advanced_settings",
    h3("Upload spectra"),
    fileInput(
      "upload_spectra",
      NULL,
      buttonLabel = "Upload spectra",
      multiple = FALSE
    ),
    h3("Modify luminance sliders"),
    lapply(1:5, advanced_row),
    hr(),
    bookmarkButton(),
    hr(),
    actionButton("back_from_a_matrix", "Back"),
  )
}
