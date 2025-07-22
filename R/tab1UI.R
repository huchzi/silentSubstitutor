tab1UI <- function() {
    tabPanelBody("1. Set primaries", sidebarLayout(
      sidebarPanel(
        p("First, you need to define the characteristics of your apparatus."),
        h3("Select primaries"),
        uiOutput("select_primary"),
        h3("Set luminances [cd/m^2]"),
        br(),
        shinyBS::bsTooltip(
          "luminance_primary_1",
          "The number of primaries and their names are determined by the uploaded spectra file.",
          placement = "right"
        ),
        shinyBS::bsTooltip(
          "luminance_primary_2",
          "The number of primaries and their names are determined by the uploaded spectra file.",
          placement = "right"
        ),
        shinyBS::bsTooltip(
          "luminance_primary_3",
          "The number of primaries and their names are determined by the uploaded spectra file.",
          placement = "right"
        ),
        shinyBS::bsTooltip(
          "luminance_primary_4",
          "The number of primaries and their names are determined by the uploaded spectra file.",
          placement = "right"
        ),
        shinyBS::bsTooltip(
          "luminance_primary_5",
          "The number of primaries and their names are determined by the uploaded spectra file.",
          placement = "right"
        ),
        lapply(1:5, function(n) uiOutput(paste0("luminance_primary_", n))),
        fluidRow(
          column(width = 6, actionButton("a_matrix", "Show A-Matrix")),
          column(width = 6, actionButton("advanced_settings", "Advanced Settings"))
        ),
        hr(),
        actionButton("nxt", "Next step...")
      ),
      mainPanel(# htmltools::includeMarkdown(here("markdown", "spectra.md")),
        h2("LED Power Spectra"),
        plotOutput("led_spectra"),
        hr(),
        h3("CIE coordinates (1964)"),
        p(h4(textOutput("cie_x"))),
        p(h4(textOutput("cie_y"))),
        br(),
        shinyBS::bsTooltip(
          "color_bx",
          "Approximation of the color of the test field in RGB space.",
          placement = "right"
        ),
        uiOutput("color_bx")
      )
    ))
}
