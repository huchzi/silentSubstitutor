tab2UI <- function() {
  tabPanelBody(
    "3. Set desired contrasts",
    sidebarLayout(
      sidebarPanel(
        h3("Set photoreceptor contrasts"),
        p("The number and type of photoreceptors you can control are determined by the number of primaries."),
        shinyBS::bsTooltip(
          "lcone",
          "Use negative values for counterphase.",
          placement = "right"
        ),
        shinyBS::bsTooltip(
          "mcone",
          "Use negative values for counterphase.",
          placement = "right"
        ),
        shinyBS::bsTooltip(
          "scone",
          "Use negative values for counterphase.",
          placement = "right"
        ),
        shinyBS::bsTooltip(
          "rod_slider",
          "Use negative values for counterphase.",
          placement = "right"
        ),
        shinyBS::bsTooltip(
          "melanopsin_slider",
          "Use negative values for counterphase.",
          placement = "right"
        ),
        contrast_slider("lcone", "L-cone contrast:"),
        contrast_slider("mcone", "M-cone contrast:"),
        contrast_slider("scone", "S-cone contrast:"),
        uiOutput("rod_slider"),
        uiOutput("melanopsin_slider"),

        shinyBS::bsTooltip(
          "maximize",
          "Press this button to scale all photoreceptor contrasts up (or down) to instrument gamut.",
          placement = "right"
        ),
        hr(),
        h3("Maximize contrasts"),
        actionButton("maximize", "Maximize contrasts"),
        hr(),
        fluidRow(
          column(width = 6, actionButton("setupStimulator", "Back")),
          column(width = 6, actionButton("exploreErrors", "Next"))
        )

      ),

      # Show a plot of the generated distribution
      mainPanel(
        h3("Silent Substitution Conditions"),
        shinyBS::bsTooltip(
          "led_contrasts",
          "MaxLuminance: maximal luminance that this LED will reach with the given contrast.",
          placement = "bottom"
        ),
        tableOutput("led_contrasts"),
        downloadButton("download_excel", label = "Download Excel"),
        downloadButton("download_json", label = "Download JSON"),
        # htmltools::includeMarkdown(here("markdown", "contrasts.md")),
        h3("LED outputs"),
        plotOutput("led_luminances")
      )
    )
  )
}
