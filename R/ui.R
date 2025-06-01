require(shiny)
require(htmltools)
require(markdown)
require(shinyBS)

my_titlePanel <- function(title, windowTitle = title) {
  css <- paste("background-color: #4CAF50",
               "color : white",
               "padding-left: 15px",
               "margin-left: -15px",
               "margin-right: -15px",
               "border-radius: 5px",
               sep = ";")
  tagList(tags$head(tags$title(windowTitle)), h2(title, style = css))
}

ui <- function(request) {
  fluidPage(
    shinyFeedback::useShinyFeedback(),
    # Application title
    my_titlePanel("SilentSubstiTutor (v0.9.0)"),
    tabsetPanel(
      id = "switcher",
      type = "hidden",
      tab1UI(),
      tabAdvancedUI(),
      tabAMatrixUI(),
      tab2UI(),
      tabPanelBody(
        "4. Estimate errors",
        sidebarLayout(
          sidebarPanel(
            sliderInput(
              "mac_pigment",
              "Macular pigment optical density",
              min = -.3,
              max = .3,
              step = .05,
              value = 0
            ),
            sliderInput(
              "lens_age",
              "Lens age [years]",
              min = 0,
              max = 100,
              step = 5,
              value = 30
            ),
            sliderInput(
              "l_cone_shift",
              "L-cone shift [nm]",
              min = -30,
              max = 30,
              step = 2,
              value = 0
            ),
            sliderInput(
              "m_cone_shift",
              "M-cone shift [nm]",
              min = -30,
              max = 30,
              step = 2,
              value = 0
            ),
            hr(),
            actionButton("createStimuli", "Back")
          ),
          mainPanel(
            fluidRow(
              h3("Modified cone fundamentals"),
              br(),
              plotOutput("cone_fundamentals"),
              hr(),
              column(width = 5,
                     h3("Resulting photoreceptor contrasts"),
                     br(),
                     tableOutput("photoreceptor_contrasts")
              ),
              column(width = 7,
                     h3("LED contribution"),
                     br(),
                     plotOutput("photoreceptors", height = "300px")
                     )
            ),



          )
        )
      )


    )


  )
}
