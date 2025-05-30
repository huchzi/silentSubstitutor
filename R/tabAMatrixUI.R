tabAMatrixUI <- function() {
  tabPanelBody("2. A_matrices", fluidRow(
    # withMathJax(htmltools::includeMarkdown(here("markdown", "a_matrix.md"))),
    column(
      width = 5,
      h2("Square A-matrix"),
      shinyBS::bsTooltip(
        "a_matrix",
        "For calculating silent substitution conditions, the A-matrix needs to have a square shape. Thus, you can control only as many photoreceptors as there are primaries.",
        placement = "bottom",
        options = list(container = "body")
      ),
      tableOutput("a_matrix"),
      p(
        "In order to calculate contrasts, the columns are normalized to sum up to 1."
      )
    ),
    column(width = 2),
    column(
      width = 5,
      h2("Full photoreceptor A-Matrix"),
      shinyBS::bsTooltip(
        "a_matrix_b",
        "For estimating the resulting contrast even in the photoreceptors that are not controlled, a non-square A-Matrix is calculated.",
        placement = "bottom"
      ),
      tableOutput("a_matrix_b")
    )),
    hr(),
    actionButton("back_from_a_matrix", "Back")
  )
}
