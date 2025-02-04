# ui.R
library(shiny)

ui <- fluidPage(
  titlePanel("Theophylline PK Simulation"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("CL", "Clearance (CL):", min = 0.01, max = 0.1, value = 0.04, step = 0.005),
      sliderInput("V", "Volume of Distribution (V):", min = 0.1, max = 1, value = 0.47, step = 0.01),
      sliderInput("n", "Sample Size:", min = 10, max = 500, value = 50, step = 10),
      sliderInput("dose", "Dose (mg):", min = 50, max = 500, value = 100, step = 50),
      checkboxInput("includeIIV", "Include Inter-individual Variability (IIV)", value = TRUE),
      actionButton("simulate", "Simulate")
    ),
    mainPanel(
      plotOutput("pkPlot")
    )
  )
)
