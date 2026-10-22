library(shiny)

ui<- fluidPage(
  titlePanel("Fish Genetics: Response to Selection Calculator"),
  sidebarLayout(sidebarPanel(
    numericInput("population_mean", "Original PopulationMean:",
                                                       value=200,
                                                       min=-1000000,
                                                       max=1000000),
                                          numericInput("selected_mean",
                                                       "Mean of Selection Individuals:",
                                                       value=260,
                                                       min=-1000000,
                                                       max=1000000),
                                          numericInput("heritability",
                                                       "Narrow-sense Heritabiity(h²):",
                                                       value=0.30,
                                                       min=0,
                                                       max=1,
                                                       step=0.01),
                                          actionButton("calculate","Calculate Response")),
                             mainPanel(h3("Results"),
                                       verbatimTextOutput("results")
                                       )
    )
  )
               server<- function(input, output, session) {
                 result<- eventReactive(input$calculate,{
                   validate(need(!is.na(input$heritability) &&
                                   input$heritability >= 0 && input$heritability <= 1,
                                 "Heritability must be between 0 and 1"))
                   S <- input$selected_mean - input$population_mean
                   R <- input$heritability * S
                   list(S=S,
                        R=R,
                        new_mean=input$population_mean + R)
                 })
                 output$results <- renderPrint({
                   x<- result()
                 cat("Selection Differential (S):", x$S, "\n")
                 
                 cat("Response to Selection (R):", x$R, "\n")
                 
                 cat("Expected Next Generation Mean:",
                     x$new_mean, "\n")
                 })
               }
               exists("ui")
               shinyApp(ui = ui, server = server)
               
               
               library(shinythemes)
               
               ui <- fluidPage(
                 theme = shinytheme("flatly"),
                 
                 tags$head(tags$style(HTML("
    body { background-color: #eaf6fb; }
    h2 { color: #0b5d8a; font-weight: bold; }
    .well { background-color: #d6eef8; border: 2px solid #5bb5d9; }
    .btn-primary { background-color: #0b5d8a; border-color: #0b5d8a; }
    #results { background-color: #ffffff; color: #0b5d8a;
               border: 2px solid #5bb5d9; font-size: 16px; }
  "))),
                 
                 titlePanel("Fish Genetics: Response to Selection Calculator"),
                 sidebarLayout(
                   sidebarPanel(
                     numericInput("population_mean", "Original Population Mean:", value = 200),
                     numericInput("selected_mean", "Mean of Selected Individuals:", value = 260),
                     numericInput("heritability", "Narrow-sense Heritability (h2):",
                                  value = 0.30, min = 0, max = 1, step = 0.01),
                     actionButton("calculate", "Calculate Response", class = "btn-primary")
                   ),
                   mainPanel(
                     h3("Results"),
                     verbatimTextOutput("results")
                   )
                 )
               )
               
               server <- function(input, output, session) {
                 result <- eventReactive(input$calculate, {
                   S <- input$selected_mean - input$population_mean
                   R <- input$heritability * S
                   list(S = S, R = R, new_mean = input$population_mean + R)
                 })
                 
                 output$results <- renderPrint({
                   x <- result()
                   cat("Selection Differential (S):", x$S, "\n")
                   cat("Response to Selection (R):", x$R, "\n")
                   cat("Expected Next Generation Mean:", x$new_mean, "\n")
                 })
               }
               
               shinyApp(ui = ui, server = server)
             
              
             