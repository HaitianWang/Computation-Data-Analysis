library(shiny)
library(shinyWidgets) # for pickerInput
library(ggplot2)      # for ggplot

a <- read.csv("winequality-red.csv", sep=";")

ui <- fluidPage(
  sidebarPanel(
    selectInput("plot_type", "Select plot type:", choices = c("Scatter", "Pairwise", "geom_bin2d")),
    conditionalPanel(
      condition = "input.plot_type == 'Pairwise'",
      pickerInput(
        inputId = "selected_attributes",
        label = "Select attributes:",
        choices = names(a),
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      )
    ),
    conditionalPanel(
      condition = "input.plot_type == 'Scatter'|| input.plot_type == 'geom_bin2d'",
      selectInput("x_attr", "Select x-axis attribute:", names(a)),
      selectInput("y_attr", "Select y-axis attribute:", names(a))
    )
  ),
  mainPanel(
    plotOutput("plot", height = "400px", width = "600px")
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    if (input$plot_type == "Pairwise") {
      selected_attrs <- input$selected_attributes
      if (length(selected_attrs) < 2) {
        return(NULL)
      }
      pairs(a[, selected_attrs])
    } else if(input$plot_type == "Scatter") {
      x_attr <- input$x_attr
      y_attr <- input$y_attr
      ggplot(a, aes_string(x = x_attr, y = y_attr)) +
        geom_point() +
        geom_smooth()
    }else if (input$plot_type == "geom_bin2d"){
      x_attr <- input$x_attr
      y_attr <- input$y_attr
      ggplot(a, aes_string(x=x_attr, y=y_attr) ) +
        geom_bin2d()
    }
  })
}

shinyApp(ui, server)













