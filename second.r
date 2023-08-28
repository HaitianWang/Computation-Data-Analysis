# Load required libraries
library(shiny)
library(ggplot2)
library(gridExtra)

# Read data
df <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep=";")

# Define the UI
ui <- fluidPage(
  titlePanel("Histogram Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var1", "Choose x for Histogram 1:", choices = names(df)),
      selectInput("x_var2", "Choose x for Histogram 2:", choices = names(df))
    ),  
    mainPanel(
      plotOutput("histogram")
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Function to generate the histogram plots
  generate_histograms <- function(data, x_var) {
    ggplot(data, aes_string(x = x_var)) +
      geom_histogram(bins = 20, color = "white") +
      labs(title = paste("Histogram of", x_var), x = x_var, y = "Frequency")
  }
  
  # Render the side-by-side histograms
  output$histogram <- renderPlot({
    # Create both histograms
    hist1 <- generate_histograms(df, input$x_var1)
    hist2 <- generate_histograms(df, input$x_var2)
    
    # Use grid.arrange to plot them side by side
    grid.arrange(hist1, hist2, ncol=2)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
