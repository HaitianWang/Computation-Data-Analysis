library(ggplot2)

original_df <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep=";")
df <- original_df[original_df$fixed.acidity >= 7, c("fixed.acidity", "residual.sugar")]





ggplot(df, aes(x=fixed.acidity, y=residual.sugar)) +
  geom_point() +
  labs(title="Scatter Plot of Fixed Acidity vs Residual Sugar", 
       x="Fixed Acidity", y="Residual Sugar")




# library(shiny)
# 
# # UI
# ui <- fluidPage(
#   titlePanel("Scatter Plot Visualization"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("x_var", "Choose x variable:", choices = names(original_df)),
#       selectInput("y_var", "Choose y variable:", choices = names(original_df))
#     ),  
#     mainPanel(
#       plotOutput("scatterPlot")
#     )
#   )
# )
# 
# # Server
# server <- function(input, output) {
#   
#   output$scatterPlot <- renderPlot({
#     ggplot(original_df, aes_string(x=input$x_var, y=input$y_var)) +
#       geom_point() +
#       labs(title=paste("Scatter Plot of", input$x_var, "vs", input$y_var), 
#            x=input$x_var, y=input$y_var)
#   })
# }
# 
# # Run the Shiny app
# shinyApp(ui = ui, server = server)





















