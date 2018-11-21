
library(shiny)
library(tidyverse)

final_shiny_data <- read_rds("ps7_app_data")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Midterm Elections 2018 Polling Error"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        #makes radio buttons to determine what the x axis is for the graph
         radioButtons("radio",
                      "Select x-axis:",
                      choices = c("Percent Female", "Percent Independent"),
                      selected = "Percent Independent")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("scatterplot")
      )
   )
)

# Define server logic required to draw a scatterplot
server <- function(input, output) {
   
   output$scatterplot <- renderPlot({
     #if the radio button is switched to percent independent, create one plot
     if(input$radio == "Percent Independent"){
       
     final_shiny_data %>%
       ggplot(aes(x = percent_independant, y = polling_error)) +
         geom_point() +
         geom_smooth(method = lm) +
         ggtitle("There was little to no correlation between percentage of 
respondants who identify asindependent and polling error") +
         labs(x = "Percentage of Independent Voters", y = "Polling Error")
       
     }
     #if the radio button is switched to percent female, create this other plot
     else if(input$radio == "Percent Female") {
       
       final_shiny_data %>%
         ggplot(aes(x = percent_female, y = polling_error)) +
         geom_point() +
         geom_smooth(method = lm) +
         ggtitle("There was little to no correlation between percentage
of female respondants andpolling error") +
         labs(x = "Percentage of Female Voters", y = "Polling Error")
       
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

