
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

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$scatterplot <- renderPlot({
     if(input$radio == "Percent Independent"){
       
     final_shiny_data %>%
       ggplot(aes(x = percent_independant, y = polling_error)) +
         geom_point() +
         geom_smooth(method = lm) +
         ggtitle("There was little to no correlation between percentage of 
respondants who identify asindependent and polling error") +
         labs(x = "Percentage of Independent Voters", y = "Polling Error")
       
     }
     
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

