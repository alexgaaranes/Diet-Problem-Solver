library(shiny)
library(bslib)
source("nutri_table.R") # Source the nutrition table for constrainst and obj func
source("diet_funcs.R")  # Source the functions that will do the calculations

# Define UI for application that draws the table 
ui <- page_sidebar(
    title = "Diet Problem Solver",
    sidebar = sidebar( # Sidebar for choosing the food
        width = 300,
        checkboxGroupInput(
            "food_indices",
            "Check the food in your diet",
            choiceNames = nutritionTable$Foods,
            choiceValues = 1:(length(nutritionTable$Foods))
        ),
    ),
    "Diet Plan",
    tableOutput("diet_plan"),
)

# Define server logic required to draw the table 
server <- function(input, output) {
    output$diet_plan <- renderTable({
        indices <- as.numeric(input$food_indices)
        getDietPlan(indices)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
